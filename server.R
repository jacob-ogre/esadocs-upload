# BSD_2_clause

rand_str <- function(len=30) {
  return(paste(sample(c(rep(0:9,each=5),
                        LETTERS,
                        letters),
                      len,
                      replace=TRUE),
               collapse='')
  )
}

shinyServer(function(input, output) {

  file_info <- reactive({
    if(!is.null(input$upload_file)) {
      return(input$upload_file)
    }
    return(NULL)
  })

  observe(print(file_info()))

  get_raw_text <- function(f) {
    with_text <- try(pdf_text(f), silent = TRUE)
    if(class(with_text) == "try-error") {
      return(warning("No text layer or not a PDF."))
    } else {
      return(paste(with_text, collapse = " "))
    }
  }

  # Check that required data is OK
  validate_data <- function() {
    types = c("candidate", "conserv_agmt", "consultation",
              "federal_register", "five_year_review", "misc",
              "policy", "recovery_plan")
    if(!is.null(file_info) &
       input$in_title != "" &
       input$in_date != "" &
       input$in_doctype != "not_selected") {
      is_pdf <- try(pdf_info(file_info()$datapath), silent = TRUE)
      if(class(is_pdf) == "try-error") {
        return(FALSE)
      }
      with_text <- try(pdf_text(file_info()$datapath), silent = TRUE)
      if(class(with_text) == "try-error") {
        return(FALSE)
      } else {
        # A hack to try to detect whether there is a text layer; some docs
        # may be OK but may fail this check (e.g., a map with just a few words)
        tokenized <- unlist(str_split(paste(with_text, collapse = " "), " "))
        tokenized <- tokenized[tokenized != ""]
        if(length(tokenized) < 10) {
          return(FALSE)
        }
      }
      if(nchar(input$in_title) < 16 | nchar(input$in_title) > 256) {
        return(FALSE)
      }
      is_date <- try(as.Date(input$in_date))
      if(class(is_date) == "try-error") {
        return(FALSE)
      }
      if(!(input$in_doctype %in% types)) {
        return(FALSE)
      }
      return(TRUE)
    }
    return(FALSE)
  }

  submit_changes <- function() {
    file_id <- rand_str()
    prep_species <- ifelse(input$in_species != "",
                           strsplit(input$in_species, split = "; "),
                           NA)
    prep_geo <- ifelse(!is.na(input$in_geo) & input$in_geo != "",
                       strsplit(input$in_geo, split = "; "),
                       NA)
    prep_tags <- ifelse(input$in_tags != "",
                        strsplit(input$in_tags, split = "; "),
                        NA)

    result <- docs_create(
      index = "esadocs",
      type = input$in_doctype,
      id = file_id,
      body = list(
        doc = list(
          title = get_value(input$in_title, "title"),
          n_pages = ifelse(input$in_npages != "" | is.null(got_dat()$n_pages),
                           as.numeric(input$in_npages),
                           as.numeric(got_dat()$n_pages)),
          fr_citation_page = ifelse(input$in_frpage != "" |
                                      is.null(got_dat()$fr_citation_page),
                                    input$in_frpage,
                                    got_dat()$fr_citation_page),
          federal_agency = ifelse(input$in_fed != "" |
                                    is.null(got_dat()$federal_agency),
                                  input$in_fed,
                                  got_dat()$federal_agency),
          activity_code = ifelse(input$in_actcode != "" |
                                   is.null(got_dat()$activity_code),
                                 input$in_actcode,
                                 got_dat()$activity_code),
          ch_status = ifelse(!is.na(input$in_chstatus) |
                               is.null(got_dat()$ch_status),
                             input$in_chstatus,
                             got_dat()$ch_status),
          doc_type = ifelse(input$in_misc_doctype != "" |
                              is.null(got_dat()$doc_type),
                            input$in_misc_doctype,
                            got_dat()$doc_type),
          species = ifelse(!is.na(prep_species) | is.null(got_dat()$species),
                           prep_species,
                           got_dat()$species),
          geo = ifelse(!is.na(prep_geo) | is.null(got_dat()$geo),
                       prep_geo,
                       got_dat()$geo),
          tags = ifelse(!is.na(prep_tags) | is.null(got_dat()$tags),
                        prep_tags,
                        got_dat()$tags)
        )
      )
    )
    dater <- NA
    if(input$in_date != "") {
      if(input$in_date == "NA") {
        to_add_date <- NA
      } else {
        isdate <- try(as.Date(input$in_date))
        if(class(isdate) != "try-error") {
          to_add_date <- input$in_date
        } else {
          stop(paste(input$in_date, "is not a date."))
        }
      }
      dater <- docs_update(
        index = "esadocs",
        type = got_doc()$`_type`,
        id = cur_doc(),
        body = list( doc = list( date = to_add_date ) )
      )$result
    }
    return(list(main_res = result$result, date_res = dater))
  }

  # Submit modal
  observeEvent(input$submit, {
    if(validate_data()) {
      showModal(modalDialog(
        title = HTML("<h3>Submit</h3>"),
        HTML("<h4>Submit this document and data?</h4>"),
        HTML(
          paste(
            c("<div style='font-size:large; padding-left:15px'>",
              paste("<b>Title</b>:", input$in_title),
              paste("<b>Date</b>:", input$in_date),
              paste("<b>Type</b>:", input$in_doctype),
              paste("<b># pages</b>:", input$in_npages),
              paste("<b>FR citation</b>:", input$in_frpage),
              paste("<b>Federal agency</b>:", input$in_fed),
              paste("<b>Activity code</b>:", input$in_actcode),
              paste("<b>CH status</b>:", input$in_chstatus),
              paste("<b>Misc. doc type</b>:", input$in_misc_doctype),
              paste("<b>Species</b>:", input$in_species),
              paste("<b>Geo-tags</b>:", input$in_geo),
              paste("<b>Tags</b>:", input$in_tags),
              "</div>"
            ),
            collapse = "<br>")
        ),
        size = "m",
        footer = tagList(
          actionButton(
            "cancel_submit",
            label = "No",
            style = "background-color: #F44336; color: white"),
          actionButton(
            "real_submit",
            label = "Yes",
            style = "background-color: #304FFE; color: white")
        )
      ))
    } else {
      showModal(modalDialog(
        title = HTML("<h3>Error</h3>"),
        div(
          style = "font-size:larger; font-weight:500;",
          p("There was an error with the file or data you are trying to submit.
             It may be:"),
          tags$ul(
            span(style="font-style:italic; font-weight:bold;",
                 "The file is not a PDF."),
            "We only accept PDFs at this time."
          ),
          tags$ul(
            span(style="font-style:italic; font-weight:bold;",
                 "The PDF doesn't have embedded text."),
            "We only accept PDFs with selectable at this time."
          ),
          tags$ul(
            span(style="font-style:italic; font-weight:bold;",
                 "The title is too short or too long."),
            "Title lengths should be 16-256 characters."
          ),
          tags$ul(
            span(style="font-style:italic; font-weight:bold;",
                 "The date is not properly formatted."),
            "Please use the international standard, YYYY-MM-DD."
          ),
          tags$ul(
            span(style="font-style:italic; font-weight:bold;",
                 "The document type is not selected."),
            "A non-'Select one' document type must be selected."
          ),
          p("Please correct the error and re-try your submission.")
        ),
        size = "m",
        footer = actionButton(
          "cancel_submit",
          label = "OK",
          style = "background-color: #F44336; color: white"
        )
      ))
    }
  })

  observeEvent(input$cancel_submit, {
    removeModal()
  })



})
