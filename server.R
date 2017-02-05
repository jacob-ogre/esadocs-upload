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

shinyServer(function(input, output, session) {

  file_info <- reactive({
    if(!is.null(input$upload_file)) {
      return(input$upload_file)
    }
    return(NULL)
  })

  observe(print(file_info()))

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

  # Submit for real and remove modal
  observeEvent(input$real_submit, {
    if(input$key_code == Sys.getenv("ESADOC_KEY")) {
      # dest <- gsub(prep_pdfpath(),
      #              pattern = "https://esadocs.cci-dev.org",
      #              replacement = "/home/jacobmalcom/Data")
      pdf_path <- prep_pdfpath()
      dest <- gsub(pdf_path,
                   pattern = "https://esadocs.cci-dev.org/ESAdocs/misc",
                   replacement = "/Users/jacobmalcom/temp")
      cp_res <- file.copy(file_info()$datapath, dest, overwrite = FALSE)
      if(!cp_res) {

      }
      new_data <- submit_data()
      observe(print(paste("new_data$main_res:", new_data$main_res)))

      OKS <- c("noop", "updated", "created")
      observe(print(cp_res))
      if(cp_res & new_data$main_res %in% OKS) {
        shinyBS::createAlert(
          session,
          anchorId = "success_note",
          content = paste("Data for", input$in_title,
                          "added! Record ID = ", new_data$file_id),
          style = "success",
          append = FALSE
        )
        log_changes()
      } else {
        shinyBS::createAlert(
          session,
          anchorId = "success_note",
          content = paste("Data for", input$in_title, "failed!"),
          style = "error",
          append = FALSE
        )
      }
      fields <- c("doc_id", "in_title", "in_date", "in_npages",
                  "in_fed", "in_actcode", "in_frpage", "in_chstatus",
                  "in_misc_doctype", "in_species", "in_geo", "in_tags")
      res <- lapply(fields, updateTextInput, session = session, value = "")

    } else if(input$key_code == "") {
      showModal(modalDialog(
        title = HTML("<h3>Key required</h3>"),
        HTML("<p style='font-size:large'>Enter the current key found in the
             shared GDrive folder.</p>"),
        size = "m",
        footer = actionButton(
          "cancel_submit",
          label = "OK",
          style = "background-color: #F44336; color: white"
        )
        ))
    }
  })

  prep_pdfpath <- function() {
    fname <- file_info()$name
    fname <- gsub(fname, pattern = " ", replacement = "_")
    fname <- gsub(x = fname, pattern = "&", replacement = "and")
    fname <- gsub(x = fname, pattern = "\\(|\\)|\'|\"", replacement = "")
    fname <- gsub(x = fname, pattern = "\\,", replacement = "")
    dest <- file.path("https://esadocs.cci-dev.org/ESAdocs",
                      input$in_doctype,
                      fname)
    return(dest)
  }

  submit_data <- function() {
    file_id <- rand_str()
    prep_agency <- ifelse(input$in_fed != "",
                          strsplit(input$in_fed, split = "; "),
                          NA)
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
          title = input$in_title,
          date = ifelse(input$in_date != "NA", input$in_date, NA),
          pdf_path = prep_pdfpath(),
          n_pages = ifelse(input$in_npages != "", input$in_npages, NA),
          fr_citation_page = ifelse(input$in_frpage != "", input$in_frpage, NA),
          federal_agency = ifelse(input$in_fed != "", prep_agency, NA),
          activity_code = ifelse(input$in_actcode != "", input$in_actcode, NA),
          ch_status = ifelse(input$in_chstatus != "", input$in_chstatus, NA),
          raw_txt = paste(pdf_text(file_info()$datapath), collapse = " "),
          species = ifelse(!is.na(prep_species), prep_species, NA),
          geo = ifelse(!is.na(prep_geo), prep_geo, NA),
          tags = ifelse(!is.na(prep_tags), prep_tags, NA)
        )
      )
    )
    return(list(file_id = file_id,
                main_res = result$result,
                fpath = prep_pdfpath()))
  }


})

