# BSD_2_clause

options(shiny.maxRequestSize=10*1024^2)

rand_str <- function(len=30) {
  str <- paste(
    sample(
      c(rep(0:9,each=5),
        LETTERS,
        letters),
      len,
      replace=TRUE),
  collapse='')
  return(str)
}

shinyServer(function(input, output, session) {

  file_info <- reactive({
    if(!is.null(input$upload_file)) {
      return(input$upload_file)
    }
    return(NULL)
  })

  output$testing_msg <- renderText({
    paste("input$upload_file", input$upload_file)
    # paste("is_pdf:", is_pdf(), "with_txt:", with_txt(), "tempdir:", tempdir())
  })

  # TEST ELEMENTS
  is_pdf <- reactive({
    if(!is.null(file_info())) {
      pdftest <- try(pdf_info(file_info()$datapath))
      if(class(pdftest) != "try-error") {
        return(TRUE)
      }
      # file.remove(file_info()$datapath)
      show("not_a_pdf")
      Sys.sleep(3)
      hide("not_a_pdf")
      reset("upload_file")
      return(FALSE)
    }
    return(FALSE)
  })

  with_txt <- reactive({
    if(!is.null(file_info())) {
      with_text <- try(pdf_text(file_info()$datapath), silent = TRUE)
      if(class(with_text) == "try-error") {
        # show()
        # file.remove(file_info()$datapath)
        # reset("upload_file")
        return(FALSE)
      } else {
        # A hack to try to detect whether there is a text layer; some docs
        # may be OK but may fail this check (e.g., a map with just a few words)
        tokenized <- unlist(str_split(paste(with_text, collapse = " "), " "))
        tokenized <- tokenized[tokenized != ""]
        if(length(tokenized) > 10) {
          return(TRUE)
        }
        return(FALSE)
      }
    }
    return(FALSE)
  })

  title_ok <- reactive({
    if(nchar(input$in_title) > 8 & nchar(input$in_title) < 256) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  date_ok <- reactive({
    is_date <- try(as.Date(input$in_date), silent = TRUE)
    if(class(is_date) != "try-error") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  type_ok <- reactive({
    types = c("candidate", "conserv_agmt", "consultation",
              "federal_register", "five_year_review", "misc",
              "policy", "recovery_plan")
    if(input$in_doctype %in% types) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  key_ok <- reactive({
    if(input$key_code == Sys.getenv("ESADOC_KEY")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  # BATCH TESTS
  test_pdf_text <- function() {
    if(is_pdf() & with_txt()) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  test_required <- function() {
    if(title_ok() & date_ok() & type_ok() & key_ok()) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  observe({
    if(test_pdf_text()) {
      hide("spacer", anim = TRUE, animType = "slide", time = 1)
      hide("pad_foot", anim = FALSE)
      show("top_hr", anim = FALSE)
      show("req_head", anim = FALSE)
      show("pad_foot_2", anim = TRUE, animType = "fade", time = 1)
      show("spacer_2", anim = TRUE, animType = "slide", time = 1)
      show("req_1", anim = TRUE, animType = "fade", time = 1)
      show("req_2", anim = TRUE, animType = "fade", time = 1)
    }
  })

  observe({
    if(test_required()) {
      hide("spacer_2", anim = TRUE, animType = "slide", time = 1)
      hide("req_head", anim = FALSE)
      show("opt_head", anim = FALSE)
      show("optional_1", anim = TRUE, animType = "slide", time = 1)
      show("optional_2", anim = TRUE, animType = "slide", time = 1)
      show("optional_3", anim = TRUE, animType = "slide", time = 1)
      show("submit_btn", anim = TRUE, animType = "fade", time = 1)
    }
  })

  shinyBS::createAlert(
    session,
    anchorId = "more_data",
    content = paste(
      "<p style='font-weight:bold'>",
      "Please include as much data as possible. We will process the",
      "document with various programs to extract additional structured",
      "data, but manually entered data can help humans search for",
      "documents using human-recognized patterns.</b>"
    ),
    style = "info",
    dismiss = TRUE,
    append = FALSE
  )

  # CLEAR INPUT FIELDS
  clear_most_fields <- function() {
    fields <- c("doc_id", "in_title", "in_date", "in_npages",
                "in_fed", "in_actcode", "in_frpage", "in_chstatus",
                "in_species", "in_geo", "in_tags", "in_orig_url")
    res <- lapply(fields, updateTextInput, session = session, value = "")
    updateSelectInput(session, "in_doctype", selected = "not_selected")
    reset("upload_file")
  }

  clear_all_fields <- function() {
    clear_most_fields()
    updateTextInput(session, "key_code", value = "")
  }

  # Submit modal
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = HTML("<h3>Submit</h3>"),
      HTML("<h4>Submit the document and this data?</h4>"),
      HTML(
        paste(
          c("<div style='font-size:large; padding-left:15px'>",
            paste("<b>Title</b>:", input$in_title),
            paste("<b>Date</b>:", input$in_date),
            paste("<b>Type</b>:", input$in_doctype),
            paste("<b># pages</b>:", input$in_npages),
            paste("<b>FR citation</b>:", input$in_frpage),
            paste("<b>Federal agency(ies)</b>:", input$in_fed),
            paste("<b>Activity code</b>:", input$in_actcode),
            paste("<b>CH status</b>:", input$in_chstatus),
            paste("<b>Misc. doc type</b>:", input$in_misc_doctype),
            paste("<b>Species</b>:", input$in_species),
            paste("<b>Geo-tags</b>:", input$in_geo),
            paste("<b>Tags</b>:", input$in_tags),
            paste("<b>Original URL</b>:", input$in_orig_url),
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
  })

  copy_upload_file <- function() {
    pdf_path <- prep_pdfpath()
    dest <- gsub(pdf_path,
                 pattern = "https://esadocs.cci-dev.org",
                 replacement = "/home/jacobmalcom/Data")
    cp_res <- file.copy(file_info()$datapath, dest, overwrite = FALSE)
    if(!cp_res) {
      dest <- paste0(dest, ".", rand_str(5), ".pdf")
      pdf_path <- gsub(dest,
                       pattern = "/home/jacobmalcom/Data",
                       replacement = "https://esadocs.cci-dev.org/ESAdocs")
      cp_res <- file.copy(file_info()$datapath, dest, overwrite = FALSE)
    }
    return(list(cp_res = cp_res, pdf_path = pdf_path))
  }

  # CLEAN UP FILE NAMES, WHICH WILL BE UGLY
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

  # Submit for real and remove modal
  observeEvent(input$real_submit, {
    if(key_ok()) {
      copy_res <- copy_upload_file()
      observe({print(input$in_orig_url)})
      if(!copy_res$cp_res) {
        showModal(modalDialog(
          title = "Error",
          p(style = "font-size: large",
            icon("ban", "fa-3x"),
            "There was an error with the file. You may try re-naming it,
            but it probably won't help. To try another file, `Cancel` from
            the main page."
          ),
          span(style = "text-align:center",
               a(href="mailto:esa@defenders.org", "Contact us")),
          size = "s",
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      } else {
        new_data <- submit_data(copy_res$pdf_path)
        OKS <- c("noop", "created")
        if(new_data$main_res %in% OKS) {
          shinyBS::createAlert(
            session,
            anchorId = "success_note",
            content = paste("Data for <b>", input$in_title,
                            "</b> added! <br> Record ID = ", new_data$file_id),
            style = "success",
            append = FALSE
          )
          removeModal()
          removeClass(id = "key_code", "attention")
          clear_most_fields()
          reset("upload_file")
        } else {
          file.remove(file_info()$datapath)
          shinyBS::createAlert(
            session,
            anchorId = "success_note",
            content = paste("Data input for", input$in_title, "failed!<br>",
                            "Please check the inputs and try again. If the",
                            "error persists,",
                            "<a href='mailto:esa@defenders.org'>contact us</a>."),
            style = "error",
            append = FALSE
          )
        }
      }
    } else {
      addClass(id = "key_code", "attention")
    }
  })

  submit_data <- function(pdf_path) {
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
    prep_md5 <- digest(file_info()$datapath, "md5", file = TRUE)

    result <- docs_create(
      index = "esadocs",
      type = input$in_doctype,
      id = file_id,
      body = list(
        title = input$in_title,
        date = ifelse(input$in_date != "NA", input$in_date, NA),
        type = input$in_doctype,
        pdf_path = pdf_path,
        pdf_md5 = prep_md5,
        link = ifelse(input$in_orig_url != "", input$in_orig_url, NA),
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
    return(list(file_id = file_id,
                main_res = result$result,
                fpath = prep_pdfpath()))
  }

  observeEvent(input$cancel, {
    showModal(modalDialog(
      title = "Cancel",
      "Cancel this upload?",
      size = "s",
      footer = tagList(
        actionButton(
          "cancel_cancel",
          label = "No",
          style = "background-color: #F44336; color: white"
        ),
        actionButton(
          "real_cancel",
          label = "Yes",
          style = "background-color: #304FFE; color: white"
        )
      )
    ))
  })

  ######### CANCELS #########
  # Cancel for real and remove cancel modal
  observeEvent(input$real_cancel, {
    file.remove(file_info()$datapath)
    reset("upload_file")
    clear_most_fields()
    removeModal()
  })

  # Cancel the cancel
  observeEvent(input$cancel_cancel, {
    removeModal()
  })

  observeEvent(input$cancel_submit, {
    removeModal()
  })

  # Help modal
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Add a new ESAdoc (+ data)",
      div(style='font-size:larger',
        hr(),
        h4("Caution: With great power comes great responsibility"),
        p("This app is a convenience tool to add new documents and data to the
          ESAdocs database. For now, there is no round of review...once
          a document is submitted, the elasticsearch database is changed!
          There's no need to worry about this - we can use `ESAdocs edit`
          to make revisions - but be aware."),
        hr(),
        div(style="color:#000000;background-color:#cccccc; padding:10px",
          h4("Uploading docs and data"),
          tags$ol(
            tags$li("Click the `BROWSE` button to select a file to upload."),
            tags$li("Fill in the required data (first row)."),
            tags$li("Add the current key (alpha-numeric text)."),
            tags$li("Submit to upload the file and make it searchable (or Cancel).")
          ),
          HTML("<img src='simple_upload.png' width='100%'>")
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })


})

