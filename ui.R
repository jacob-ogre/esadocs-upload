# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(
  fluidPage(
    theme = shinythemes::shinytheme("paper"),
    div(class = "outer",
        shinyjs::useShinyjs(),
        # tags$style(appCSS),
        tags$head(
          HTML("<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400'
               rel='stylesheet' type='text/css'>"),
          HTML('<link rel="icon" type="image/png" href="favicon-32x32.png" sizes="32x32" />'),
          tags$style(HTML(readLines("www/custom-style.css")))
          ),

        br(),
        fluidRow(
          column(
            1,
            tags$a(href="https://esadocs.cci-dev.org/esadocs-upload/",
                   img(src = "ESAdocs_upload.svg",
                       height = "80px")
            )
          ),
          column(
            10,
            fluidRow(
              column(2),
              column(4,
                fileInput(
                  "upload_file",
                  "Upload PDF",
                  width = "400px"
                )
              ),
              column(4,
                helpText(
                  HTML(
                    "The PDF must have embedded text. If an 'image' PDF, please
                    use optical character recognition (OCR) software or
                    <a href='https://cci-dev.org/shiny/closed/PDF_OCR/'
                    target='_blank'>try Defenders' OCR app</a>.")
                )
              )
            ),
            fluidRow(hr(class="style-four")),
            fluidRow(
              shinyBS::bsAlert("success_note")
            ),
            fluidRow(
              column(8,
                helpText(
                  "Please include as much data as possible. We will process the
                  document with various programs to extract additional structured
                  data, but manually entered data can help humans search for
                  documents using human-recognized patterns."
                )
              )
            ),
            fluidRow(
              br(),
              column(
                4,
                textInput(
                  inputId = "in_title",
                  label = "Title*",
                  width = "110%",
                  value = NA,
                  placeholder = "Document title (16-256 characters)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "in_date",
                  label = "Date*",
                  width = "110%",
                  value = NA,
                  placeholder = "Doc. date (YYYY-MM-DD)"
                )
              ),
              column(
                4,
                selectInput(
                  inputId = "in_doctype",
                  label = "Document Type*",
                  choices = list(
                    "Select one" = "not_selected",
                    "candidate doc" = "candidate",
                    "conserv. agreement" = "conserv_agmt",
                    "consultation" = "consultation",
                    "Federal Register" = "federal_register",
                    "five-year review" = "five_year_review",
                    "miscellaneous" = "misc",
                    "policy" = "policy",
                    "recovery plan" = "recovery_plan"),
                  width = "110%"
                )
              )
            ),
            fluidRow(
              br(),
              column(
                4,
                textInput(
                  inputId = "in_frpage",
                  label = "Fed. Reg. citation",
                  width = "110%",
                  value = NA,
                  placeholder = "(text, FR citation)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "in_fed",
                  label = "Federal agency(ies)",
                  width = "110%",
                  value = NA,
                  placeholder = "(text, semicolon sep. list)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "in_actcode",
                  label = "Activity Code",
                  width = "110%",
                  placeholder = "(text, alpha-numeric)"
                )
              )
            ),
            fluidRow(
              br(),
              column(
                4,
                textInput(
                  inputId = "in_chstatus",
                  label = "Crit. Hab. status",
                  width = "110%",
                  placeholder = "(text)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "in_npages",
                  label = "# pages",
                  width = "110%",
                  value = NA,
                  placeholder = "(numeric)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "in_species",
                  label = "Species",
                  width = "110%",
                  value = NA,
                  placeholder = "Spp. in doc. (text, semicolon sep. list)"
                )
              )
            ),
            fluidRow(
              br(),
              column(
                4,
                textInput(
                  inputId = "in_geo",
                  label = "Geographical tags",
                  width = "110%",
                  value = NA,
                  placeholder = "Places in doc (text, semicolon sep. list)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "in_tags",
                  label = "Tags",
                  width = "110%",
                  value = NA,
                  placeholder = "Keywords (text, semicolon sep. list)"
                )
              ),
              column(
                4,
                textInput(
                  inputId = "key_code",
                  label = "Current key",
                  width = "110%",
                  value = NA,
                  placeholder = "alpha-numeric"
                )
              ),
              column(3,
                p(style = "font-size:larger; color:#4d4d4d",
                  "* = required field")
              ),
              column(7),
              column(
                1,
                actionButton(
                  "cancel",
                  label = "Cancel",
                  style = "background-color: #F44336; color: white"
                )
              ),
              column(
                1,
                actionButton(
                  "submit",
                  label = "Submit",
                  style = "background-color: #304FFE; color: white"
                )
              )
            ),
            fluidRow(
              br(), br()
            )
          ),
          column(
            1,
            br(),
            actionButton(
              "help",
              "Help",
              icon = icon("question-circle"))
          )
        )
      )
  )
)

body
