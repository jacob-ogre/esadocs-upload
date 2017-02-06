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
              column(6,
                fileInput(
                  "upload_file",
                  "Upload PDF",
                  width = "100%"
                )
              ),
              column(3,
                helpText(
                  HTML(
                    "The PDF must have embedded text. If an image-only PDF,
                    please use optical character recognition (OCR) software to
                    generate a text layer.")
                )
              )
            ),
            fluidRow(hr(class="style-four")),
            fluidRow(
              column(2),
              column(8,
                shinyBS::bsAlert("success_note")
              ),
              column(2)
            ),
            fluidRow(
              column(2),
              column(8,
                shinyBS::bsAlert("more_data")
              ),
              column(2)
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
                  inputId = "in_orig_url",
                  label = "Original online version",
                  width = "110%",
                  value = NA,
                  placeholder = "URL (alpha-numeric)"
                )
              )
            ),
            fluidRow(
              column(
                4,
                textInput(
                  inputId = "key_code",
                  label = "Current key*",
                  width = "110%",
                  value = NA,
                  placeholder = "alpha-numeric"
                )
              ),
              column(3,
                p(style = "font-size:larger; color:#4d4d4d",
                  "* = required field")
              ),
              column(3),
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
            ),
            fluidRow(
              column(1),
              column(
                10,
                div(
                  style = "text-align:center",
                  hr(),
                  HTML('<footer>
                       <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
                       <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>
                       <br />
                       This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" rel="dct:type">work</span>
                       by <a xmlns:cc="http://creativecommons.org/ns" href="http://defenders.org" property="cc:attributionName" rel="cc:attributionURL">Defenders of Wildlife</a>
                       is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
                       </footer>'),
                  hr(),
                  br()
                )
              ),
              column(1)
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
