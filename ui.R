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
          div(
            id = "spacer",
            br(), br(), br(), br(), br(), br(),
            br(), br(), br()
          ),
          hidden(
            div(
              id = "spacer_2",
              br(), br(), br()
            )
          )
        ),
        fluidRow(
          column(1,
            tags$a(
              href="https://esadocs.cci-dev.org/esadocs-upload/",
              img(src = "ESAdocs_upload.svg", height = "80px")
            )
          ),
          column(10,
            fluidRow(
              column(2),
              column(8,
                myFileInput(
                  "upload_file",
                  "Upload an ESA-related PDF",
                  accept = "application/pdf",
                  placeholder = "Select a PDF with embedded text",
                  width = "100%"
                )
              )
            ),
            # fluidRow(textOutput("testing_msg")),
            fluidRow(
              column(2),
              column(8,
                bsAlert("not_a_pdf_text")
              ),
              column(2)
            ),
            fluidRow(hidden(div(id = "top_hr", hr(class="style-four")))),
            fluidRow(
              column(2),
              column(8,
                shinyBS::bsAlert("success_note")
              ),
              column(2)
            ),
            fluidRow(
              column(3,
                hidden(div(
                  id = "req_head",
                  h5(class = "req_col", "Add required data")
                )),
                hidden(div(
                  id = "opt_head",
                  h5(class = "opt_col", "Add additional data")
                ))
              )
            ),
            fluidRow(
              hidden(div(id = "req_1",
                br(),
                column(4,
                  tipify(
                    textInput(
                      inputId = "in_title",
                      label = "Title*",
                      width = "110%",
                      value = NA,
                      placeholder = "Document title (8-256 characters)"
                    ),
                    title = "More informative titles are better!",
                    trigger = "focus"
                  )
                ),
                column(4,
                  tipify(
                    textInput(
                      inputId = "in_date",
                      label = "Date*",
                      width = "110%",
                      value = NA,
                      placeholder = "Doc. date (YYYY-MM-DD)"
                    ),
                    title = "Use the international standard",
                    trigger = "focus"
                  )
                ),
                column(4,
                  tipify(
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
                    ),
                    title = "These are the only options for now",
                    trigger = "focus",
                    placement = "top"
                  )
                )
              ))
            ),
            fluidRow(
              hidden(div(id = "optional_1",
                br(),
                column(4,
                  textInput(
                    inputId = "in_frpage",
                    label = "Fed. Reg. citation",
                    width = "110%",
                    value = NA,
                    placeholder = "(text, FR citation)"
                  )
                ),
                column(4,
                  textInput(
                    inputId = "in_fed",
                    label = "Federal agency(ies)",
                    width = "110%",
                    value = NA,
                    placeholder = "(text, semicolon sep. list)"
                  )
                ),
                column(4,
                  textInput(
                    inputId = "in_actcode",
                    label = "Activity Code",
                    width = "110%",
                    placeholder = "(text, alpha-numeric)"
                  )
                )
              ))
            ),
            fluidRow(
              hidden(div(id = "optional_2",
                br(),
                column(4,
                  textInput(
                    inputId = "in_chstatus",
                    label = "Crit. Hab. status",
                    width = "110%",
                    placeholder = "(text)"
                  )
                ),
                column(4,
                  textInput(
                    inputId = "in_npages",
                    label = "# pages",
                    width = "110%",
                    value = NA,
                    placeholder = "(numeric)"
                  )
                ),
                column(4,
                  textInput(
                    inputId = "in_species",
                    label = "Species",
                    width = "110%",
                    value = NA,
                    placeholder = "Spp. in doc. (text, semicolon sep. list)"
                  )
                )
              ))
            ),
            fluidRow(
              hidden(div(id = "optional_3",
                br(),
                column(4,
                  textInput(
                    inputId = "in_geo",
                    label = "Geographical tags",
                    width = "110%",
                    value = NA,
                    placeholder = "Places in doc (text, semicolon sep. list)"
                  )
                ),
                column(4,
                  textInput(
                    inputId = "in_tags",
                    label = "Tags",
                    width = "110%",
                    value = NA,
                    placeholder = "Keywords (text, semicolon sep. list)"
                  )
                ),
                column(4,
                  textInput(
                    inputId = "in_orig_url",
                    label = "Original online version",
                    width = "110%",
                    value = NA,
                    placeholder = "URL (alpha-numeric)"
                  )
                )
              ))
            ),
            fluidRow(
              hidden(div(id = "req_2",
                br(),
                column(4,
                  tipify(
                    textInput(
                      inputId = "key_code",
                      label = "Current key*",
                      width = "110%",
                      value = NA,
                      placeholder = "alpha-numeric"
                    ),
                    title = "From the shared OneDrive file",
                    trigger = "focus"
                  )
                ),
                column(3,
                  p(style = "font-size:larger; color:#4d4d4d",
                    "* = required field")
                ),
                column(3),
                column(1,
                  actionButton(
                    "cancel",
                    label = "Cancel",
                    style = "background-color: #F44336; color: white"
                  )
                ),
                hidden(div(id = "submit_btn",
                  column(1,
                    actionButton(
                      "submit",
                      label = "Submit",
                      style = "background-color: #304FFE; color: white"
                    )
                  )
                ))
              ))
            ),
            fluidRow(
              div(
                id = "pad_foot",
                br(), br(), br(), br(), br(), br(),
                br(), br(), br(), br(), br(), br()
              ),
              hidden(div(
                id = "pad_foot_2",
                br(), br(), br(),
                br(), br(), br()
              ))
            ),
            fluidRow(
              column(5),
              column(1,
                     HTML('
          <a href="http://defenders.org">
          <img style="vertical-align:middle" src="DOW_logo_small.png" height="60"></a>
        ')
              ),
              column(1,
                     HTML('
          <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
          <img alt="Creative Commons License" style="border-width:0;padding-top:15px" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>
        ')
              ),
              column(5)
            ),
            fluidRow(
              column(1),
              column(10,
                     div(
                       style = "text-align:center",
                       HTML('<footer>
            <br />
            <p>This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" rel="dct:type">work</span>
            by <a xmlns:cc="http://creativecommons.org/ns" href="http://defenders.org" property="cc:attributionName" rel="cc:attributionURL">Defenders of Wildlife</a>
            is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.</p>
            <br />
          </footer>'),
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
