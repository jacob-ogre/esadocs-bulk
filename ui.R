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
    div(
      class = "outer",
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
        column(2,
          tags$a(
            href="https://esadocs.cci-dev.org/esadocs-bulk/",
            img(src = "ESAdocs_bulk.svg", height = "80px"),
            tabindex = "-1"
          )
        ),
        column(8,
          div(id = "required",
            column(4,
              tipify(
                textInput(
                  inputId = "submitter",
                  label = "Submitter name",
                  width = "110%",
                  value = NA,
                  placeholder = "First & last names"
                ),
                title = "So we know who to address",
                trigger = "focus"
              )
            ),
            column(4,
              tipify(
                textInput(
                  inputId = "submitter_email",
                  label = "Submitter email",
                  width = "110%",
                  value = NA,
                  placeholder = "Double-check the address"
                ),
                title = "So we can get in touch",
                trigger = "focus"
              )
            ),
            column(4,
              tipify(
                selectInput(
                  inputId = "doctype",
                  label = "Document type",
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
                title = "All docs in one upload should be the same type",
                trigger = "focus",
                placement = "top"
              )
            )
          )
        ),
        column(1,
          br(),
          actionButton(
            "help",
            "Help",
            icon = icon("question-circle"))
        ),
        column(1,
          HTML("<img src='DOW_logo_small.png'>")
        )
      ),
      fluidRow(
        hidden(div(
          id = "add_files",
          br(), br(),
          column(2),
          column(8,
            myFileInput(
              "upload_file",
              "Upload ESA-related PDFs of the type above",
              accept = "application/pdf",
              placeholder = "Select PDFs",
              width = "100%",
              multiple = TRUE
            ),
            helpText(
              tags$ul(
                tags$li(
                  "You may select PDFs with or without embedded (selectable) text.
                  PDFs with selectable text will be processed faster - and the
                  text will likely be more accurate - than PDFs without text. Those
                  lacking selectable text will be OCR'd then added to ESAdocs."
                ),
                tags$li(HTML(paste(
                  "Maximum file size is currently 30MB. If you get a 'Maximum
                  file size exceeded' upload warning, one or more files are
                  > 10MB. Please re-browse and exclude large files. To have
                  large files added, please",
                  tags$a(href = "mailto:esa@defenders.org", "contact us.")
                )))
              )
            )
          ),
          column(2)
        ))
      ),
      fluidRow(
        hidden(div(
          id = "submit_btn",
          br(),
          column(12,
            column(2),
            column(6,
              h4("For upload"),
              tableOutput("to_upl_files")
            ),
            column(2,
              br(),
              column(6,
                actionButton(
                  "cancel",
                  label = "Cancel",
                  style = "background-color: #F44336; color: white"
                )
              ),
              column(6,
                actionButton(
                  "submit",
                  label = "Submit",
                  style = "background-color: #304FFE; color: white"
                )
              )
            ),
            column(2)
          )
        ))
      ),
      fluidRow(
        column(3),
        column(6, shinyBS::bsAlert("not_a_pdf")),
        column(3)
      ),
      fluidRow(
        column(3),
        column(6, shinyBS::bsAlert("success_note")),
        column(3)
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
        column(2,
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
              </footer>'
            ),
            br()
          )
        ),
        column(1)
      )
    )
  )
)
