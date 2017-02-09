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

  # TEST ELEMENTS
  is_pdf <- reactive({
    if(!is.null(file_info())) {
      pdftests <- sapply(
        file_info()$datapath, function(x) try(pdf_info(x), silent = TRUE)
      )
      if(any(class(pdftests) == "try-error")) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    return(FALSE)
  })

  submitter_ok <- reactive({
    if(nchar(input$submitter) > 4 & nchar(input$submitter) < 256) {
      if(grepl(input$submitter, pattern = "[[:print:]] [[:print:]]")) {
        return(TRUE)
      }
      return(FALSE)
    } else {
      return(FALSE)
    }
  })

  submitter_email_ok <- reactive({
    if(nchar(input$submitter_email) > 4 & nchar(input$submitter_email) < 256) {
      if(grepl(input$submitter_email, pattern = "^[^@]+@[^@]+\\.[^@]+$")) {
        return(TRUE)
      }
      return(FALSE)
    }
    return(FALSE)
  })

  type_ok <- reactive({
    types = c("candidate", "conserv_agmt", "consultation",
              "federal_register", "five_year_review", "misc",
              "policy", "recovery_plan")
    if(input$doctype %in% types) {
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

  observe({
    if(submitter_ok() & submitter_email_ok() & type_ok()) {
      hide("spacer", anim = TRUE, animType = "slide")
      hide("pad_foot", anim = TRUE, animType = "slide")
      show("spacer_2", anim = TRUE, animType = "slide")
      show("add_files", anim = TRUE, animType = "slide")
      show("pad_foot_2", anim = TRUE, animType = "slide")
    }
  })

  observe({
    if(!is.null(file_info())) {
      if(is_pdf()) {
        show("submit_btn", anim = TRUE, animType = "fade")
        hide("pad_foot_2")
        output$to_upl_files <- renderTable(
          dplyr::select(file_info(), name, size),
          striped = TRUE,
          hover = FALSE,
          bordered = FALSE,
          width = "100%"
        )
      } else {
        createAlert(
          session,
          "not_a_pdf",
          title = "Not a PDF",
          content = paste("One or more files are not PDF or may be damaged.
                          Please check all files and try again."),
          style = "error",
          dismiss = TRUE
        )
        hide("submit_btn", anim = TRUE, animType = "fade")
        file.remove(file_info()$datapath)
        reset("upload_file")
      }
    } else {
      hide("submit_btn", anim = TRUE, animType = "fade")
    }
  })

  ###########################################################################
  ## SUBMITS
  ###########################################################################
  observeEvent(input$submit, {
    if(key_ok()) {
      showModal(modalDialog(
        title = HTML("<h3>Submit</h3>"),
        HTML(paste("<h4>Submit these",
                   dim(file_info())[1],
                   "PDFs to ESAdocs?</h4>")
        ),
        size = "s",
        footer = tagList(
          actionButton(
            "cancel_cancel",
            label = "No",
            style = "background-color: #F44336; color: white"),
          actionButton(
            "real_submit",
            label = "Yes",
            style = "background-color: #304FFE; color: white")
        )
      ))
    } else {
      addClass(id = "key_code", "attention")
    }
  })

  key_ok <- reactive({
    if(input$key_code == Sys.getenv("ESADOC_KEY")) {
      return(TRUE)
    }
    return(FALSE)
  })

  observeEvent(input$real_submit, {
    res <- copy_upload_file()
    file.remove(file_info()$datapath)
    updateSelectInput(session, "doctype", selected = "not_selected")
    hide("submit_btn", anim = TRUE, animType = "slide")
    show("pad_foot_2", anim = TRUE, animType = "slide")
    reset("upload_file")
    removeClass(id = "key_code", "attention")
    removeModal()
  })

  #####################
  ## FILE COPY FN
  prep_pdfpath <- function(f) {
    fname <- gsub(x = f, pattern = " ", replacement = "_")
    fname <- gsub(x = fname, pattern = "&", replacement = "and")
    fname <- gsub(x = fname, pattern = "\\(|\\)|\'|\"", replacement = "")
    fname <- gsub(x = fname, pattern = "\\,", replacement = "")
    fname <- gsub(x = fname,
                  pattern = ".pdf$|.PDF$",
                  replacement = paste0("_", rand_str(5), ".pdf"))
    # dest <- file.path("/home/jacobmalcom/Data/bulk_ESAdocs",
    #                   input$doctype,
    #                   fname)
    dest <- file.path("/Users/jacobmalcom/Work/Data/bulk_ESAdocs",
                      input$doctype,
                      fname)
    return(dest)
  }

  copy_upload_file <- function() {
    pdf_paths <- sapply(file_info()$name, prep_pdfpath)
    cp_res <- file.copy(file_info()$datapath, pdf_paths, overwrite = FALSE)
    res_dat <- rbind(file_info(), cp_res, pdf_paths)
    if(any(!cp_res)) {
      output$msg <- renderTable(res_dat)
    }
    return(res_dat)
  }



  ###########################################################################
  ## CANCELS
  ###########################################################################
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

  # Cancel for real and remove cancel modal
  observeEvent(input$real_cancel, {
    file.remove(file_info()$datapath)
    updateTextInput(session, "submitter", value = NA)
    updateTextInput(session, "submitterEmail", value = NA)
    updateSelectInput(session, "doctype", selected = "not_selected")
    reset("upload_file")
    removeModal()
  })

  # Cancel the cancel
  observeEvent(input$cancel_cancel, {
    removeModal()
  })

  ###########################################################################
  ## HELP
  ###########################################################################
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
