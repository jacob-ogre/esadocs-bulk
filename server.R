# BSD_2_clause

options(shiny.maxRequestSize=30*1024^2)

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
      ckclss <- sapply(pdftests, class)
      names(ckclss) <- seq_along(ckclss)
      if(any(ckclss == "try-error")) {
        res <- cbind(file_info(), ckclss)
        return(res)
      } else {
        return("Yes")
      }
    }
    return("No")
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
    if(input$doctype != "not_selected") {
      removeClass("docdiv", "attn_outline")
    }
  })

  observe({
    if(!is.null(file_info())) {
      pdf_chk <- is_pdf()
      if(identical(pdf_chk, "Yes")) {
        show("submit_btn", anim = TRUE, animType = "fade")
        hide("pad_foot_2")
        output$to_upl_files <- renderTable(
          dplyr::select(file_info(), name, size),
          striped = TRUE,
          hover = FALSE,
          bordered = FALSE,
          width = "100%"
        )
        if(dim(file_info())[1] <= 5) {
          createAlert(
            session,
            "too_few",
            title = "Not many docs",
            content = paste("Please consider using the",
                            "<a href='https://esadocs.cci-dev.org/esadocs-upload/'>",
                            "ESAdocs Upload</a> app. Adding document data during",
                            "manually during upload can really improve search results."),
            style = "warning",
            dismiss = TRUE
          )
        }
      } else if(class(pdf_chk) == "data.frame") {
        bad_fil <- dplyr::filter(pdf_chk, ckclss == "try-error")
        createAlert(
          session,
          "not_a_pdf",
          title = "PDF problem",
          content = paste("These files are not PDFs or are damaged:<br>&nbsp&nbsp",
                          paste(bad_fil$name, collapse = "<br>&nbsp&nbsp"),
                          "<br>Please remove or replace these with valid PDFs."),
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
        HTML(paste("<h5>Submit these",
                   dim(file_info())[1],
                   "PDFs to ESAdocs?</h5>")
        ),
        hidden(div(
          id = "save_error",
          p(style = "color:red; font-weight:bold; font-size:larger",
            "WARNING: Upload record failed to save")
        )),
        hidden(div(
          id = "waiting",
          p("Transferring...")
        )),
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
    subf <- file.path("/home/jacobmalcom/Data/bulk_ESAdocs", "rda",
                      paste0("bulk_upload_", dim(res)[1],
                             "_docs_", Sys.Date(), "_", rand_str(5), ".rda"))
    # subf <- file.path("/Users/jacobmalcom/Work/Data/bulk_ESAdocs", "rda",
    #                   paste0("bulk_upload_", dim(res)[1],
    #                          "_docs_", Sys.Date(), "_", rand_str(5), ".rda"))
    sav_dat <- try(save(res, file = subf), silent = TRUE)
    if(class(sav_dat) == "try-error") {
      show("save_err")
    }
    show("waiting")
    scp_res <- copy_to_ocrvm(res)
    if(all(scp_res == 0)) {
      file.remove(file_info()$datapath)
      updateSelectInput(session, "doctype", selected = "not_selected")
      addClass("docdiv", "attn_outline")
      hide("submit_btn", anim = TRUE, animType = "slide")
      reset("upload_file")
      removeClass(id = "key_code", "attention")
      removeModal()
      createAlert(
        session,
        "success",
        title = "Success!",
        content = paste("Your", dim(file_info())[1],
                        "ESA-related PDFs were uploaded."),
        style = "success",
        dismiss = TRUE
      )
    } else {
      createAlert(
        session,
        "success",
        title = "Error!",
        content = paste("Your", dim(file_info())[1],
                        "ESA-related PDFs were uploaded but were not",
                        "copied to the processing server. Please",
                        "<a href='esa@defenders.org'>contact us</a> if the",
                        "problem persists."),
        style = "error",
        dismiss = TRUE
      )
      updateSelectInput(session, "doctype", selected = "not_selected")
      addClass("docdiv", "attn_outline")
      removeClass(id = "key_code", "attention")
      removeModal()
    }
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
    dest <- file.path("/home/jacobmalcom/Data/bulk_ESAdocs",
                      input$doctype,
                      fname)
    # dest <- file.path("/Users/jacobmalcom/Work/Data/bulk_ESAdocs",
    #                   input$doctype,
    #                   fname)
    return(dest)
  }

  copy_upload_file <- function() {
    pdf_paths <- sapply(file_info()$name, prep_pdfpath)
    cp_res <- file.copy(file_info()$datapath, pdf_paths, overwrite = FALSE)
    res_dat <- cbind(file_info(), cp_res, pdf_paths,
                     rep(input$submitter, length(pdf_paths)),
                     rep(input$submitter_email, length(pdf_paths)),
                     rep(input$doctype, length(pdf_paths)))
    return(res_dat)
  }

  copy_to_ocrvm <- function(df) {
    get_cmd <- function(x) {
      paste0("scp -C ", x, " ", Sys.getenv("OCR_SERVER"), "/", input$doctype, "/")
    }
    cmd <- paste0("scp -C ", paste(df$pdf_paths, collapse = " "), " ",
                  Sys.getenv("OCR_SERVER"), "/", input$doctype, "/")
    scp_res <- system(cmd, wait = TRUE)
    return(scp_res)
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
