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

  # with_txt <- reactive({
  #   if(!is.null(file_info())) {
  #     with_text <- try(pdf_text(file_info()$datapath), silent = TRUE)
  #     txt_tests <- sapply(
  #       file_info()$datapath, function(x) try(pdf_text(x), silent = TRUE)
  #     )
  #     if(any(class(txt_tests) == "try-error")) {
  #       return(FALSE)
  #     } else {
  #       tokenize <- function(x) {
  #         tmp <- unlist(str_split(paste(x, collapse = " "), " "))
  #         length(tmp)
  #       }
  #       tokens <- sapply(txt_tests, tokenize)
  #       if(any(tokens) < 10) {
  #         return(FALSE)
  #       } else {
  #         return(TRUE)
  #       }
  #     }
  #   }
  #   return(FALSE)
  # })

  submitter_ok <- reactive({
    if(nchar(input$submitter) > 4 & nchar(input$submitter) < 256) {
      return(TRUE)
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


})
