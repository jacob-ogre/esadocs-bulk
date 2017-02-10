# BSD_2_clause

library(digest)
library(dplyr)
library(elastic)
library(pdftools)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(stringr)

# options(shiny.reactlog=TRUE)
system("touch /home/shiny/esadocs-upload/restart.txt", intern = FALSE)
readRenviron("/home/jacobmalcom/.Renviron")
# readRenviron("/Users/jacobmalcom/.Renviron")

elastic::connect()

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

myFileInput <- function(inputId, label, multiple = FALSE, accept = NULL,
                        placeholder = "No file selected", width = NULL) {

  restoredValue <- restoreInput(id = inputId, default = NULL)

  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }

  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }

  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    style = "display: none;",
    `data-restore` = restoredValue
  )

  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse=',')

  div(
    class = "form-group shiny-input-container",
    style = if(!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    label %AND% tags$label(label),

    div(
      class = "input-group",
      tags$label(
        class = "input-group-btn",
        span(
          class = "btn btn-default btn-file",
          "Browse...",
          inputTag
        )
      ),
      tags$input(
        type = "text",
        class = "form-control",
        placeholder = placeholder,
        readonly = "readonly"
      )
    ),

    tags$div(
      id=paste(inputId, "_progress", sep=""),
      class="progress progress-striped active shiny-file-input-progress",
      tags$div(class="progress-bar")
    )
  )
}

