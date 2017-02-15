#! /usr/bin/r
# BSD_2_clause
#
# This script lives on and is run on the DOC_SERVER side.

library(digest)
library(dplyr)
library(elastic)
library(pdftools)

readRenviron("/home/jacobmalcom/.Renviron")

DOC_SERVER <- Sys.getenv("DOC_SERVER")
DOC_PATH <- Sys.getenv("DOC_PATH")
STG_PATH <- Sys.getenv("STG_PATH")

infiles <- list.files(
  STG_PATH,
  pattern = "pdf$",
  full.names = TRUE,
  recursive = TRUE
)
if(length(infiles) == 0) stop("No files to accession")

rand_str <- function(len=30) {
  str <- paste(
    sample(c(rep(0:9,each=5), LETTERS, letters), len, replace=TRUE), collapse=''
  )
  return(str)
}

accession_file <- function(f) {
  raw_txt <- unlist(pdf_text(f))
  pdf_md5 <- digest::digest(f, file = TRUE)
  type <- basename(dirname(f))
  newf <- gsub(f, pattern = "stage_ESAdocs", replacement = "ESAdocs")
  pdf_path <- gsub(
    newf,
    pattern = "/home/jacobmalcom/Data",
    replacement = "https://esadocs.cci-dev.org"
  )
  doc_id <- rand_str()
  body <- list(
    raw_txt = raw_txt,
    pdf_path = pdf_path,
    pdf_md5 = pdf_md5
  )
  acc_f <- docs_create(
    index = "esadocs",
    type = type,
    id = doc_id,
    body = body
  )
  rn <- file.rename(f, newf)
  chmod <- Sys.chmod(newf, mode = "0644")
  return(list(acc_res = acc_f$result,
              mv_res = rn,
              doc_id = doc_id))
}

connect()
acc_res <- bind_rows(lapply(infiles, accession_file))
acc_res$file <- infiles

save(acc_res,
     file = file.path(
       DOC_PATH, "rda",
       paste0("accession_result_",
              gsub(Sys.time(), pattern = " |:", replacement = "_"),
              ".rda")
     )
)
