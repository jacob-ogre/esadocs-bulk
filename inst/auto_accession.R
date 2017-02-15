#! /usr/bin/r
# BSD_2_clause
#
# This script lives on and is run on the DOC_SERVER side.

library(digest)
library(elastic)
library(pdftools)

readRenviron("/home/jacobmalcom/.Renviron")

DOC_SERVER <- Sys.getenv("DOC_SERVER")
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
  body <- list(
    raw_txt = raw_txt,
    pdf_path = pdf_path,
    pdf_md5 = pdf_md5
  )
  print(paste("type:", type))
  print("body:")
  print(body)
  # acc_f <- docs_create(
  #   index = "esadocs",
  #   type = type,
  #   id = rand_str(),
  #   body = body
  # )
  # rn <- file.rename(f, newf)
  rn <- TRUE
  return(list(acc_res = acc_f$result,
              mv_res = rn))
}

connect()
acc_res <- unlist(lapply(infiles, accession_file))
acc_res <- data.frame(
  file <- infiles,
  acc_res <- acc_res$acc_res,
  mv_res <- acc_res$mv_res,
  stringsAsFactors = FALSE
)

save(acc_res,
     file = file.path(
       BULK_PATH, "rda",
       paste0("accession_result_",
              gsub(Sys.time(), pattern = " |:", replacement = "_"),
              ".rda")
     )
)
