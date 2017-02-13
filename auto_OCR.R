#! /usr/bin/r
# BSD_2_clause
#
# This script lives on and is run on the OCR_SERVER side of life (of life!).

library(parallel)

OCR_proc <- function(infile) {
  outf <- gsub(infile, pattern = "bulk_ESAdocs", replacement = "bulk_ESAdocs_OCR")
  cmd <- paste0("ocrmypdf ",
                "--deskew ",
                "--rotate-pages --rotate-pages-threshold 10 ",
                "--oversample 300 ",
                "--skip-text ",
                "-l eng --tesseract-config ~/asciimostly '",
                infile,
                "' ",
                outf)
  if(!file.exists(outf)) {
    res <- try(system(command = cmd, intern = FALSE, wait = TRUE))
    if(res[1] == "try-error") {
      return(res)
    } else {
      return(0)
    }
  } else {
    return("File exists")
  }
}

infiles <- list.files(
  Sys.getenv("OCR_PATH"),
  full.names = TRUE,
  recursive = TRUE
)

cur_res <- mclapply(
  X = infiles,
  FUN = wrap_ocrmypdf,
  mc.preschedule = FALSE,
  mc.cores = 5
)
