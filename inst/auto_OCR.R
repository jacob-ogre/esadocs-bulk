#! /usr/bin/r
# BSD_2_clause
#
# This script lives on and is run on the OCR_SERVER side of life (of life!).

readRenviron("/home/jacobmalcom/.Renviron")

library(dplyr)
library(parallel)

#' Use ocrmypdf to OCR a given infile; OCR version to bulk_ESAdocs_OCR
#'
OCR_proc <- function(infile) {
  outf <- gsub(infile, pattern = "bulk_ESAdocs", replacement = "bulk_ESAdocs_OCR")
  outf <- gsub(outf, pattern = "\\.pdf$|\\.PDF$", replacement = "_OCR.pdf")
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
    if(class(res)[1] == "try-error") {
      return(res)
    } else {
      return(0)
    }
  } else {
    return("File exists")
  }
}

BULK_PATH <- Sys.getenv("BULK_PATH")
infiles <- list.files(
  BULK_PATH,
  pattern = "pdf$|PDF$",
  full.names = TRUE,
  recursive = TRUE
)
if(length(infiles) == 0) stop("No files to OCR")

if(length(system("pgrep auto", intern = TRUE)) <= 1) {
  cur_res <- mclapply(
    X = infiles,
    FUN = OCR_proc,
    mc.preschedule = FALSE,
    mc.cores = 5
  )
} else {
  stop("Already running OCR.")
}

cur_res_df <- data_frame(
  file = infiles,
  ocr_res = unlist(cur_res)
)

for(i in 1:dim(cur_res_df)[1]) {
  if(cur_res_df$ocr_res == 0) {
    file.rename(
      cur_res_df$file[i],
      gsub(
        cur_res_df$file[i],
        pattern = "bulk_ESAdocs",
        replacement = "bulk_ESAdocs_bak"
      )
    )
  }
}

save(
  cur_res_df,
  file = file.path(
    BULK_PATH, "rda",
    paste0("cur_res_df_",
           gsub(Sys.time(), pattern = " |:", replacement = "_"),
           ".rda")
  )
)
