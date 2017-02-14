#! /usr/bin/r
# BSD_2_clause
#
# This script lives on and is run on the OCR_SERVER side of life (of life!).

res <- try(readRenviron("/home/jacobmalcom/.Renviron"))

OCR_PATH <- paste0(Sys.getenv("OCR_PATH"), "_OCR")
DOC_SERVER <- Sys.getenv("DOC_SERVER")
STG_PATH <- Sys.getenv("STG_PATH")

infiles <- list.files(
  OCR_PATH,
  full.names = TRUE,
  recursive = TRUE
)

scp_cmd <- function(f) {
  dir <- basename(dirname(f))
  cmd <- paste0("scp -C ", f, " ", DOC_SERVER, ":", STG_PATH, "/", dir)
  res <- try(system(cmd, wait = TRUE, intern = TRUE))
  if(class(res) == "try-error") return(FALSE)
  return(TRUE)
}

scp_res <- lapply(infiles, scp_cmd)
scp_res <- unlist(scp_res)
cur_res <- data.frame(file = infiles, scp_res = scp_res)
