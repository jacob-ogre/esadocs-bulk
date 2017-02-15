#! /usr/bin/r
# BSD_2_clause
#
# This script lives on and is run on the OCR_SERVER side of life (of life!).

readRenviron("/home/jacobmalcom/.Renviron")

OCR_PATH <- Sys.getenv("OCR_PATH")
DOC_SERVER <- Sys.getenv("DOC_SERVER")
STG_PATH <- Sys.getenv("STG_PATH")
BULK_PATH <- Sys.getenv("BULK_PATH")

infiles <- list.files(
  OCR_PATH,
  full.names = TRUE,
  recursive = TRUE
)

if(length(infiles) == 0) stop("No files to move")

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
for(i in cur_res$file) {
  if(cur_res$scp_res) {
    file.rename(i, gsub(i, pattern = "bulk_ESAdocs_OCR",
                        replacement = "bulk_ESAdocs_bak"))
  }
}

save(
  cur_res,
  file = file.path(
    BULK_PATH, "rda",
    paste0("scp_OCR_",
           gsub(Sys.time(), pattern = " |:", replacement = "_"),
           ".rda")
  )
)

