###############################################################################
## Please source the `1-umg.R` and `2-ukl.R` first
#' if the dataset should be regenerated.
###############################################################################

library("data.table")

full <- rbind(
    readRDS("umg.RDS"),
    readRDS("ukl.RDS")
)

csvfile <- file.path("..", "extdata", "sbcdata.csv")
zipfile <- paste0(csvfile, ".zip")

fwrite(full, file = csvfile)
unlink(zipfile)
zip(zipfile, csvfile)
unlink(csvfile)
