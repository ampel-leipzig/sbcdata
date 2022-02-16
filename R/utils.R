#' Get internal file path to CSV files
#'
#' Helper function to easily access CSV files in `inst/extdata` directory.
#'
#' @param x `character`, base file name.
#' @return `character` full file path.
#' @noRd
.file_extdata <- function(x) {
    system.file("extdata", x, package = "sbcdata", mustWork = TRUE)
}
