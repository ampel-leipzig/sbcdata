#' AMPEL Preprocessing
#'
#' Run the preprocessing as done in the AMPEL project.
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @return `data.table`, same as `x` but with additional columns `Excluded` and
#' `Label`. Also 6 attributes are added:
#' `"exclude_message"`, `"exclude_cases"`, `"exclude_cbc"` that
#' describe the exclusion and the kept cases and complete blood counts (CBC).
#' `"label_message"`, `"label_cases"`, `"label_cbc"` that
#' describe the labeling and the affected cases and complete blood counts (CBC).
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @import data.table
#' @export
sbc_preprocess <- function(x) {
    sbc_label(sbc_exclude_entries(x))
}
