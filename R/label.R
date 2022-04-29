#' Label CBCs
#'
#' Run the labeling as done in the AMPEL project.
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @param time `numeric(1)`, label entries with `"SecToIcu"`
#' below this time and `Diagnosis == "Sepsis"` as "Sepsis" or as "Control"
#' otherwise.
#' @return `data.table`, same as `x` but with an added column `Label` and
#' 3 attributes, namely `"label_message"`, `"label_cases"`, `"label_cbc"` that
#' describe the labeling and the affected cases and complete blood counts (CBC).
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @import data.table
#' @export
#' @examples
#' x <- sbc_exclude_entries(sbcdata)
#' attributes(x)
#' x <- sbc_label(x)
#' attributes(x)
sbc_label <- function(x, time = 6 * 3600) {
    keep <- .is_only_sepsis(x) & .is_time_range(x, c(-Inf, time)) & !x$Excluded
    changed <- .is_only_sepsis(x) & !keep & !x$Excluded
    x[, Label := ifelse(keep, "Sepsis", "Control")]
    msg <- c(
        "Analysed control group",
        "Analysed sepsis group",
        sprintf("CBCs > %.0f h before ICU admission", time / 3600)
    )
    ncases <- c(
        .count_cbc_cases(x[!keep & !x$Excluded,]),
        .count_cbc_cases(x[keep,]),
        .count_cbc_cases(x[changed,])
    )
    ncbc <- c(
        count_cbc(x[!keep & !x$Excluded,]),
        count_cbc(x[keep,]),
        count_cbc(x[changed,])
    )
    setattr(x, "label_message", msg)
    setattr(x, "label_cases", ncases)
    setattr(x, "label_cbc", ncbc)
    x
}
