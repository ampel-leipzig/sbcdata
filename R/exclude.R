#' AMPEL Preprocessing/Exclusion
#'
#' Run the preproccesing/exclusion as done in the AMPEL project.
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @return `data.frame`, same as `x` but with an added column `Excluded` and
#' 3 attributes `"message"`, `"n_cases"`, `"n_cbc"` that describe the
#' processing and the kept cases and complete blood counts (CBC)
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @import data.table
#' @export
#' @examples
#' x <- exclude_entries(sbcdata)
#' attributes(x)
exclude_entries <- function(x) {
    x[, Excluded := FALSE]
    msg <- character()
    ncases <- integer()
    ncbc <- integer()

    ## All
    msg <- c(msg, "All cases aged >= 18")
    ncases <- c(ncases, .count_cbc_cases(x))
    ncbc <- c(ncbc, count_cbc(x))

    ## Exclude duplicated laboratory diagnostics.
    excl <- .is_duplicated_lab_diagnostic(x)
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "Duplicated blood samples")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Exclude CBC from ICU
    excl <- .is_sender_icu(x, icu = "ICU") | .is_sectoicu_negative(x)
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "BCs from ICU")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Exclude second, third, ... episode
    excl <- x$Episode > 1 | is.na(x$Episode)
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "BCs from following episodes")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Exclude incomplete CBC
    excl <- !.is_cbc(x, complete = TRUE)
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "Incomplete BCs")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Exclude SIRS only
    excl <- .is_only_sirs(x)
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "Cases with SIRS but without Sepsis diagnosis")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Current Status
    msg <- c(msg, "Assessable")
    ncases <- c(ncases, .count_cbc_cases(x[!x$Excluded,]))
    ncbc <- c(ncbc, count_cbc(x[!x$Excluded,]))

    ## Control Group
    msg <- c(msg, "Analysed control group")
    ncases <- c(ncases, .count_cbc_cases(x[!x$Excluded & x$Diagnosis == "Control",]))
    ncbc <- c(ncbc, count_cbc(x[!x$Excluded & x$Diagnosis == "Control",]))

    ## Sepsis group
    msg <- c(msg, "Sepsis")
    sepsis <- .is_only_sepsis(x)
    ncases <- c(ncases, .count_cbc_cases(x[!x$Excluded & sepsis,]))
    ncbc <- c(ncbc, count_cbc(x[!x$Excluded & sepsis,]))

    ## Exclude Sepsis cases that are not admitted to a medical ICU
    excl <- !.is_target_icu(x, "MICU")
    excl <- (is.na(excl) | excl) & sepsis
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "Cases admitted to non-medical ICU")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Shouldn't we move > 72 h before Admission to Control?
    ## Exclude CBC in Sepsis cases not between 72-6 h before admission to ICU
    excl <- !.is_time_range(x, c(72, 6) * 3600)
    excl <- (is.na(excl) | excl) & sepsis
    newex <- !x$Excluded & excl
    x[, Excluded := Excluded | excl]
    msg <- c(msg, "BC not >= 6 h and <= 72 h before ICU admission")
    ncases <- c(ncases, .count_cbc_cases(x[newex,]))
    ncbc <- c(ncbc, count_cbc(x[newex,]))

    ## Sepsis group
    msg <- c(msg, "Analysed sepsis group")
    ncases <- c(ncases, .count_cbc_cases(x[!x$Excluded & sepsis,]))
    ncbc <- c(ncbc, count_cbc(x[!x$Excluded & sepsis,]))

    setattr(x, "message", msg)
    setattr(x, "n_cases", ncases)
    setattr(x, "n_cbc", ncbc)
    x
}

#'
#' Test for Complete Blood Count
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @param columns `character`, name of columns from [`sbcdata`] that should
#' be treated as complete blood count (CBC).
#' @param complete `logical`, if `TRUE` the CBC is only count if none of the
#' `columns` is `NA`.
#' @return `logical` vector. `TRUE` if the row/entry is part/complete CBC.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_cbc <- function(x, columns = c("HGB", "MCV", "PLT", "RBC", "WBC"),
                    complete = FALSE) {
    rs <- rowSums(is.na(x[, columns, with = FALSE]))

    if (complete)
        rs == 0
    else
        rs < length(columns)
}

#' Test for Duplicated Laboratory Diagnostics
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @return `logical` vector. `TRUE` if the row/entry is a duplicated laboratory
#' diagnostic.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_duplicated_lab_diagnostic <- function(x) {
    duplicated(x[, .(Id, Center, Time)]) |
        duplicated(x[, .(Id, Center, Time)], fromLast = TRUE)
}

#' Test for CBC from Sender ICU
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @param icu `character`, regular expression used to determine intensive care
#' units in the `Sender` column of `x`.
#' @param ... passed to [`grepl()`].
#' @return `logical` vector. `TRUE` if the CBC was sent from an ICU.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_sender_icu <- function(x, icu = "IMC|ICU", ...) {
    grepl(icu, x$Sender)
}

#' Test for CBC from sender ICU based on SecToIcu
#'
#' If `SecToIcu` is negativ, the blood sample was analysed after submission to
#' an ICU (even if the Sender was originally a non-ICU ward).
#'
#' @param x `data.table`, in the format described in [`sbcdata`]
#' @return `logical` vector. `TRUE` if the CBC was sent from an ICU.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_sectoicu_negative <- function(x) {
    (!is.na(x$SecToIcu) & x$SecToIcu < 0L)
}

#' Test for Sepsis-only Cases
#'
#' @param x `data.frame`, in the format described in [`sbcdata`]
#' @return `logical` vector. `TRUE` if the case was labeled as Sepsis.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_only_sepsis <- function(x) {
    x$Diagnosis == "Sepsis"
}

#' Test for SIRS-only Cases
#'
#' @param x `data.frame`, in the format described in [`sbcdata`]
#' @return `logical` vector. `TRUE` if the case was labeled as SIRS exclusively.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_only_sirs <- function(x) {
    x$Diagnosis == "SIRS"
}

#' Test for ICU
#'
#' @param x `data.frame`, in the format described in [`sbcdata`]
#' @param target `character`, regular expression used to determine intensive care
#' units in the `TargetIcu` column of `x`.
#' @param ... passed to [`grepl()`].
#' @return `logical` vector. `TRUE` if the case was admitted to the
#' targeted ICU in the current episode.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_target_icu <- function(x, target = "MICU", ...) {
    grepl(target, x$TargetIcu, ...)
}

#' Test for Adequat Time Range
#'
#' @param x `data.frame`, in the format described in [`sbcdata`]
#' @param range `numeric`, valid time range.
#' @param column `character`, time column used.
#' @return `logical` vector. `TRUE` if the row/entry is between time range
#' `range`.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_time_range <- function(x, range = c(72, 6) * 3600, column = "SecToIcu") {
    if (!is.numeric(range) || length(range) != 2)
        stop("'range' has to be a numeric of length 2!")
    range <- range(range)
    x[[column]] >= range[1L] & x[[column]] <= range[2L]
}
