#' Count Cases or Blood Counts
#'
#' Function to count number of individual cases and blood counts.
#'
#' @param x `data.table`, in the format described in [`sbcdata`].
#' @return `integer`, number of cases or blood counts
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @rdname counts
#' @export
#' @examples
#' count_cases(sbcdata)
count_cases <- function(x) {
    uniqueN(x, by = c("Id", "Center"))
}

#' @rdname counts
#' @param columns `character`, name of columns from [`sbcdata`] that should
#' be treated as complete blood count (CBC).
#' @param complete `logical`, if `TRUE` the CBC is only count if none of the
#' `columns` is `NA`.
#' @export
#' @examples
#' count_cbc(sbcdata)
count_cbc <- function(x, columns = c("HGB", "MCV", "PLT", "RBC", "WBC"),
                      complete = FALSE) {
    sum(.is_cbc(x, columns, complete))
}

#' Count CBC cases
#'
#' Function to count cases with (in)complete CBC (ignoring CRP/PCT and so on)
#'
#' @param x `data.table`, in the format described in [`sbcdata`].
#' @param columns `character`, name of columns from [`sbcdata`] that should
#' be treated as complete blood count (CBC).
#' @param complete `logical`, if `TRUE` the CBC is only count if none of the
#' `columns` is `NA`.
#' @examples
#' .count_cbc_cases(sbcdata)
#' @noRd
.count_cbc_cases <- function(x, columns = c("HGB", "MCV", "PLT", "RBC", "WBC"),
                             complete = FALSE) {
    count_cases(x[.is_cbc(x, columns, complete),])
}
