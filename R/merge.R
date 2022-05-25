#' Merge lab entries
#'
#' Merge laboratory measurements.
#'
#' @param x `data.table`.
#' @param f `factor`, splitting factor
#' @param columns `character`, column names of laboratory measurements in `x`.
#' @return `data.table` depending on `x` with reduced number of rows.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.merge_df_lab <- function(x,
                          columns = c(
                            "CRP", "PCT", "HGB", "MCV", "PLT", "RBC", "WBC")) {
    x[, keep := TRUE]

    .sub <- function(xx) {
        if (nrow(xx) <= 1 || !.is_lab_mergeable(xx[, columns, with = FALSE]))
            xx
        else {
            xx[1L, (columns) :=
                as.list(.merge_lab(as.matrix(xx[, columns, with = FALSE])))
            ]
            xx[, keep := c(TRUE, rep.int(FALSE, nrow(xx) - 1L))]
        }
    }

    x[,
        c(columns, "keep") :=
            as.list(.sub(.SD[, c(columns, "keep"), with = FALSE])),
        by = .(Id, Center, Time)
    ]
    x <- x[(keep),]
    x[, keep := NULL][]
}

#' Test for mergeable Labs
#'
#' Mergeable laboratory measurements are measurements of different laboratory
#' values at the same time (HGB and CRP with same timestamp;
#' in contrast to the same measurements with different
#' results at the same time, e.g. CRP with same timestamp).
#'
#' @param x `matrix`, mode `numeric`, only laboratory values.
#' @return `logical`. `TRUE` if the rows contain mergeable laboratory entries.
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.is_lab_mergeable <- function(x) {
    all(colSums(!is.na(x)) <= 1L)
}

#' Merge Labs
#'
#' @param x `matrix`, mode `numeric`, only laboratory values.
#' @return `double(ncol(x))`
#' @importFrom stats setNames
#' @author Sebastian Gibb <mail@@sebastiangibb.de>
#' @noRd
.merge_lab <- function(x) {
    nna <- !is.na(x)
    cs <- colSums(nna)
    r <- setNames(rep.int(NA_real_, ncol(x)), names(cs))
    r[cs == 1L] <- x[nna]
    r
}
