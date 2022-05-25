test_that(".merge_df_lab", {
    x <- data.table(
        Id = rep(1:4, c(1, 3, 2, 1)),
        Center = rep(c("G", "L"), c(6, 1)),
        Time = c(0, 0, 0, 1, 0, 0, 0),
        CRP = c(NA, NA, 100, NA, NA, NA, 5),
        HGB = c(7, 6, NA, 5, 6.4, 6.5, NA),
        HCT = c(0.3, 0.4, NA, 0.4, 0.3, 0.4, NA)
    )
    r <- data.table(
        Id = rep(1:4, c(1, 2, 2, 1)),
        Center = rep(c("G", "L"), c(5, 1)),
        Time = c(0, 0, 1, 0, 0, 0),
        CRP = c(NA, 100, NA, NA, NA, 5),
        HGB = c(7, 6, 5, 6.4, 6.5, NA),
        HCT = c(0.3, 0.4, 0.4, 0.3, 0.4, NA)
    )
    expect_equal(.merge_df_lab(x, columns = c("CRP", "HGB", "HCT")), r)
})

test_that(".is_lab_mergeable", {
    x <- cbind(
        CRP = c(NA, NA, 100),
        HGB = c(6.5, 6, NA),
        HCT = c(0.3, 0.25, NA)
    )

    expect_false(.is_lab_mergeable(x[c(1, 2),]))
    expect_false(.is_lab_mergeable(x))
    expect_true(.is_lab_mergeable(x[c(1, 3),]))
    expect_true(.is_lab_mergeable(x[c(2, 3),]))

    x[2, 2] <- NA
    expect_true(.is_lab_mergeable(x[c(2, 3),]))
})

test_that(".merge_lab", {
    x <- cbind(
        CRP = c(NA, 100),
        HGB = c(6.5, NA),
        HCT = c(0.3, NA)
    )
    expect_equal(.merge_lab(x), c(CRP = 100, HGB = 6.5, HCT = 0.3))

    x <- cbind(
        CRP = c(NA, 100),
        HGB = c(NA, 6.5),
        HCT = c(0.3, NA)
    )
    expect_equal(.merge_lab(x), c(CRP = 100, HGB = 6.5, HCT = 0.3))
})
