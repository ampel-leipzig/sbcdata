test_that("count_cases", {
    x <- data.table(
        Id = c(rep(1, 3), 2),
        Center = c("G", "G", "L", "L")
    )
    expect_identical(count_cases(x), 3L)
})

test_that("count_cbc", {
    x <- data.table(
        CRP = c(NA, NA, 100, NA),
        HGB = c(6.5, 6, NA, NA),
        HCT = c(0.3, 0.25, NA, 0.24)
    )
    expect_identical(count_cbc(x, c("HGB", "HCT")), 3L)
    expect_identical(count_cbc(x, "HGB"), 2L)
    expect_identical(count_cbc(x, c("HGB", "HCT"), complete = TRUE), 2L)
})

test_that(".count_cbc_cases", {
    x <- data.table(
        Id = c(1, 1, 2, 3),
        Center = "Leipzig",
        CRP = c(NA, NA, 100, NA),
        HGB = c(6.5, 6, NA, NA),
        HCT = c(0.3, 0.25, NA, 0.24)
    )
    expect_identical(.count_cbc_cases(x, c("HGB", "HCT")), 2L)
    expect_identical(.count_cbc_cases(x, c("HGB", "HCT"), complete = TRUE), 1L)
})
