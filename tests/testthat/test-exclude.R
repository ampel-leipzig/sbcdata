s <- data.table(
    Id = c(rep(1, 3), 2),
    Diagnosis = c(rep("Sepsis", 3), "SIRS"),
    Center = rep("G", 4),
    SecToIcu = c(1, -1, -1, NA),
    Sender = c("ED", "CIMC", "SICU", "MICU"),
    TargetIcu = c("CIMC", "CIMC", "SICU", "MICU"),
    CRP = c(NA, NA, 100, NA),
    HGB = seq(6, 8, length.out = 4)
)

test_that(".is_cbc", {
    x <- data.table(
        CRP = c(NA, NA, 100, NA),
        HGB = c(6.5, 6, NA, NA),
        HCT = c(0.3, 0.25, NA, 0.24)
    )
    expect_identical(.is_cbc(x, c("HGB", "HCT")), c(TRUE, TRUE, FALSE, TRUE))
    expect_identical(.is_cbc(x, "HGB"), c(TRUE, TRUE, FALSE, FALSE))
    expect_identical(
        .is_cbc(x, c("HGB", "HCT"), complete = TRUE),
        c(TRUE, TRUE, FALSE, FALSE)
    )
})

test_that(".is_cbc", {
    x <- data.table(
        Id = c(1, 1, 2, 3, 1),
        Center = c(rep("G", 4), "L"),
        Time = c(1, 1, 1, 2, 1)
    )
    expect_identical(
        .is_duplicated_lab_diagnostic(x),
        c(TRUE, TRUE, FALSE, FALSE, FALSE)
    )
})

test_that(".is_sender_icu", {
    expect_identical(.is_sender_icu(s), c(FALSE, TRUE, TRUE, TRUE))
    expect_identical(.is_sender_icu(s, "ICU"), c(FALSE, FALSE, TRUE, TRUE))
})

test_that(".is_sectoicu_negative", {
    expect_identical(.is_sectoicu_negative(s), c(FALSE, TRUE, TRUE, FALSE))
})

test_that(".is_only_sepsis", {
    expect_identical(.is_only_sepsis(s), c(TRUE, TRUE, TRUE, FALSE))
})

test_that(".is_only_sirs", {
    expect_identical(.is_only_sirs(s), c(FALSE, FALSE, FALSE, TRUE))
})

test_that(".is_target_icu", {
    expect_identical(.is_target_icu(s), c(FALSE, FALSE, FALSE, TRUE))
    expect_identical(.is_target_icu(s, "IMC"), c(TRUE, TRUE, FALSE, FALSE))
})

test_that(".is_time_range", {
    x <- data.table(
        SecToIcu = 1:5
    )
    expect_error(.is_time_range(x, 1:5), "length 2")
    expect_identical(.is_time_range(x, c(1, 5)), rep(TRUE, 5))
    expect_identical(
        .is_time_range(x, c(2, 4)), c(FALSE, TRUE, TRUE, TRUE, FALSE)
    )
})
