test_that(".file_extdata", {
    expect_error(.file_extdata("foo"))
    expect_true(file.exists(.file_extdata("labcodes.csv")))
})
