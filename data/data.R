## make CSV files accessible by variable names
delayedAssign("sbcdata", local({
    data.table::fread(
        file = utils::unzip(
            sbcdata:::.file_extdata("sbcdata.csv.zip"),
            junkpaths = TRUE,
            exdir = tempdir()
        ),
        na.strings = c("NA", ""),
        showProgress = FALSE
    )
}))
delayedAssign("labcodes", local({
    data.table::fread(sbcdata:::.file_extdata("labcodes.csv"))
}))
delayedAssign("sendercodes", local({
    data.table::fread(sbcdata:::.file_extdata("sendercodes.csv"))
}))
