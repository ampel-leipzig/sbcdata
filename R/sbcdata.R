#' Laboratory Diagnostics of Patients at University Hospitals
#'
#' This dataset includes laboratory diagnostics for the
#' complete blood counts without differentiation,
#' C-reactive protein and procalictonin for
#' patients admitted at the University Hospital Greifswald and Leipzig from
#' 2015 to 2020 and from 2014 to 2019, respectively.
#'
#' @format A `data.table` with 17 columns/variables:
#' \describe{
#'  \item{Id}{`integer`, identification number of the case, unique for each
#'  center.}
#'  \item{Age}{`integer`, age of the patient in years.}
#'  \item{Sex}{`character`, "`W`" for female and `"M"` for male.}
#'  \item{Diagnosis}{`character`, diagnosis, could be `"Control"`, `"SIRS"`
#'  and `"Sepsis"`. See below for details.}
#'  \item{Center}{`character`, center, one of `"Greifswald"` or `"Leipzig"`.}
#'  \item{Sender}{`character`, sender/origin which send the blood sample to the
#'  laboratory. See [`sendercodes`] for a description of all possible codes.}
#'  \item{Epsiode}{`integer`, counter for episodes on intensive care units,
#'  is incremented by one after each discharge from the intensive care unit.}
#'  \item{Time}{`integer`, (relative) time when the blood was analysed.
#'  The first timepoint for each case is always set to zero.}
#'  \item{TragetIcu}{`character`, the name/type of the intensive care unit where
#'  the patient/case has to be admitted to. See [`sendercodes`] for a
#'  description of all possible codes.}
#'  \item{SecToIcu}{`integer`, time in seconds until the patient/case has to be
#'  admitted to the `TargetIcu` intensive care unit.
#'  This time is negative if the patient/case is already on the
#'  intensive care unit `TargetIcu`.}
#'  \item{CRP}{`double`, C-reactive protein in mg/l.}
#'  \item{HGB}{`double`, hemoglobin in mmol/l.}
#'  \item{MCV}{`double`, mean corpuscular volume in fl.}
#'  \item{PLT}{`double`, procalcitonin in Gpt/l.}
#'  \item{RBC}{`double`, platelets in Tpt/l.}
#'  \item{WBC}{`double`, red blood count in Gpt/l.}
#'  \item{PCT}{`double`, white blood count in ng/ml.}
#' }
#'
#' @source{
#' ## Data University Medicine Greifswald
#' \describe{
#'  \item{Data Processing/R package}{Sebastian Gibb}
#'  \item{Laboratory Data Collection/Extraction}{Matthias Nauck and Stefan Bollmann}
#'  \item{Administration Data Extraction}{Thomas Hildebrandt}
#'  \item{Reference Ethic Committee}{BB133/10}
#' }}
#'
#' @source{
#' ## Data University Hospital Leipzig
#' \describe{
#'  \item{R package}{Sebastian Gibb}
#'  \item{Data Processing}{Paul Ahrens and Mark Wernsdorfer}
#'  \item{Laboratory Data Collection/Extraction}{Thorsten Kaiser}
#'  \item{Administration Data Extraction}{Thorsten Kaiser}
#'  \item{Reference Ethic Committee}{214/18ek}
#' }}
#'
#' @details
#' The `Diagnosis` was based on ICD10 codes. `"Sepsis"` was assumed for:
#' \itemize{
#'  \item{A02.1}
#'  \item{A20.7}
#'  \item{A22.7}
#'  \item{A24.1}
#'  \item{A26.7}
#'  \item{A32.7}
#'  \item{A39.2, A39.3, A39.4}
#'  \item{A40.0, A40.1, A40.2, A40.3, A40.8, A40.9}
#'  \item{A41.0, A41.1, A41.2, A41.3, A41.4, A41.51, A41.52, A41.58, A41.8,
#'  A41.9}
#'  \item{A42.7}
#'  \item{B37.7}
#'  \item{R57.2}
#' }
#' If the ICD10 code was R65.x without any of the sepsis-related codes above the
#' `Diagnoses` `"SIRS"` was used. Everything else is labeled as "`Control`".
#'
#' For the `Center` `"Greifswald"` there are a few entries with duplicated
#' time points `Time` for the same `Id` and `Sender` with different laboratory
#' values. This happens due to the analyses of multiple blood samples from the
#' same patient at the same time in the same run of the analyser.
#' It could not be decided which one is the correct/better one so removal is
#' suggested. An example could be found below.
#'
#' At the `Center` `"Greifswald"` the admission/discharge timepoint was recorded
#' in and extracted from the clinical information system. These data were not
#' available for the `Center` `"Leipzig"`. There the first/last blood sample
#' taken on an intensive care unit was taken as timepoint for
#' admission/discharge (which is not necessarly part of the dataset).
#' That's why the first blood sample on an intensive care
#' unit could have a `SecToIcu` of zero in contrast to a negative value for
#' `Center` `"Greifswald"`. If needed this could be harmonized by adding the
#' first `SecToIcu` for an intensive care unit to all `SecToIcu` for each
#' `Episode` in the `"Greifswald"` and/or `"Leipzig"` subset.
#' An example could be found below.
#'
#' In a few cases there is a mismatch between the timepoint of
#' admission/discharge extracted from the clinical information system at
#' `Center` `"Greifswald"` and the entry in `Center`. It could happen that the
#' `Sender` is a non-ICU ward and the `SecToIcu` time is negative. According to
#' the admission data the patient was already on an ICU but the laboratory order
#' was taken from a non-ICU ward. Mostly this time difference is around a few
#' minutes (a blood sample was taken on the non-ICU ward, but the analysis in
#' the laboratory started after transfer to the ICU).
#'
#' @examples
#' ## remove duplicate laboratory entries (see text above for explanation)
#' greifswald <- subset(sbcdata, Center == "Greifswald")
#' dup <- duplicated(greifswald[, .(Id, Time)]) |
#'     duplicated(greifswald[, .(Id, Time)], fromLast = TRUE)
#' mean(dup)
#' greifswald <- greifswald[!dup,]
#'
#' ## adjust SecToIcu for subset Greifswald (see text above for explanation)
#' greifswald <- subset(sbcdata, Center == "Greifswald")
#' ## create helper columns
#' greifswald[, isNewWard := (
#'     c(FALSE, Id[-1] == Id[-.N]) &         # same case
#'     c(FALSE, Sender[-1] != Sender[-.N])   # new ward
#' )]
#' greifswald[, isIcuAdmission := isNewWard & grepl("ICU", Sender)]
#' ## recalculate SecToIcu
#' greifswald[, SecToIcu := SecToIcu - SecToIcu[isIcuAdmission][Episode]]
#' ## drop helper columns
#' greifswald[, `:=` (isNewWard = NULL, isIcuAdmission = NULL)]
"sbcdata"
