#' Import and Format MIMIC-IV
#'
#' Import and format MIMIC-IV dataset as done in the AMPEL project.
#'
#' @param path `character(1)`, path to the root MIMIC-IV folder (that contains
#' the subfolders: core and hosp).
#' @param verbose `logical(1)`, if `TRUE` progress messages are shown.
#' @return `data.frame`, same as `x` but with an added column `Excluded` and
#' 3 attributes `"message"`, `"n_cases"`, `"n_cbc"` that describe the
#' processing and the kept cases and complete blood counts (CBC)
#' @details
#' For the MIMIC-IV dataset the files `core/patients.csv`, `core/transfers.csv`,
#' `hosp/labevents.csv`, and `hosp/diagnoses_icd.csv` are needed.
#' @author Daniel Steinbach <daniel.steinbach@@medizin.uni-leipzig.de> and
#' Sebastian Gibb <mail@@sebastiangibb.de>
#' @references
#' - Johnson, A., Bulgarelli, L., Pollard, T., Horng, S., Celi, L. A., & Mark, R.
#' (2021). MIMIC-IV (version 1.0). PhysioNet. \doi{10.13026/s6n6-xd98}.
#' - Documentation \url{https://mimic.mit.edu/docs/iv/}
#' @export
#' @examples
#' \dontrun{
#' mimic <- import_mimic("path/to/mimic-iv-1.0")
#' }
import_mimic <- function(path, verbose = interactive()) {
    if (verbose)
        message("Reading labevents")

    labevents <- fread(
        file.path(path, "hosp", "labevents.csv"),
        select = c(
            "subject_id", "hadm_id", "itemid", "charttime", "valuenum"
        )
    )

    if (verbose)
        message("Preprocess labevents")

    ## drop events without laboratory measurements
    labevents <- labevents[!is.na(valuenum), ]

    labevents <- labevents[
        labevents$itemid %in% c(
            51301, # White Blood Cells, K/uL, Blood, Hematology, LOINC 804-5
            51279, # Red Blood Cells, m/uL, Blood, Hematology, LOINC 789-8
            51265, # Platelet Count, K/uL, Blood, Hematology, LOINC 777-3
            51222, # Hemoglobin, g/dL, Blood, Hematology, LOINC 718-7,
            51250  # MCV, fL, Blood, Hematology, LOINC 787-2
        ),
    ]

    ## long to wide
    labevents <- unique(
        labevents,
        by = c("subject_id", "hadm_id", "charttime", "itemid")
    )
    labevents <- dcast(
        labevents,
        subject_id + hadm_id + charttime ~ itemid,
        value.var = "valuenum"
    )

    ## rename columns
    setnames(labevents,
        c("51301", "51279", "51265", "51222", "51250"),
        c("WBC",   "RBC",   "PLT",   "HGB",   "MCV")
    )

    ## convert HGB to mmol/l
    labevents[, HGB := HGB * 0.621]

    if (verbose)
        message("Reading transfers")

    ## we rely on core/transfers because icu/icustays not necessarily
    ## contain all ICU stays (~ 5%)
    ## https://github.com/MIT-LCP/mimic-code/issues/1165
    transfers <- fread(file.path(path, "core", "transfers.csv"))

    if (verbose)
        message("Preprocess transfers")

    setorder(transfers, subject_id, intime)
    transfers <- transfers[eventtype != "discharge",]

    ## recode sender
    icumap <- fread(system.file(
        "extdata", "mimic-iv-1.0", "icumap.csv",
        package = "sbcdata", mustWork = TRUE
    ))
    transfers[, Sender := icumap$Type[match(careunit, icumap$Icu)]]

    ## some ICU stays have no labevents and got lost in the subsequent non-equi
    ## join, we temporary add a fake labevent in the middle of the icustay to
    ## ensure the icustay is included.
    fakelabevents <-
        transfers[grepl("ICU", Sender), .(subject_id, intime, outtime)]
    fakelabevents[, charttime := intime + ((outtime - intime) / 2)]
    ## make fake labevents by -Inf
    fakelabevents[, `:=` (HGB = -Inf, intime = NULL, outtime = NULL)]

    labevents <- rbind(labevents, fakelabevents, fill = TRUE)
    setorder(labevents, subject_id, charttime)

    if (verbose)
        message("Combine labevents and transfers")

    ## perform non-equi join to get sender id, temporary columns needed
    transfers[, `:=`(jintime = intime, jouttime = outtime)]
    labevents[, jcharttime := charttime]

    labevents <- transfers[
        labevents,
        on = .(subject_id, jintime <= jcharttime, jouttime > jcharttime)
    ]
    ## drop temporary columns
    labevents[, `:=`(jintime = NULL, jouttime = NULL, i.hadm_id = NULL)]

    ## treat all patients without a identified sender as general ward.
    ## among others this include out of hospital patients without any
    ## careunit/intime/outtime
    labevents$Sender[is.na(labevents$Sender)] <- "GEN"

    if (verbose)
        message("Reading patients")

    patients <- fread(
        file.path(path, "core", "patients.csv"),
        drop = c("anchor_year_group", "dod")
    )
    patients[, Sex := ifelse(gender == "F", "W", "M")]

    ## recalculate age
    patients[, Birth := anchor_year - anchor_age]

    if (verbose)
        message("Combine labevents and patients")

    labevents <- merge(
        labevents, patients[, .(subject_id, Sex, Birth)], by = "subject_id",
        all.x = TRUE, all.y = FALSE
    )
    labevents[, Age := year(charttime) - Birth]
    labevents[, Birth := NULL]

    ## drop children
    labevents <- labevents[Age >= 18,]

    if (verbose)
        message("Reading diagnoses_icd")

    diagnoses_icd <- fread(
        file.path(path, "hosp", "diagnoses_icd.csv"),
        select = c("subject_id", "hadm_id", "icd_code")
    )

    ## filter ICD-9 and ICD-10 sepsis codes
    diagnoses_icd[, SIRS := grepl("99590|R65\\.[0-9]+", diagnoses_icd$icd_code)]
    diagnoses_icd[, Sepsis := diagnoses_icd$icd_code %in% c(
        "99591", "99592",
        "A021", "A207", "A227", "A241", "A267", "A327", "A392", "A393", "A394",
        "A40", "A400", "A401", "A403", "A408", "A409", "A41", "A410", "A4101",
        "A4102", "A411", "A412", "A413", "A414", "A4150", "A4151", "A4152",
        "A4153", "A4159", "A418", "A4181", "A4189", "A419", "A427", "B377")
    ]
    sirs_sepsis <- diagnoses_icd[,
        .(SIRS = max(SIRS), Sepsis = max(Sepsis)),
        by = c("subject_id", "hadm_id")
    ]

    ## Code Control as 0, SIRS as 1, an Sepsis as 2, SIRS would be overwritten
    ## by Sepsis
    sirs_sepsis[,
        Diagnosis :=
            c("Control", "SIRS", "Sepsis")[1 + SIRS + (2 - SIRS) * Sepsis]
    ]
    sirs_sepsis[, `:=`(SIRS = NULL, Sepsis = NULL)]

    if (verbose)
        message("Combine labevents and diagnoses_icd")

    labevents <- merge(
        labevents, sirs_sepsis, by = c("subject_id", "hadm_id"),
        all.x = TRUE, all.y = FALSE
    )

    ## assume Control for missing Diagnosis
    labevents$Diagnosis[is.na(labevents$Diagnosis)] <- "Control"

    if (verbose)
        message("Calculate Time to ICU")

    setorder(labevents, subject_id, charttime)

    ## helper columns for TargetIcu and SecToIcu calculation
    labevents$transfer_id[is.na(labevents$transfer_id)] <- 0
    labevents[,
        isNewWard := (
            ## same patient
            c(FALSE, subject_id[-1] == subject_id[-.N]) &
            ## same case
            c(
                FALSE,
                !is.na(hadm_id[-1]) & !is.na(hadm_id[-.N]) &
                hadm_id[-1] == hadm_id[-.N]
            ) &
            ## new ward
            c(FALSE, transfer_id[-1] != transfer_id[-.N])
        )
    ]
    labevents[, `:=`(
        isIcuAdmission = isNewWard & grepl("ICU", Sender),
        isIcuDischarge = isNewWard & c(FALSE, grepl("ICU", Sender[-.N]))
    )]
    ## fix if the first ward is an ICU
    labevents[,
        isIcuAdmission := c(grepl("ICU", Sender[1]), isIcuAdmission[-1]),
        by = subject_id
    ]
    labevents[,
        Episode := cumsum(isIcuDischarge) + 1L,
        by = c("subject_id", "hadm_id")
    ]
    labevents[, `:=` (
            IcuAdmissionTime = intime[isIcuAdmission][Episode],
            TargetIcu = Sender[isIcuAdmission][Episode]
        ),
        by = c("subject_id", "hadm_id")
    ]
    labevents[,
        SecToIcu := as.numeric(IcuAdmissionTime) - as.numeric(charttime)
    ]

    ## drop temporary introduced fake labevents
    labevents <- labevents[!is.infinite(HGB),]

    if (verbose)
        message("Anomynise data")

    ## anomynise similar to UMG/UKL
    labdesc <- fread(
        system.file("extdata", "labcodes.csv", package = "sbcdata")
    )

    ## reset time
    labevents[,
        Time := as.numeric(charttime) - as.numeric(charttime[1L]),
        by = c("subject_id", "hadm_id")
    ]

    ## generate new case ids and drop now useless columns
    ids <- paste(labevents$subject_id, labevents$hadm_id, sep = ":")
    labevents[,
        `:=`(
            Id = match(ids, unique(ids)),
            Center = "MIMIC-IV" # or should we use Boston here?
        )
    ]

    ## add missing CRP/PCT columns
    labevents[, `:=` (CRP = NA_real_, PCT = NA_real_)]

    labevents[,
       c("Id", "Age", "Sex", "Diagnosis", "Center", "Sender", "Episode", "Time",
         "TargetIcu", "SecToIcu", labdesc$Code), with = FALSE
    ]
}
