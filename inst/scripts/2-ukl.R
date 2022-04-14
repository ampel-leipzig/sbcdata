###############################################################################
## Script to convert and anonymize internal dataset UKL.
###############################################################################

###############################################################################
## Please source the `1-umg.R` first (needed for labdesc etc.) if the dataset
## should be regenerated.
###############################################################################

library("data.table")

lab <- fread(
    file.path("..", "intdata", "ukl_labdata.csv"),
    na.strings = c("NA", "-1")
)

## rename columns
setnames(lab, gsub("B\\.|_[ECSZ]$", "", names(lab)))

adm <- fread(
    file.path("..", "intdata", "ukl_admdata.csv"),
    select = c(
        "ID_Patient", "ID_case", "ID_order", "Age", "Sex", "Timestamp",
        "Codesender"
    ),
    na.strings = c("NA", "-1")
)

## rename columns
setnames(
    adm,
    old = c("ID_Patient", "ID_case", "ID_order", "Timestamp", "Codesender"),
    new = c("PatientId", "Id", "OrderId", "Time", "Sender")
)

## combine datasets
full <- cbind(adm, lab)

## remove entries where Sex is unknown
full <- full[Sex != "U",]

icd <- fread(
    file.path("..", "intdata", "ukl_icddiagnoses.csv"),
    select = c("PAT", "FAL  MED", "DIA"),
    na.string = c("NA", "-1")
)

## rename columns
setnames(icd, c("PatientId", "Id", "Icd"))

## drop duplicates
icd <- unique(icd)

## collapse Icd column
icd[, Icd := paste(Icd, collapse = ";"), by = Id]

## drop duplicates
icd <- unique(icd)

## combine ICD diagnoses (merge will change the order, cbind has to be called
## first, cause the adm and lab files are equally ordered)
full <- merge(full, icd, by = c("PatientId", "Id"), all.x = TRUE)

## reorder
setorder(full, Id, Time, OrderId)

## rewrite diagnosis
## SIRS
## do we want to treat R65.0, R65.1 as sepsis?
full[, Diagnosis := ifelse(grepl("R65\\.[0-9]+", Icd), "SIRS", "Control")]
## Sepsis
## A02.1
## A20.7
## A22.7   
## A23.9   
## A24.1   
## A26.7   
## A32.7   
## A39.2, A39.3, A39.4   
## A40.0, A40.1, A40.2, A40.3, A40.8, A40.9   
## A41.0, A41.1, A41.2, A41.3, A41.4, A41.51, A41.52, A41.58, A41.8, A41.9 
## A42.7   
## B37.7   
## R57.2
##
## will overwrite some SIRS cases
full[, Diagnosis := ifelse(
    grepl(paste(
        "A02\\.1|A20\\.7|A22\\.7|A23\\.9|A24\\.1|A26\\.7|A32\\.7|A39\\.[2-4]",
        "A40\\.[0-3,8,9]|A41\\.[0-5,8-9][1,2,8]*|A42\\.7|B37\\.7|R57\\.2",
        sep = "|"
    ), Icd),
    "Sepsis", Diagnosis
)]

## drop now useless ICD column
full[, Icd := NULL]

## recode sender
sendermap <- fread(
    file.path("..", "intdata", "ukl_sendermap.csv"),
    na.strings = c("NA", "")
)

full[, Sender := sendermap$Type[match(Sender, sendermap$Sender)]]
full[, Sender := ifelse(is.na(Sender), "GEN", Sender)]

## recalc Age from days to years
full[, Age := trunc(Age / 365.25)]

## drop patients < 18 years
full <- full[Age >= 18,]

setorder(full, Id, Time, OrderId)

## helper columns for TargetIcu and SecToIcu calculation
full[, isNewWard := (
    c(FALSE, Id[-1] == Id[-.N]) &         # same case
    c(FALSE, Sender[-1] != Sender[-.N])   # new ward
)]
full[, `:=`(
    isIcuAdmission = isNewWard & grepl("ICU", Sender),
    isIcuDischarge = isNewWard & c(FALSE, grepl("ICU", Sender[-.N]))
)]
## fix if the first ward is an ICU
full[,
    isIcuAdmission := c(grepl("ICU", Sender[1]), isIcuAdmission[-1]),
    by = Id
]
full[,
    Episode := cumsum(isIcuDischarge) + 1L,
    by = Id
]
full[, `:=` (
        TimeAdmissionIcu = Time[isIcuAdmission][Episode],
        TargetIcu = Sender[isIcuAdmission][Episode]
), by = Id]

full[, SecToIcu := as.numeric(TimeAdmissionIcu) - as.numeric(Time)]

## reset time
full[,
    Time := as.numeric(Time) - as.numeric(Time[1L]),
    by = Id
]
full[, Time:= as.numeric(Time)]

labdesc <- fread(file.path("..", "extdata", "labcodes.csv"))

## generate new case ids and drop now useless columns
ids <- full$Id
set.seed(220216)
full[,
    `:=`(
        Id = match(ids, sample(unique(ids))),
        Center = "Leipzig",
        Set = "Training"
    )
]
setorder(full, Id, Time, Episode)
full <- full[,
    c("Id", "Age", "Sex", "Diagnosis", "Center", "Set",
      "Sender", "Episode", "Time",
      "TargetIcu", "SecToIcu", labdesc$Code), with = FALSE
]

## remove entries where all laboratory values are NA
allNa <- rowSums(!is.na(full[, labdesc$Code, with = FALSE])) == 0
full <- full[!allNa,]

## export data
saveRDS(full, file = "ukl.RDS")
