###############################################################################
## Please source the `2-ukg.R` first (needed for labdesc etc.) if the dataset
## should be regenerated.
###############################################################################

library("data.table")

## read output from Maria Schmidt
full <- as.data.table(readRDS(file.path("..", "intdata", "ukl_2019.rds")))
icd <- as.data.table(
    readRDS(file.path("..", "intdata", "ukl_caseid_all.icd_2019-2021.rds"))
)[, .(CaseId, all.ICD)]

## drop unknown Id column
full[, Id := NULL]

## rename columns
setnames(
    full,
    old = c("CaseId", "OrderTime"),
    new = c("Id", "Time")
)

setnames(
    icd,
    old = c("CaseId", "all.ICD"),
    new = c("Id", "Icd")
)

## remove entries where Sex is unknown
full <- full[Sex != "U",]

full <- merge(full, icd, by = "Id", all.x = TRUE, all.y = FALSE)

## rewrite diagnosis
full[, Diagnosis :=  "Control"]
## SIRS
## we treat R65.1 as sepsis below
full$Diagnosis[grepl("R65\\.[0-9]+", full$Icd)] <- "SIRS"
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
## R65.1
##
## will overwrite some SIRS cases
full$Diagnosis[grepl(paste(
    "A02\\.1|A20\\.7|A22\\.7|A23\\.9|A24\\.1|A26\\.7|A32\\.7|A39\\.[2-4]",
    "A40\\.[0-3,8,9]|A41\\.[0-5,8-9][1,2,8]*|A42\\.7|B37\\.7|R57\\.2|R65\\.1",
    sep = "|"
), full$Icd)] <- "Sepsis"

## reorder
setorder(full, Id, Time, OrderId)

## drop patients < 18 years
full <- full[Age >= 18,]

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

## get UKL2014-2019 dataset and avoid ID overwritting
ukl2019 <- readRDS("ukl.RDS")
maxId <- max(ukl2019$Id)

## generate new case ids and drop now useless columns
ids <- full$Id
set.seed(220414)
full[,
    `:=`(
        Id = match(ids, sample(unique(ids))) + maxId,
        Center = "Leipzig",
        Set = "Validation"
    )
]

labdesc <- fread(file.path("..", "extdata", "labcodes.csv"))

setorder(full, Id, Time, Episode)
full <- full[,
    c("Id", "Age", "Sex", "Diagnosis", "Center", "Set",
      "Sender", "Episode", "Time",
      "TargetIcu", "SecToIcu", labdesc$Code), with = FALSE
]

## remove entries where all laboratory values are NA
allNa <- rowSums(!is.na(full[, labdesc$Code, with = FALSE])) == 0
full <- full[!allNa,]

## combine datasets
full <- rbind(ukl2019, full)

## export data
saveRDS(full, file = "ukl.RDS")
