###############################################################################
## Please source the `2-ukg.R` first (needed for labdesc etc.) if the dataset
## should be regenerated.
###############################################################################

library("data.table")

## read output from Maria Schmidt
full <- as.data.table(readRDS(file.path("..", "intdata", "ukl_2019.rds")))

## rename columns
setnames(
    full,
    old = c("CaseId", "OrderTime"),
    new = c("Id", "Time")
)

## add missing Sepsis label for R57.2
r572 <- fread(
    file.path("..", "intdata", "ukl_faelle_r57.2_2014-2022.csv"),
    select = c("Fall  Medizinisch")
)

setnames(r572, "Id")

full$Diagnosis[full$Id %in% r572$Id] <- "Sepsis"

## remove entries where Sex is unknown
full <- full[Sex != "U",]

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
