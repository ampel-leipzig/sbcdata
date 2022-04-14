###############################################################################
## Script to convert and anonymize internal dataset UMG.
###############################################################################

###############################################################################
## Laboratory Data
###############################################################################

library("data.table")

## import lab data
lab <- fread(
    file.path("..", "intdata", "umg_labdata_2015_2020_sepsis_20210707.csv")
)

## rename columns
setnames(
    lab,
    c("Id", "OrderId", "Sender", "Time", "Code", "Description", "Value", "Unit")
)

## drop useless order id
#lab$OrderId <- NULL

## rename pct_h_b20150609 to pct_h
lab$Code[lab$Code == "pct_h_b20150609"] <- "pct_h"

## keep just CBC (sysmex data) and CRP, PCT
lab <- lab[grepl("^[a-z]+_sys$|^crp_h|^pct_h.*$", lab$Code),]

## keep just hb, mcv, plt, rbc, wbc, crp, pct
lab <- lab[grepl("hb|mcv|plt|rbc|wbc|crp|pct", lab$Code),]

## rename codes
lab[, Code := toupper(gsub("_.*$", "", lab$Code))]
lab$Code[lab$Code == "HB"] <- "HGB"

## method description
labdesc <- unique(lab[, .(Code, Description, Unit)])
setorder(labdesc, Code)

labdesc[, Description := c(
    CRP = "C-reactive protein",
    HGB = "hemoglobin",
    MCV = "mean corpuscular volume",
    PCT = "procalcitonin",
    PLT = "platelets",
    RBC = "red blood count",
    WBC = "white blood count"
)[Code]]

## drop method description columns
lab[, `:=` (Description = NULL, Unit = NULL)]

## some values contain text like "not enough", "technical not possible", "error"
#table(lab$Value[!grepl("[0-9.]", lab$Value)])

## would be replaced by NA due to conversion
lab[, Value := as.numeric(Value)]

## drop NA
lab <- lab[!is.na(Value),]

## order by Id, and Time
setorder(lab, Id, Time, OrderId)

labsenderdesc <- data.frame(
    Sender = c("AMB",
               "GEN", "ED",
               "CICU", "CIMC",
               "MICU", "MIMC",
               "NICU", "NIMC",
               "OR",
               "PICU", "PIMC",
               "SICU", "SIMC",
               "STUD"),
    Description = c(
        "out patients",
        "general/normal ward",
        "emergency department",
        "combined intensive care",
        "combined intermediate care",
        "medical intensive care unit",
        "medical intermediate care",
        "neurological intensive care",
        "neurological intermediate care",
        "operation room",
        "pediatric intensive care unit",
        "pediatric intermediate care",
        "surgical intensive care unit",
        "surgical intermediate care",
        "study measurements"
    )
)

## recode sender
sendermap <- read.csv(
    file.path("..", "intdata", "umg_lab_sendermap.csv"),
    na.strings = c("NA", "")
)

lab[, Sender := sendermap$Type[match(Sender, sendermap$Sender)]]
lab$Sender[is.na(lab$Sender)] <- "OTHER"

## drop duplicated
lab <- unique(lab)

## there are ~ 3000 entries with duplicated time but different order id and
## different value, because we can't decide which one is correct we drop all
## of them
#lab <-
#    lab[!(duplicated(lab[c("Id", "Sender", "Time", "Code")]) |
#            duplicated(lab[c("Id", "Sender", "Time", "Code")], fromLast = TRUE)),]

## reshape fails due to memory limitation
lab <- dcast(lab, Id + OrderId + Time + Sender ~ Code, value.var = "Value")

###############################################################################
## Administration Data
###############################################################################

## import administration data
adm <- fread(
    file.path("..", "intdata", "umg_admdata_2015_2020_sepsis_20210921.csv")
)

## rename columns
setnames(adm, c(
    "Id",
    "DateAdmission", "DateDischarge",
    "WardAdmission", "WardDischarge",
    "Drg", "Age", "Sex", "Icd",
    paste(c(
            "Icu",
            "DateAdmissionIcu", "TimeAdmissionIcu",
            "DateDischargeIcu", "TimeDischargeIcu"
        ),
        rep(1:10, each = 5),
        sep = "."
    )
))

## drop useless columns
useless <-  c(
    "Drg",
    "DateAdmission", "DateDischarge",
    "WardAdmission", "WardDischarge",
    paste(
        c("DateAdmissionIcu", "DateDischargeIcu"),
        rep(1:10, each = 2),
        sep = "."
    )
)
adm <- adm[, setdiff(names(adm), useless), with = FALSE]

## recode sex
adm[, Sex := c("M", "W")[Sex]]

## remove duplicated entries - 4850 entries
adm <- unique(adm)

## 42 patient ids are still duplicated but differ in some values
## (DateDischarge and Age), drop them
adm <- adm[!(duplicated(Id) | duplicated(Id, fromLast = TRUE)),]

## last entry is just NA (may be a conversion problem with libreoffice)
adm <- adm[!is.na(adm$Id),]

## reshape to long format
adm <- reshape(
    adm, varying = 5:34, idvar = "Id", timevar = "Episode", direction = "long"
)

## convert time
adm[, `:=`(
    TimeAdmissionIcu = as.POSIXct(
        TimeAdmissionIcu, tz = "UTC", format = "%d.%m.%Y %H:%M:%S"
    ),
    TimeDischargeIcu = as.POSIXct(
        TimeDischargeIcu, tz = "UTC", format = "%d.%m.%Y %H:%M:%S"
    )
)]

## order by Id
setorder(adm, Id, Episode)

## remove entries without ICU admission (empty Episodes) except the first
## (never on ICU)
adm <- adm[adm$Episode == 1 | nchar(adm$Icu),]

## remove now useless Episode column
adm[, Episode := NULL]

## reorder by Id, Admission
setorder(adm, Id, TimeAdmissionIcu)

## recode sender
icumap <- read.csv(file.path("..", "intdata", "umg_adm_icumap.csv"))

adm[, Icu := icumap$Type[match(Icu, icumap$Icu)]]

## clean icu
adm$Icu[!nchar(adm$Icu)] <- NA

## clean icd
adm$Icd[!nchar(adm$Icd)] <- NA

## rewrite diagnosis
adm[, Diagnosis :=  "Control"]
## SIRS
## do we want to treat R65.0, R65.1 as sepsis?
adm$Diagnosis[grepl("R65\\.[0-9]+", adm$Icd)] <- "SIRS"
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
adm$Diagnosis[grepl(paste(
    "A02\\.1|A20\\.7|A22\\.7|A23\\.9|A24\\.1|A26\\.7|A32\\.7|A39\\.[2-4]",
    "A40\\.[0-3,8,9]|A41\\.[0-5,8-9][1,2,8]*|A42\\.7|B37\\.7|R57\\.2",
    sep = "|"
), adm$Icd)] <- "Sepsis"

## drop now useless ICD column
adm[, Icd := NULL]

## combine datasets (just keep lab values with entry in adm data)
lab <- lab[lab$Id %in% adm$Id,]
adm <- adm[adm$Id %in% lab$Id,]
# mean(lab$Id %in% adm$Id) [1] 0.9998121 # there are still some entries from
# foreign centers left
# mean(adm$Id %in% lab$Id) # [1] 0.9189129

## perform non-equi join to get sender id, temporary columns needed
adm[, `:=`(
    jTimeAdmissionIcu = TimeAdmissionIcu,
    jTimeDischargeIcu = TimeDischargeIcu
)]
lab[, jTime := Time]

## this merges just entries with an given TimeAdmissionIcu/TimeDischargeIcu
lab <- adm[
    lab,
    on = .(Id, jTimeAdmissionIcu <= jTime, jTimeDischargeIcu > jTime)
]
## drop temporary columns
lab[, `:=`(jTimeAdmissionIcu = NULL, jTimeDischargeIcu = NULL)]

## additionally merge Age, Sex, Diagnosis for non ICU (and ICU again)
lab[, `:=` (Age = NULL, Sex = NULL, Diagnosis = NULL)]

lab <- merge(
    lab, unique(adm[, .(Id, Age, Sex, Diagnosis)]),
    by = "Id", all.x = TRUE, all.y = FALSE
)

setorder(lab, Id, Time, OrderId)

## helper columns for TargetIcu and SecToIcu calculation
lab[, isNewWard := (
    c(FALSE, Id[-1] == Id[-.N]) &         # same case
    c(FALSE, Sender[-1] != Sender[-.N])   # new ward
)]
lab[, `:=`(
    isIcuAdmission = isNewWard & grepl("ICU", Sender),
    isIcuDischarge = isNewWard & c(FALSE, grepl("ICU", Sender[-.N]))
)]
## fix if the first ward is an ICU
lab[,
    isIcuAdmission := c(grepl("ICU", Sender[1]), isIcuAdmission[-1]),
    by = Id
]
lab[,
    Episode := cumsum(isIcuDischarge) + 1L,
    by = Id
]
lab[, `:=` (
        TimeAdmissionIcu = TimeAdmissionIcu[isIcuAdmission][Episode],
        TargetIcu = Sender[isIcuAdmission][Episode]
), by = Id]

lab[, SecToIcu := as.numeric(TimeAdmissionIcu) - as.numeric(Time)]

## reset time
lab[,
    Time := as.numeric(Time) - as.numeric(Time[1L]),
    by = Id
]
lab[, Time:= as.numeric(Time)]

## generate new case ids and drop now useless columns
ids <- lab$Id
set.seed(220216)
lab[,
    `:=`(
        Id = match(ids, sample(unique(ids))),
        Center = "Greifswald",
        Set = "Validation"
    )
]
setorder(lab, Id, Time, Episode)
lab <- lab[,
    c("Id", "Age", "Sex", "Diagnosis", "Center", "Set",
      "Sender", "Episode", "Time",
      "TargetIcu", "SecToIcu", labdesc$Code), with = FALSE
]

## export data
write.csv(
    labdesc,
    file.path("..", "extdata", "labcodes.csv"),
    row.names = FALSE
)
write.csv(
    labsenderdesc,
    file.path("..", "extdata", "sendercodes.csv"),
    row.names = FALSE
)
saveRDS(lab, file = "umg.RDS")
