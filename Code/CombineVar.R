rm(list = ls())
library(data.table)

# ---- 2016-2022 ----
# get input
input <- readRDS("Data/Out/labor_input_2016_2022.rds")
colnames(input)
input[, `:=`(AN = as.numeric(AN))]
setkey(input, AN, FI, FI_EJ)
# input_ETP<-readRDS("Data/Out/labor_input_2016_2022.rds")

# get capacity
capacity <- readRDS("Data/Out/capacity_2016_2022.rds")
colnames(capacity)
capacity[, `:=`(AN = as.numeric(AN))]
setkey(capacity, AN, FI, FI_EJ)
# get output
output <- readRDS("Data/Out/output_2016_2022.rds")
colnames(output)
output[, `:=`(AN = as.numeric(AN))]
setkey(output, AN, FI, FI_EJ)
# get dummy
dummy <- readRDS("Data/Out/dummy_2016_2022.rds")
colnames(dummy)
dummy[, `:=`(AN = as.numeric(AN))]
setkey(dummy, AN, FI, FI_EJ)

## control and status together
control_status <- readRDS("Data/Out/control_stat_2016_2022.rds")

# let's merge the dataset.
dt <- input[output, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[capacity, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[dummy, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[control_status, on = .(AN, FI, FI_EJ), nomatch = 0]

colnames(dt)

dt[is.na(dt)] <- 0
dt <- dt[AN != 2020]
dt[, EFF_MD := EFFSAL_TOT + EFFLIB_TOT]
dt[, ETP_INF := ETP_INFAVECSPE + ETP_INFSANSSPE + ETP_DIRINF]
dt[, ETP_NONMED := ETP_CAD + ETP_DIR + ETP_AUTADM]
dt[, VEN_TOT := VEN_HDJ_TOT + VEN_HDN_TOT]
dt[, ENTSSR := ENT + SEJHC_SSR]
dt[, PASSU := PASSU_PED + PASSU_GEN]
colnames(dt)
saveRDS(dt, "Data/Out/combineddata_2016_2022.rds")

# ---- 2013-2015 ----
# Extend the dataset to 2023
input <- readRDS("Data/Out/labor_input_2013_2015.rds")
output <- readRDS("Data/Out/output_2013_2015.rds")
capacity <- readRDS("Data/Out/capacity_2013_2015.rds")
psy <- readRDS("Data/Out/PSY_2013_2015.rds")
passu <- readRDS("Data/Out/PASSU_2013_2015.rds")
ssr <- readRDS("Data/Out/SSR_2013_2015.rds")
usld <- readRDS("Data/Out/USLD_2013_2015.rds")
had <- readRDS("Data/Out/HAD_2013_2015.rds")

control_status <- readRDS("Data/Out/control_stat_2013_2015.rds")

# merge
dt <- input[output, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[capacity, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[psy, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[passu, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[ssr, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[usld, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[had, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[control_status, on = .(AN, FI, FI_EJ), nomatch = 0]
dt[is.na(dt)] <- 0
dt[, EFF_MD := EFFSAL_TOT + EFFLIB_TOT]
dt[, ETP_INF := ETP_INFAVECSPE + ETP_INFSANSSPE + ETP_DIRINF]

dt[, VEN_TOT := VEN_HDJ_TOT + VEN_HDN_TOT]
dt[, PASSU := PASSU_PED + PASSU_GEN]
dt[, ENTSSR := ENT + SEJHC_SSR + JOUHP_SSR]

saveRDS(dt, "Data/Out/combineddata_2013_2015.rds")
pacman::p_load(knitr)
VAR <- c("AN", "FI", "FI_EJ", "STJR", "ETP_INF", "EFF_MD", "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "ENTSSR", "SEJ_HAD", "LIT_MCO", "PLA_MCO", "CASEMIX", "CANCER", "TEACHING", "RESEARCH")
var <- c("YEAR", "ID1", "ID2", "STATUS", "NURSES", "DOCTORS", "INPATIENT", "OUTPATIENT", "SESSIONS", "CONSULTATIONS", "EMERGENCY", "PSY_OUT", "PSY_IN", "REHAB&LTAC", "HOME", "BEDS", "SLOTS", "CASEMIX", "CANCER", "TEACHING", "RESEARCH")
tb <- data.table(Variable = VAR, Label = var)
md_tb <- kable(tb, format = "markdown")
writeLines(md_tb, "Notes/Paper/VarTable.md")
