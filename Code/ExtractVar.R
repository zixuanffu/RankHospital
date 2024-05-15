rm(list = ls())
library(data.table)
library()
# labor input: according to solen
doctor <- c("EFFSAL_TOT", "EFFLIB_TOT")
infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE", "ETP_DIRINF")
aide_soignant <- c("ETP_AID")
non_med <- c("ETP_CAD", "ETP_DIR", "ETP_AUTADM")


dt_labor_all <- data.table()


for (i in seq(2016, 2022)) {
    dt_labor <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    if (i == 2022) {
        infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE", "ETP_DIRSOI")
    } else {
        infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE", "ETP_DIRINF")
    }
    labor <- c(doctor, infirmier, aide_soignant, non_med)
    cols <- c("AN", "FI", "FI_EJ", labor)
    dt_labor <- dt_labor[, ..cols]
    for (j in labor) {
        dt_labor[, (j) := as.numeric(get(j))]
    }
    if (i == 2022) {
        setnames(dt_labor, "ETP_DIRSOI", "ETP_DIRINF")
    }
    dt_labor_all <- rbind(dt_labor_all, dt_labor)
}
saveRDS(dt_labor_all, "Data/Out/labor_input_2016_2022.rds")


# capacity
cols_cap <- c("AN", "FI", "FI_EJ", "LIT_MCO", "PLA_MCO")
dt_cap_all <- data.table()
for (i in seq(2016, 2022)) {
    dt_cap <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    dt_cap <- dt_cap[, ..cols_cap]
    dt_cap_all <- rbind(dt_cap_all, dt_cap)
}
saveRDS(dt_cap_all, "Data/Out/capacity_2016_2022.rds")

# output
psy <- c("SEJ_HTP_TOT", "VEN_HDJ_TOT", "VEN_HDN_TOT")
cols_out <- c(
    "AN", "FI", "FI_EJ", "SEJHC_MCO", "JOU_MCO", "SEJHP_MCO", "PASSU_GEN", "PASSU_PED", "SEANCES_MED",
    psy, "SEJHC_SSR", "JOUHP_SSR", "ENT", "SEJ_HAD", "CONSULT_EXT"
)
dt_out_all <- data.table()
for (i in seq(2016, 2022)) {
    dt_out <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    dt_out <- dt_out[, ..cols_out]
    dt_out_all <- rbind(dt_out_all, dt_out)
}
saveRDS(dt_out_all, "Data/Out/output_2016_2022.rds")

dt_dummy <- data.table()
for (i in seq(2016, 2022)) {
    dt <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    dt[, `:=`(OPE = !(SEJHC_MCO > 0), PSY = (EFFSAL_PSY > 0 | EFFLIB_PSY > 0))]
    dt <- dt[, .(AN, FI, FI_EJ, OPE, PSY)]
    dt_dummy <- rbind(dt_dummy, dt)
}
saveRDS(dt_dummy, "Data/Out/dummy_2016_2022.rds")

# control
cols <- c("AN", "finess", "rs", "A7", "A9", "A10", "A11")
cols_new <- c("AN", "FI", "RS", "CANCER", "CASEMIX", "TEACHING", "RESEARCH")
year <- 2016:2022

control <- data.table()
for (i in seq_along(year)) {
    dt <- readRDS(paste0("Data/In/Hospidiag/hd_", year[i], ".rds"))
    dt <- dt[, ..cols]
    control <- rbind(control, dt)
}
setnames(control, cols, cols_new)
saveRDS(control, "Data/Out/control_2016_2022.rds")

# status

## according to https://lannuaire.service-public.fr/navigation/chu
## there are 32 CHU in France
## we will get their FI_EJ for each CHU by using 2016 data.
a <- readRDS(paste0("Data/In/ID/ID_", 2016, ".rds"))
colnames(a) <- toupper(colnames(a))
cols_id <- c("AN", "FI", "FI_EJ", "RS", "GRP", "STJ", "STJR", "CAT", "CATR", "NAT", "SIR", "REG", "DEP")
a <- a[, ..cols_id]
CHU <- a[CATR == "PUB1" & CAT == "101"]
CHU_FI_EJ <- unique(CHU$FI_EJ)

lookup <- data.table(STJR = c(0, 1, 2, 3), STJR_LABEL = c("CHU", "PUB", "PLU", "PNL"))

status <- data.table()
for (i in 2016:2022) {
    id <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    colnames(id) <- toupper(colnames(id))
    id <- id[, ..cols_id]
    id[FI_EJ %in% CHU_FI_EJ, STJR := 0]
    id[, STJR := as.numeric(STJR)]
    id[lookup, STJR_LABEL := i.STJR_LABEL, on = .(STJR)]
    status <- rbind(status, id)
}
saveRDS(status, "Data/Out/status_2016_2022.rds")

# combine control and status
control <- readRDS("Data/Out/control_2016_2022.rds")
status <- readRDS("Data/Out/status_2016_2022.rds")
cols <- colnames(control)[!colnames(control) %in% c("FI", "RS")]
for (i in cols) {
    control[, (i) := as.numeric(gsub(",", ".", get(i)))]
}
control_stat_23 <- status[STJR == 2 | STJR == 3][control, on = .(AN, FI), nomatch = 0]
control_stat_01 <- status[STJR == 0 | STJR == 1][control, on = .(AN, FI_EJ = FI), nomatch = 0]
control_stat <- rbind(control_stat_01, control_stat_23)
colnames(control_stat)
control_stat <- unique(control_stat, by = c("AN", "FI", "FI_EJ"))
saveRDS(control_stat, "Data/Out/control_stat_2016_2022.rds")

# extract those with stable legal status
status <- readRDS("Data/Out/status_2016_2022.rds")
stat_unique <- unique(status[, .(FI, FI_EJ, STJR)])
stat_unique <- stat_unique[, .N, by = .(FI)]
stat_unique <- stat_unique[N == 1]
fi <- stat_unique$FI
status <- unique(status[FI %in% fi, .(FI, FI_EJ, STJR, STJR_LABEL)])
saveRDS(status, "Data/Out/status_stable_2016_2022.rds")
