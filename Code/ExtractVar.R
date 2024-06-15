rm(list = ls())
pacman::p_load(data.table)

# ---- Variable Extraction for years 2016-2022 ---- #

# ---- Extract labor input: according to Solen
doctor <- c("EFFSAL_TOT", "EFFLIB_TOT")
infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE", "ETP_DIRINF")
aide_soignant <- c("ETP_AID")
non_med <- c("ETP_CAD", "ETP_DIR", "ETP_AUTADM")

dt_labor_all <- data.table() # create an empty data.table to stack all years

for (i in seq(2016, 2022)) { # seq(2016, 2022) is the same as 2016:2022
    dt_labor <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds")) # read from the SYGEN data
    if (i == 2022) { # a change of name in 2022
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
    if (i == 2022) { # to make the names consistent with the previous years
        setnames(dt_labor, "ETP_DIRSOI", "ETP_DIRINF")
    }
    # correct the missing FI_EJ
    stat <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    colnames(stat) <- toupper(colnames(stat))
    dt_labor <- merge(dt_labor, stat[, .(FI, FI_EJ)], by = "FI", all.x = TRUE)
    setnames(dt_labor, "FI_EJ.y", "FI_EJ")
    dt_labor_all <- rbind(dt_labor_all, dt_labor)
}
saveRDS(dt_labor_all, "Data/Out/labor_input_2016_2022.rds")


# ---- Extract capacity ---- #
cols_cap <- c("AN", "FI", "FI_EJ", "LIT_MCO", "PLA_MCO")
dt_cap_all <- data.table()
for (i in seq(2016, 2022)) {
    dt_cap <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    dt_cap <- dt_cap[, ..cols_cap]

    stat <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    colnames(stat) <- toupper(colnames(stat))
    dt_cap <- merge(dt_cap, stat[, .(FI, FI_EJ)], by = "FI", all.x = TRUE)
    setnames(dt_cap, "FI_EJ.y", "FI_EJ")

    dt_cap_all <- rbind(dt_cap_all, dt_cap)
}
saveRDS(dt_cap_all, "Data/Out/capacity_2016_2022.rds")

# ---- Extract output ---- #
psy <- c("SEJ_HTP_TOT", "VEN_HDJ_TOT", "VEN_HDN_TOT")
cols_out <- c(
    "AN", "FI", "FI_EJ", "SEJHC_MCO", "SEJHP_MCO", "PASSU_GEN", "PASSU_PED", "SEANCES_MED",
    psy, "SEJHC_SSR", "JOUHP_SSR", "ENT", "SEJ_HAD", "CONSULT_EXT"
)
dt_out_all <- data.table()
for (i in seq(2016, 2022)) {
    dt_out <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    dt_out <- dt_out[, ..cols_out]

    stat <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    colnames(stat) <- toupper(colnames(stat))
    dt_out <- merge(dt_out, stat[, .(FI, FI_EJ)], by = "FI", all.x = TRUE)
    setnames(dt_out, "FI_EJ.y", "FI_EJ")

    dt_out_all <- rbind(dt_out_all, dt_out)
}
saveRDS(dt_out_all, "Data/Out/output_2016_2022.rds")

# ---- Extract dummy variables ---- #
dt_dummy <- data.table()
for (i in seq(2016, 2022)) {
    dt <- readRDS(paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
    dt[, `:=`(OPE = !(SEJHC_MCO > 0), PSY = (EFFSAL_PSY > 0 | EFFLIB_PSY > 0))]
    dt <- dt[, .(AN, FI, FI_EJ, OPE, PSY)]

    stat <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    colnames(stat) <- toupper(colnames(stat))
    dt <- merge(dt, stat[, .(FI, FI_EJ)], by = "FI", all.x = TRUE)
    setnames(dt, "FI_EJ.y", "FI_EJ")

    dt_dummy <- rbind(dt_dummy, dt)
}
saveRDS(dt_dummy, "Data/Out/dummy_2016_2022.rds")

# ---- Extract control variables ---- #
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

# ---- Extract legal status ---- #

## according to https://lannuaire.service-public.fr/navigation/chu
## there are 32 CHU in France
## we will get their FI_EJ for each CHU by using 2016 data
## STJR=0 for CHU, STJR=1 for PUB, STJR=2 for PLU, STJR=3 for PNL

a <- readRDS(paste0("Data/In/ID/ID_", 2016, ".rds"))
colnames(a) <- toupper(colnames(a))
cols_id <- c("AN", "FI", "FI_EJ", "RS", "GRP", "STJ", "STJR", "CAT", "CATR", "NAT", "SIR", "REG", "DEP")
a <- a[, ..cols_id]
CHU <- a[CATR == "PUB1" & CAT == "101"] # CHU have CATR = "PUB1" and CAT = "101"
CHU_FI_EJ <- unique(CHU$FI_EJ) # get the FI_EJ of CHU

lookup <- data.table(STJR = c(0, 1, 2, 3), STJR_LABEL = c("CHU", "PUB", "PLU", "PNL")) # lookup table for STJR

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

# ---- Combine control and status ---- #
# Because the control variable dataset use FI for private hospitals and use FI_EJ for public hospitals.
control <- readRDS("Data/Out/control_2016_2022.rds")
status <- readRDS("Data/Out/status_2016_2022.rds")
cols <- colnames(control)[!colnames(control) %in% c("FI", "RS")]
for (i in cols) {
    control[, (i) := as.numeric(gsub(",", ".", get(i)))]
}
control_stat_23 <- merge(status[STJR == 2 | STJR == 3], control, by = c("AN", "FI"), all.x = TRUE)
control_stat_01 <- merge(status[STJR == 0 | STJR == 1], control, by.x = c("AN", "FI_EJ"), by.y = c("AN", "FI"), all.x = TRUE)
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


# ---- Variable Extraction for years 2013-2015 ---- #

# Extend to 2013 but only for variables that are going to be used.
# There's no SYGEN data for 2013-2015, we need to manually aggregate them from various sheets

# ---- Extract labor input ---- #
# ---- locate the data ---- #
# doctors q20
# nurses q23 ETPPNM N2100 (ETP_DIRINF) 2200 (ETP_INFSANSSPE) 2300 (ETP_INFAVECSPE)
# aidesoignant q23 ETPPNM N2500 (ETP_AID)
# non_med q23 ETPPNM N1000
# ---- aggregation ---- #
# N1000=N1000
# N2100=N2120+N2130
# N2200=N2210+N2220
# N2300=N2310+N2320+N2340
# N2500=N2510+N2520+N2530
dt_input_all <- data.table()
cols_doctor <- c("AN", "FI", "FI_EJ", "EFFSAL", "EFFLIB")
perso_id <- c("N1000", "N2100", "N2200", "N2300", "N2500")
perso_names <- c("ETP_ADM", "ETP_DIRINF", "ETP_INFSANSSPE", "ETP_INFAVECSPE", "ETP_AID")
for (i in seq(2013, 2015)) {
    dt <- readRDS(paste0("Data/In/Q23/Q23_", i, ".rds"))
    dt <- dt[, c("AN", "FI", "FI_EJ", "PERSO", "ETPPNM")]
    dt[, ETPPNM := as.numeric(ETPPNM)]
    dt <- dcast(dt, AN + FI + FI_EJ ~ PERSO, value.var = "ETPPNM")
    dt[is.na(dt)] <- 0
    dt[, `:=`(N2100 = N2120 + N2130, N2200 = N2210 + N2220, N2300 = N2310 + N2320 + N2340, N2500 = N2510 + N2520 + N2530)]
    setnames(dt, perso_id, perso_names)
    cols <- c("AN", "FI", "FI_EJ", perso_names)
    dt <- dt[, ..cols]
    setkey(dt, AN, FI, FI_EJ)

    dt_doctor <- readRDS(paste0("Data/In/Q20/Q20_", i, ".rds"))
    dt_doctor <- dt_doctor[PERSO == "M9999", ..cols_doctor]
    setnames(dt_doctor, c("EFFSAL", "EFFLIB"), c("EFFSAL_TOT", "EFFLIB_TOT"))
    dt_doctor[is.na(dt_doctor)] <- 0
    setkey(dt_doctor, AN, FI, FI_EJ)

    dt_input <- dt[dt_doctor, on = .(AN, FI, FI_EJ)]
    dt_input_all <- rbind(dt_input_all, dt_input)
}
saveRDS(dt_input_all, "Data/Out/labor_input_2013_2015.rds")

# ---- Extract capacity ---- #
cols_cap <- c("AN", "FI", "FI_EJ", "LIT_MCO", "PLA_MCO")
dt_cap_all <- data.table()
for (i in seq(2013, 2015)) {
    dt_cap <- readRDS(paste0("Data/In/MCO/MCO_", i, ".rds"))
    dt_cap <- dt_cap[, ..cols_cap]
    dt_cap_all <- rbind(dt_cap_all, dt_cap)
}
saveRDS(dt_cap_all, "Data/Out/capacity_2013_2015.rds")

# ---- Extract basic output ---- #
cols_out <- c("AN", "FI", "FI_EJ", "SEJHC_MCO", "SEJHP_MCO", "SEA_MCO", "PCON")
dt_out_all <- data.table()
for (i in seq(2013, 2015)) {
    dt_out <- readRDS(paste0("Data/In/MCO/MCO_", i, ".rds"))
    dt_out <- dt_out[, ..cols_out]
    setnames(dt_out, c("SEA_MCO", "PCON"), c("SEANCES_MED", "CONSULT_EXT"))
    dt_out_all <- rbind(dt_out_all, dt_out)
}
saveRDS(dt_out_all, "Data/Out/output_2013_2015.rds")

# ---- Extract PASSU,  PSY, SSR, USLD, HAD,---- #
start <- 2013
end <- 2015

table_name <- "URGENCES2"
cols <- c("AN", "FI", "FI_EJ", "URG", "PASSU")
dt_all <- data.table()
for (i in start:end) {
    dt <- readRDS(paste0("Data/In/", table_name, "/", table_name, "_", i, ".rds"))
    colnames(dt)
    dt <- dt[, ..cols]
    dt <- dcast(dt, AN + FI + FI_EJ ~ URG, value.var = "PASSU")
    dt[is.na(dt)] <- 0
    dt_all <- rbind(dt_all, dt)
}
setnames(dt_all, c("GEN", "PED"), c("PASSU_GEN", "PASSU_PED"))
saveRDS(dt_all, "Data/Out/PASSU_2013_2015.rds")

table_name <- "PSY"
cols <- c("AN", "FI", "FI_EJ", "VEN_HDJ", "VEN_HDN", "SEJ_HTP")
dt_all <- data.table()
for (i in start:end) {
    dt <- readRDS(paste0("Data/In/", table_name, "/", table_name, "_", i, ".rds"))
    stat <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    dt <- dt[DIS == "TOT", ..cols]

    # correct the missing FI_EJ
    dt <- merge(dt, stat[, .(FI, FI_EJ, RS)], by = "FI", all.x = TRUE)
    setnames(dt, "FI_EJ.y", "FI_EJ")

    dt[is.na(dt)] <- 0
    setnames(dt, c("VEN_HDJ", "VEN_HDN", "SEJ_HTP"), c("VEN_HDJ_TOT", "VEN_HDN_TOT", "SEJ_HTP_TOT"))
    dt_all <- rbind(dt_all, dt)
}
saveRDS(dt, "Data/Out/PSY_2013_2015.rds")


table_name <- "SSR"
dt <- readRDS(paste0("Data/In/", table_name, "/", table_name, "_", 2013, ".rds"))
cols <- c("AN", "FI", "FI_EJ", "SEJHC", "JOUHP")
dt_all <- data.table()
for (i in start:end) {
    dt <- readRDS(paste0("Data/In/", table_name, "/", table_name, "_", i, ".rds"))
    dt <- dt[GDE == "SSR_TOT", ..cols]
    dt[is.na(dt)] <- 0
    setnames(dt, c("SEJHC", "JOUHP"), c("SEJHC_SSR", "JOUHP_SSR"))
    dt_all <- rbind(dt_all, dt)
}
dt_all <- unique(dt_all, by = c("AN", "FI", "FI_EJ"))
saveRDS(dt_all, "Data/Out/SSR_2013_2015.rds")

table_name <- "USLD"
cols <- c("AN", "FI", "FI_EJ", "ENT")
dt_all <- data.table()
for (i in start:end) {
    dt <- readRDS(paste0("Data/In/", table_name, "/", table_name, "_", i, ".rds"))
    dt <- dt[, ..cols]
    dt[is.na(dt)] <- 0
    dt_all <- rbind(dt_all, dt)
}
saveRDS(dt_all, "Data/Out/USLD_2013_2015.rds")

table_name <- "HAD"
cols <- c("AN", "FI", "FI_EJ", "SEJ_HAD")
dt_all <- data.table()
for (i in start:end) {
    dt <- readRDS(paste0("Data/In/", table_name, "/", table_name, "_", i, ".rds"))
    dt <- dt[, ..cols]
    dt[is.na(dt)] <- 0
    dt_all <- rbind(dt_all, dt)
}
saveRDS(dt_all, "Data/Out/HAD_2013_2015.rds")


# psy <- c("SEJ_HTP_TOT", "VEN_HDJ_TOT", "VEN_HDN_TOT")
# cols_out <- c(
#     "AN", "FI", "FI_EJ", "SEJHC_MCO", "JOU_MCO", "SEJHP_MCO", "PASSU_GEN", "PASSU_PED", "SEANCES_MED",
#     psy, "SEJHC_SSR", "JOUHP_SSR", "ENT", "SEJ_HAD", "CONSULT_EXT"
# )


# ---- Extract control ---- #
cols <- c("AN", "finess", "rs", "A7", "A9", "A10", "A11")
cols_new <- c("AN", "FI", "RS", "CANCER", "CASEMIX", "TEACHING", "RESEARCH")
year <- 2013:2015
control <- data.table()
for (i in seq_along(year)) {
    dt <- readRDS(paste0("Data/In/Hospidiag/hd_", year[i], ".rds"))
    dt <- dt[, ..cols]
    control <- rbind(control, dt)
}
setnames(control, cols, cols_new)
saveRDS(control, "Data/Out/control_2013_2015.rds")

# ---- Extract legal status ---- #
a <- readRDS(paste0("Data/In/ID/ID_", 2016, ".rds"))
colnames(a) <- toupper(colnames(a))
cols_id <- c("AN", "FI", "FI_EJ", "RS", "GRP", "STJ", "STJR", "CAT", "CATR", "NAT", "SIR", "REG", "DEP")
a <- a[, ..cols_id]
CHU <- a[CATR == "PUB1" & CAT == "101"]
CHU_FI_EJ <- unique(CHU$FI_EJ)
# Centre hospitalier universitaire (CHU)
# 32 résultat(s) sur tout le territoire

lookup <- data.table(STJR = c(0, 1, 2, 3), STJR_LABEL = c("CHU", "PUB", "PLU", "PNL"))

status <- data.table()
for (i in 2013:2015) {
    id <- readRDS(paste0("Data/In/ID/ID_", i, ".rds"))
    colnames(id) <- toupper(colnames(id))
    id <- id[, ..cols_id]
    id[FI_EJ %in% CHU_FI_EJ, STJR := 0]
    id[, STJR := as.numeric(STJR)]
    id[lookup, STJR_LABEL := i.STJR_LABEL, on = .(STJR)]
    status <- rbind(status, id)
}
saveRDS(status, "Data/Out/status_2013_2015.rds")

# ---- Combine control and status ---- #
control <- readRDS("Data/Out/control_2013_2015.rds")
status <- readRDS("Data/Out/status_2013_2015.rds")
cols <- colnames(control)[!colnames(control) %in% c("FI", "RS")]
for (i in cols) {
    control[, (i) := as.numeric(gsub(",", ".", get(i)))]
}
control_stat_23 <- merge(status[STJR == 2 | STJR == 3], control, by = c("AN", "FI"), all.x = TRUE)
control_stat_01 <- merge(status[STJR == 0 | STJR == 1], control, by.x = c("AN", "FI_EJ"), by.y = c("AN", "FI"), all.x = TRUE)
control_stat <- rbind(control_stat_01, control_stat_23)
colnames(control_stat)
control_stat <- unique(control_stat, by = c("AN", "FI", "FI_EJ"))
saveRDS(control_stat, "Data/Out/control_stat_2013_2015.rds")

# ---- Extract those with stable legal status from 2013-2022 ---- #
status1 <- readRDS("Data/Out/status_2013_2015.rds")
status2 <- readRDS("Data/Out/status_2016_2022.rds")
status <- rbind(status1, status2)
stat_unique <- unique(status[, .(FI, STJR)])
stat_unique <- stat_unique[, .N, by = .(FI)]
stat_unique <- stat_unique[N == 1]
fi <- stat_unique$FI
status <- unique(status[FI %in% fi, .(FI, FI_EJ, STJR, STJR_LABEL)])
saveRDS(status, "Data/Out/status_stable_2013_2022.rds")
