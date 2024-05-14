# labor input
# infirmier
infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE")
# ETP moyens annuels rémunérés - Infirmiers DE avec spécialisation
# ETP moyens annuels rémunérés - Infirmiers DE sans spécialisation

# aide-soignant
aide_soignant <- c("ETP_AID")
# ETP moyens annuels rémunérés - Aides-soignants

# non-medical staff
non_med <- c("ETP_TOT_HORS_SOINS")
# Total Hors Services de Soins
# Not "ETP_TOT_PNM": Total personnel non médical salarié

# medical doctors
dt <- readRDS(paste0("Data/In/SYGEN_admin/SYGEN_", 2016, ".rds"))
colnames(dt)[grepl("ETPSAL", colnames(dt))]
patterns_lib <- c("^EFFLIB.*MEDGEN$", "EFFLIB_SPEMED", "EFFLIB_SPECHI", "EFFLIB_TOT")
patterns_sal <- c("^ETPSAL.*MEDGEN$", "ETPSAL_SPEMED", "ETPSAL_SPECHI", "ETPSAL_TOT", "EFFSAL.*MEDGEN", "EFFSAL_SPEMED", "EFFSAL_SPECHI", "EFFSAL_TOT")
patterns <- c(patterns_lib, patterns_sal)
# Medicine general
# Spécialités médicales
# Spécialités chirurgicales
# Excluding: Psychiatres, Odontologistes, Pharmaciens, Autres
# ETPSAL_PSY
# ETPSAL_ODON
# ETPSAL_PHARMA
# ETPSAL_AUT

# capacity
cols_cap <- c("AN", "FI", "FI_EJ", "LIT_MCO", "PLA_MCO")

# output
psy <- c("SEJ_HTP_TOT", "VEN_HDJ_TOT", "VEN_HDN_TOT")
cols_out <- c(
    "AN", "FI", "FI_EJ", "SEJHC_MCO", "SEJHP_MCO", "PASSU_GEN", "SEANCES_MED",
    psy, "SEJHC_SSR", "JOUHP_SSR", "ENT", "SEJ_HAD", "CONSULT_EXT"
)

dt_labor_all <- data.table()
dt_cap_all <- data.table()
dt_out_all <- data.table()

# "PASSU_URG" %in% colnames(dt)
# colnames(dt)[grepl("PASSU", colnames(dt))] # PASSU_GEN, PASSU_URG

for (i in seq(2016, 2022)) {
    dt <- readRDS(paste0("Data/In/SYGEN_admin/SYGEN_", i, ".rds"))
    # dt[is.na(dt)] <- 0
    # get labor
    doctor <- colnames(dt)[grepl(paste(patterns, collapse = "|"), colnames(dt))]
    labor <- c(doctor, infirmier, aide_soignant, non_med)
    cols <- c("AN", "FI", "FI_EJ", labor)
    dt_labor <- dt[, ..cols]
    if ("EFFLIB_DT_MEDGEN" %in% doctor) {
        dt_labor[, EFFLIB_DT_MEDGEN := 0]
        setnames(dt_labor, "EFFLIB_DT_MEDGEN", "EFFLIB_MEDGEN")
    }
    if ("ETPSAL_DT_MEDGEN" %in% doctor) {
        dt_labor[, ETPSAL_DT_MEDGEN := 0]
        dt_labor[, EFFSAL_DT_MEDGEN := 0]
        setnames(dt_labor, "ETPSAL_DT_MEDGEN", "ETPSAL_MEDGEN")
        setnames(dt_labor, "EFFSAL_DT_MEDGEN", "EFFSAL_MEDGEN")
    }
    dt_labor_all <- rbind(dt_labor_all, dt_labor)

    #     # get capacity

    #     dt_cap <- dt[, ..cols_cap]
    #     dt_cap_all <- rbind(dt_cap_all, dt_cap)

    #     # get output
    #     dt_out <- dt[, ..cols_out]
    #     dt_out_all <- rbind(dt_out_all, dt_out)
}
saveRDS(dt_labor_all, "Data/Out/labor_input_2016_2022_admin.rds")
