library(data.table)

#### 1. Define input columns ####
# medical doctors
doctor <- c("ETPSAL_SPEMED", "ETPSAL_SPECHI")
# doctor_all <- c("ETPSAL_SPEMED", "ETPSAL_DT_MEDGEN", "ETPSAL_DT_URG", "ETPSAL_DT_ANESTREA", "ETPSAL_SPECHI", "ETPSAL_DT_GYNOBS")

# Spécialités médicales
# Spécialités chirurgicales
# Excluding: Psychiatres, Odontologistes, Pharmaciens, Autres
# ETPSAL_PSY
# ETPSAL_ODON
# ETPSAL_PHARMA
# ETPSAL_AUT



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

labor <- c(doctor, infirmier, aide_soignant, non_med)
cols <- c("AN","FI", "FI_EJ", labor)
#
dt2016 <- fread("Data/In/sae_stat/SAE 2016/Bases statistiques/Bases CSV/SYGEN_2016r.csv")
dt2017 <- fread("Data/In/sae_admin/SAE 2017/Base CSV/SYGEN_2017.csv")
dt2018 <- fread("Data/In/sae_admin/SAE 2018/Base CSV/SYGEN_2018.csv")
dt2019 <- fread("Data/In/sae_stat/SAE 2019/Bases statistiques/Bases CSV/SYGEN_2019r.csv")
dt2020 <- fread("Data/In/sae_stat/SAE 2020/Bases statistiques/Bases CSV/SYGEN_2020r.csv")
dt2021 <- fread("Data/In/sae_stat/SAE 2021/Bases statistiques/Bases CSV/SYGEN_2021r.csv")
dt2022 <- fread("Data/In/sae_stat/SAE 2022/Bases statistiques/Bases CSV/SYGEN_2022r.csv")

id2016 <- fread("Data/In/sae_stat/SAE 2016/CAPACT_PM_PNM/Bases CSV/capact16.csv")
id2017 <- fread("Data/In/sae_admin/SAE 2017/Base CSV/ID_2017.csv")
id2018 <- fread("Data/In/sae_admin/SAE 2018/Base CSV/ID_2018.csv")
id2019 <- fread("Data/In/sae_admin/SAE 2019/Base CSV/ID_2019.csv")
id2020 <- fread("Data/In/sae_admin/SAE 2020/Base CSV/ID_2020.csv")
id2021 <- fread("Data/In/sae_admin/SAE 2021/Base CSV/ID_2021.csv")
id2022 <- fread("Data/In/sae_stat/SAE 2022/CAPACT_PM_PNM/Bases CSV/capact22.csv")


# Initialize an empty data table to store the result
dt_all <- data.table()

for (i in seq(2016, 2022)) {
    dt <- get(paste0("dt", i))
    print("FI" %in% colnames(dt))
    dt <- dt[, ..cols]
    dt[is.na(dt)] <- 0

    dt[, input_sum := rowSums(.SD, na.rm = TRUE), .SDcols = labor]
    dt <- dt[input_sum != 0]
    dt_all <- rbind(dt_all, dt)

}


id_list <- list(id2016, id2017, id2018, id2019, id2020, id2021, id2022)
id_all <- data.table()
for (i in seq_along(id_list)) {
    colnames(id_list[[i]]) <- toupper(colnames(id_list[[i]]))
    id_list[[i]] <- id_list[[i]][, .(AN, FI, RS, FI_EJ, STJR)]
    id_all <- rbind(id_all, id_list[[i]])
}
id_all$AN <- as.numeric(id_all$AN)
dt_all$AN <- as.numeric(dt_all$AN)

dt_all_input<-data.table()
dt_all_input<- merge(id_all, dt_all, by.x = c("FI", "FI_EJ","AN"), by.y = c("FI", "FI_EJ","AN"), all.y = TRUE)

dt_all_input <- dt_all_input[!is.na(STJR)]

dt <- copy(dt_all_input)
dt[, `:=`(MD = ETPSAL_SPEMED + ETPSAL_SPECHI, IF = ETP_INFAVECSPE + ETP_INFSANSSPE, AS = ETP_AID, NM = ETP_TOT_HORS_SOINS)]
input_sum <- dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = .(AN,STJR), .SDcols = c("MD", "IF", "AS", "NM")]
input_mean <- dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by =.(AN,STJR), .SDcols = c("MD", "IF", "AS", "NM")]
