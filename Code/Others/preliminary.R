if (!require(data.table)) {
  install.packages("data.table")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(data.table)
library(ggplot2)

# Select year 2019 for the prelininary analysis

###### 1. Examine the variables ######
var <- fread("Data/Out/Liste_variables_SAE_2019_en.csv", header = TRUE, select = c("Table SAS", "Variable SAS", "Label variable SAS", "Label variable SAS_EN"))
unique(var$`Table SAS`) # list of tables
var_sygen <- var[grepl("SYGEN", var$`Table SAS`), ] # select only the SYGEN
var_sygen_etp <- var_sygen[grepl("ETP", var_sygen$`Variable SAS`), ] # select only those that contain ETP

# medical doctors
doctor <- c("ETPSAL_SPEMED", "ETPSAL_SPECHI")
doctor_all <- c("ETPSAL_SPEMED", "ETPSAL_DT_MEDGEN", "ETPSAL_DT_URG", "ETPSAL_DT_ANESTREA", "ETPSAL_SPECHI", "ETPSAL_DT_GYNOBS")


# Spécialités médicales
# Spécialités chirurgicales

# infirmier
infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE")
# ETP moyens annuels rémunérés - Infirmiers DE avec spécialisation
# ETP moyens annuels rémunérés - Infirmiers DE sans spécialisation

# aide-soignant
aide_soignant <- c("ETP_AID")
# ETP moyens annuels rémunérés - Aides-soignants

# non-medical staff
non_med <- c("ETP_TOT_HORS_SOINS", "ETP_TOT_PNM")
# Total Hors Services de Soins
# Total personnel non médical salarié

###### 2. Load the data ######
data2019 <- fread("Data/In/sygen/SYGEN_2019.csv")
data2019r <- fread("Data/In/sygen/SYGEN_2019r.csv")

labor <- c(doctor, infirmier, aide_soignant, non_med)
data2019_input <- data2019[, ..labor]
data2019r_input <- data2019r[, ..labor]
dim(data2019_input)
# [1] 3996    7
dim(data2019r_input)
# [1] 4008    7
compare <- cbind(data2019_input[, ETPSAL_SPEMED], data2019r_input[, ETPSAL_SPEMED])

# let's just use the 2019r data

data2019r_input[is.na(data2019r_input)] <- 0


# we need to identify the types of hospitals!

idb <- fread("Data/In/sae_stat/SAE 2019/Documentation/IDB_2019a.csv")
View(head(idb))
unique(idb$`STJR`)
unique(idb$`STJ`)
colnames(data2019r)
View(head(data2019r))

colnames(data2019r)[grepl("ST", colnames(data2019r))]

ght <- fread("Data/In/sae_stat/SAE 2019/Bases statistiques/Bases CSV/GHT_2019r.csv")

ST <- fread("Data/In/sae_stat/SAE 2019/Bases statistiques/Bases CSV/ST_2019a.csv")
STB <- fread("Data/In/sae_stat/SAE 2019/Bases statistiques/Bases CSV/STB_2019r.csv")

pm <- fread("Data/In/sae_stat/SAE 2019/CAPACT_PM_PNM/Bases CSV/pm_01_19.csv")
pnm <- fread("Data/In/sae_stat/SAE 2019/CAPACT_PM_PNM/Bases CSV/pnm_01_19.csv")
capact0019 <- fread("Data/In/sae_stat/SAE 2019/CAPACT_PM_PNM/Bases CSV/capact0019.csv")
capact_tot0019 <- fread("Data/In/sae_stat/SAE 2019/CAPACT_PM_PNM/Bases CSV/capact_tot0019.csv")
