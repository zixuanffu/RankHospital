library(data.table)

#### 1. Hospital ID and status ####
# Load the data for identifying hostpital
id <- fread("Data/In/sae_admin/SAE 2017/Base CSV/ID_2017.csv")
colnames(id) <- toupper(colnames(id))
# Select the columns of interest (ID and status)
id <- id[, .(AN, FI, RS, FI_EJ, STJ, STJR)]
# annee,
# N° Finess
# Raison sociale de l'entité interrogée
# N° FINESS de l'entité juridique
# Statut juridique
# Statut juridique regroupé (1 : Public, 2 : Privé lucratif, 3 : Privé non lucratif)
dim(id)
# [1] 4064    6
id <- unique(id)
dim(id)
# same

# Relabel CHU
id[grepl("CHU", RS) & STJR == 1, STJR := 0]
dim(id)

# Relabel STJR = 1 as "Public" etc.
id[STJR == 1, STJR_LABEL := "Public"]
id[STJR == 2, STJR_LABEL := "Privé lucratif"]
id[STJR == 3, STJR_LABEL := "Privé non lucratif"]
id[STJR == 0, STJR_LABEL := "CHU"]

# Summarize the number of hostpitals by STJR
hospital_sum <- id[, .N, by = .(STJR, STJR_LABEL, AN)]

#### 2. Hospital input and output ####
# Load the main dataset
dt <- fread("Data/In/sae_admin/SAE 2017/Base CSV/SYGEN_2017.csv")

# Check if FI, FI_EJ, RS are in the columns
print("FI" %in% colnames(dt)) # 1
print("FI_EJ" %in% colnames(dt)) # 1
print("RS" %in% colnames(dt)) # 0

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
id_label <- c("FI", "FI_EJ", labor)
dt_input <- dt[, ..id_label]
dt_input[is.na(dt_input)] <- 0

# remove those that do not have any labor input reported
dt_input[, input_sum := rowSums(.SD, na.rm = TRUE), .SDcols = labor]
dt_input <- dt_input[input_sum != 0]


# Merge the data
dt_input <- merge(id, dt_input, by.x = c("FI", "FI_EJ"), by.y = c("FI", "FI_EJ"), all.y = TRUE)

dt_input <- dt_input[!is.na(STJR)] # exclude those without STJR
dim(dt_input)
# [1] 3828   14

#### 3. Summarize ####
dt <- copy(dt_input)
dt[, `:=`(MD = ETPSAL_SPEMED + ETPSAL_SPECHI, IF = ETP_INFAVECSPE + ETP_INFSANSSPE, AS = ETP_AID, NM = ETP_TOT_HORS_SOINS)]
input_sum <- dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = .(AN, STJR), .SDcols = c("MD", "IF", "AS", "NM")]
input_mean <- dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = .(AN, STJR), .SDcols = c("MD", "IF", "AS", "NM")]


#### 4. Visualization ####
# Clear the plot
dev.off()


# the histogram of MDs
hist_all <- hist(dt$MD, main = "Histogram of Medical Doctors", xlab = "Number of Medical Doctors")

# 4 histograms of MDs by STJR
par(mfrow = c(2, 2))
for (i in 0:3) {
    hist(dt[STJR == i]$MD, main = paste("Histogram of Medical Doctors by STJR", i), xlab = "Number of Medical Doctors")
}

# box plot of MDs
boxplot(dt$MD, main = "Boxplot of Medical Doctors", ylab = "Number of Medical Doctors")
# 4 box plots of MDs by STJR
boxplot(MD ~ STJR, data = dt, main = "Boxplot of Medical Doctors by STJR", ylab = "Number of Medical Doctors")
