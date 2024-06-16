# ---- Prepare panel for the regression ---- #
# ---- load the dataset ---- #
rm(list = ls())
dt1 <- readRDS("Data/Out/combineddata_2016_2022.rds") # 2016-2022
dt1 <- unique(dt1, by = c("AN", "FI", "FI_EJ"))
dt2 <- readRDS("Data/Out/combineddata_2013_2015.rds") # 2013-2015
dt2 <- unique(dt2, by = c("AN", "FI", "FI_EJ"))
id <- c("AN", "FI", "FI_EJ", "STJR")
input <- c("ETP_INF", "EFF_MD", "ETP_AID")
output <- c(
    "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "ENTSSR", "SEJ_HAD",
    "LIT_MCO", "PLA_MCO"
)
control <- c("CASEMIX", "CANCER", "TEACHING", "RESEARCH")
cols <- c(id, input, output, control) # variables of interest
dt <- rbind(dt1[, ..cols], dt2[, ..cols]) # 36653

# ---- select only those with stable legal status ---- #
stat_stable <- readRDS("Data/Out/status_stable_2013_2022.rds")
fi <- stat_stable$FI
dt <- dt[FI %in% fi] # 36258
# ---- remove duplicates ---- #
dt <- unique(dt, by = c("AN", "FI")) # no duplicates

# ---- 1. Prepare the panel for nurses ---- #


# ---- filter out observations with possible coding errors
dt_inf <- dt[!(FI == 760000166 & AN == 2022) | !(FI == 910001973 & AN == 2016)]
# ---- filter out zero values on the LHS ---- #
dt_inf <- dt_inf[ETP_INF + ETP_AID > 0]
# ---- add one to the RHS to avoid zero values in taking log ---- #
varr1 <- output
varl <- "ETP_INF"
dt_inf <- dt_inf[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt_inf[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_inf[, Nobs := .N, by = .(FI)]
dt_inf <- dt_inf[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 4677
# note that even for <1 nurses, there can be a huge number of sessions
saveRDS(dt_inf, "Data/Out/dt_inf_pool.rds")

# ---- 2. Prepare the panel for medical doctors ---- #
dt_md <- dt[EFF_MD > 0 & ETP_INF > 0 & ETP_AID > 0]
dt_md <- dt_md[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
varr1 <- output
dt_md[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_md[, Nobs := .N, by = .(FI)]
dt_md <- dt_md[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 4677
saveRDS(dt_md, "Data/Out/dt_md_pool.rds")
