rm(list = ls())
pacman::p_load(data.table, plm, texreg)
source("Code/RegX_plm.R")

# ---- load the dataset ---- #
dt1 <- readRDS("Data/Out/combineddata_2016_2022.rds") # 2016-2022
dt2 <- readRDS("Data/Out/combineddata_2013_2015.rds") # 2013-2015
id <- c("AN", "FI", "FI_EJ", "STJR")
input <- c("ETP_INF", "EFF_MD")
output <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "LIT_MCO", "PLA_MCO")
control <- c("CASEMIX", "CANCER", "TEACHING", "RESEARCH")
cols <- c(id, input, output, control) # variables of interest
dt <- rbind(dt1[, ..cols], dt2[, ..cols])

# ---- select only those with stable legal status ---- #
FI_stat_change <- unique(dt[, .(FI, STJR)])
FI_stat_change <- FI_stat_change[, .N, by = .(FI)]
FI_stat_change <- FI_stat_change[N > 1]
FI_AN_unique <- unique(dt, by = c("AN", "FI"))

# ---- remove duplicates ---- #
dt <- unique(dt, by = c("AN", "FI"))

# ---- Prepare the panel for nurses ---- #
# ---- filter out observations with possible coding errors
# 2022 760000166
# 2016 910001973
dt_inf <- dt[!(FI == 760000166 & AN == 2022) | !(FI == 910001973 & AN == 2016)]
# ---- filter out zero values on the LHS ---- #
dt_inf <- dt_inf[ETP_INF > 0]
# ---- add one to the RHS to avoid zero values in taking log ---- #
varr1 <- output
varl <- "ETP_INF"
dt_inf <- dt_inf[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt_inf[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_inf[, Nobs := .N, by = .(FI)]
dt_inf <- dt_inf[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 1526
# check if there are duplicates
# check<-dt_inf[, .N, by = .(AN,FI)] # 330780503 # 370007569
saveRDS(dt_inf, "Data/Out/dt_inf.rds")

# ---- prepare the formula ---- #
dt_inf <- readRDS("Data/Out/dt_inf.rds")
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varr2 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "PLA_MCO")
varl <- "ETP_INF"
rhs1 <- paste(c(add_log(varr1), "CASEMIX-1"), collapse = " + ")
rhs2a <- paste(c(add_log(varr2), "CANCER", "CASEMIX-1"), collapse = " + ")
rhs2b <- paste(c(add_log(varr2), "CANCER", "log(PLA_MCO)*CASEMIX", "CASEMIX -1"), collapse = " + ")
lhs <- add_log(varl)
formula1 <- as.formula(paste(lhs, "~", rhs1))
formula1
formula2a <- as.formula(paste(lhs, "~", rhs2a))
formula2a
formula2b <- as.formula(paste(lhs, "~", rhs2b))
formula2b
# ---- estimate the regression model ---- #

# ---- assume strict exogeneity ---- #

# ---- fixed effects-within group estimator ---- #
RegX <- function(formula, dt) {
    zz_wg <- plm(formula, data = dt, index = c("FI", "AN"), model = "within")
    se_zz_wg <- vcovHC(zz_wg, method = c("arellano"), cluster = "group")
    # summary(zz_wg, vcov = se_zz_wg)
    zz_wg_gls <- pggls(formula, data = dt, index = c("FI", "AN"), effect = "individual", model = "within")
    se_zz_wg_gls <- vcov(zz_wg_gls, method = c("arellano"), cluster = "group")
    # summary(zz_wg_gls, vcov = se_zz_wg_gls)
    output <- capture.output(summary(zz_wg_gls, vcov = se_zz_wg_gls))
    writeLines(output, "Results/2013-2022/zz_wg_gls_summary.txt")


    # ---- fixed effects-first difference estimator ---- #
    zz_fd <- plm(formula, data = dt, index = c("FI", "AN"), model = "fd")
    se_zz_fd <- vcovHC(zz_fd, method = c("arellano"), cluster = "group")

    zz_fd_gls <- pggls(formula, data = dt, index = c("FI", "AN"), effect = "individual", model = "fd")
    se_zz_fd_gls <- vcov(zz_fd_gls, method = c("arellano"), cluster = "group")


    return
}

RegX(formula1, dt_inf)

# z <- readRDS("Results/2013-2022/reg_inf_ols_FI.rds")
# summary(z)

models <- list(extract.plm(zz_wg, vcov = se_zz_wg), extract.pggls(zz_wg_gls, vcov = se_zz_wg_gls), extract.plm(zz_fd, vcov = se_zz_fd), extract.pggls(zz_fd_gls, vcov = se_zz_fd_gls))
htmlreg(models, star.symbol = "*", caption = "Estimation results", digits = 4, custom.model.names = c("Within-group", "Within-group (GLS)", "First difference", "First difference (GLS)"), file = "Tables/2013-2022/reg_inf_m.html")
