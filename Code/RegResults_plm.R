rm(list = ls())
pacman::p_load(data.table, plm, texreg)
source("Code/RegX_plm.R")

# ---- load the dataset ---- #
dt1 <- readRDS("Data/Out/combineddata_2016_2022.rds") # 2016-2022
dt2 <- readRDS("Data/Out/combineddata_2013_2015.rds") # 2013-2015
cols <- c("AN", "FI", "FI_EJ", "EFF_MD", "ETP_INF", "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CASEMIX", "STJR") # variables of interest
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
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varl <- "ETP_INF"
dt_inf <- dt_inf[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt_inf[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_inf[, Nobs := .N, by = .(FI)]
dt_inf <- dt_inf[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 1526
# check<-dt_inf[, .N, by = .(AN,FI)] # 330780503 # 370007569


# ---- prepare the formula ---- #
rhs <- paste(c(add_log(varr1), "CASEMIX -1"), collapse = " + ")
lhs <- add_log(varl)
help(as.formula)
formula <- as.formula(paste(lhs, "~", rhs))

# ---- estimate the regression model ---- #
help(plm)
zz_fd <- plm(formula, data = dt_inf, index = c("FI", "AN"), model = "fd")
help(vcovHC)
zz_fd_sum <- summary(zz, vcov = vcovHC(zz_fd, method = c("arellano"), cluster = "group"))
zz_wg <- plm(formula, data = dt_inf, index = c("FI", "AN"), model = "within")
summary(zz_wg, vcov = vcovHC(zz_wg, method = c("arellano"), cluster = "group"))
help(pggls)
zz_fd_gls <- pggls(formula, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "fd")
summary(zz_fd_gls, vcov = vcovHC(zz_fd_gls, method = c("arellano"), cluster = "group"))
zz_wg_gls <- pggls(formula, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "within")
summary(zz_wg_gls, vcov = vcovHC(zz_wg_gls, method = c("arellano"), cluster = "group"))

z <- readRDS("Results/2013-2022/reg_inf_ols_FI.rds")
summary(z)

inherits(zz_fd_gls, "plm")

dir.create("Tables/2013-2022", showWarnings = FALSE)
help(texreg)
texreg(list(zz_fd, zz_wg, extract.pggls(zz_fd_gls), z), sdBelow = TRUE, digits = 3, file = "Tables/2013-2022/reg_inf.tex", signif.code = "letters", replace = TRUE)
texreg(extract.pggls(zz_fd_gls), sdBelow = TRUE, digits = 3, file = "Tables/2013-2022/reg_inf_gls.tex", signif.code = "letters", replace = TRUE)
