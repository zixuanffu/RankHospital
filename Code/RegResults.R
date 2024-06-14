rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")

dt_inf <- readRDS("Data/Out/dt_inf.rds")
dt_inf[, SEJ_MCO := SEJHC_MCO + SEJHP_MCO]
dt_inf[, SEJ_PSY := VEN_TOT + SEJ_HTP_TOT]

dt_inf[, MCO := (SEJHC_MCO > 1)]
colnames(dt_inf)
# replicate the regression in Table 6 col 4

varl <- "ETP_INF"
lhs <- add_log(varl)
varr <- c("SEJ_MCO", "SEANCES_MED", "PASSU")
rhs_x <- paste(c(add_log(varr), "CASEMIX"), collapse = " + ")
varr_z <- c("SEJ_MCO", "SEANCES_MED", "PASSU") # "ETP_INF"
rhs_z <- paste(add_lag(add_log(varr_z), 2), collapse = " + ")
rhs <- paste(rhs_x, "|", rhs_z) # "+lag(log(ETP_INF),1)",
rhs
formula <- as.formula(paste(lhs, "~", rhs))
formula
z <- pgmm(formula, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(z)

reg_fixest <- feols(formula, dt_inf, cluster = "STJR")
summary(reg_fixest)
help(pwtest)

pwtest(formula, data = dt_inf, effect = "individual")
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varr2 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "PLA_MCO")


rhs1 <- paste(c(add_log(varr1), "CASEMIX-1"), collapse = " + ")
rhs2a <- paste(c(add_log(varr2), "CANCER", "CASEMIX-1"), collapse = " + ")
rhs2b <- paste(c(add_log(varr2), "CANCER", "log(PLA_MCO)*CASEMIX", "CASEMIX -1"), collapse = " + ")
rhs <- paste(c(add_log(varr3), "CANCER", "log(PLA_MCO)*CASEMIX", "CASEMIX-1"), collapse = " + ")
