rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")
dt_inf <- readRDS("Data/Out/dt_inf_pool.rds")
dt_inf[, ETP_INF := ETP_INF + ETP_AID]
dt_inf <- dt_inf[ETP_INF > 0]
dt_inf[, SEJ_MCO := SEJHC_MCO + SEJHP_MCO]
dt_inf[, SEJ_PSY := VEN_TOT + SEJ_HTP_TOT]
dt_inf[, MCO := (SEJHC_MCO > 1)]
dt_inf[, PSY := (SEJ_PSY > 2)]
level_order <- c(1, 2, 3, 0) # use public as base level
dt_inf$STJR <- factor(dt_inf$STJR, levels = level_order)
colnames(dt_inf)

# pooling regression
# sort of replicate the regression in Table 6 col 4

var_input <- c("ETP_INF")
var_output <- c("SEJ_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD", "SEJ_PSY")
var_control <- c("STJR")
spec_pool <- as.formula(paste0(add_log(var_input), "~", paste(c(add_log(var_output)), collapse = "+")))
print(spec_pool)
spec_dumb <- as.formula(paste0(add_log(var_input), "~", paste(c(add_log(var_output), var_control), collapse = "+")))
print(spec_dumb)

reg_pool <- feols(spec_pool, data = dt_inf)
summary(reg_pool)
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output), var_control), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), 2), var_control), collapse = "+")
formula_iv <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
formula_iv
reg_pool_iv <- plm(formula_gmm, data = dt_inf, index = c("FI", "AN"), model = "pooling")
summary(reg_pool_iv)

pwtest(spec_pool, data = dt_inf)
reg_dumb <- feols(spec_dumb, data = dt_inf)

summary(reg_dumb)

reg_dumb_iv <- plm(formula_gmm, data = dt_inf, index = c("FI", "AN"), model = "pooling")
pwtest(spec_dumb, data = dt_inf)
reg_0 <- feols(spec_pool, data = dt_inf[STJR == 0])
summary(reg_0)
reg_1 <- feols(spec_pool, data = dt_inf[STJR == 1])
summary(reg_1)
reg_2 <- feols(spec_pool, data = dt_inf[STJR == 2])
summary(reg_2)
reg_3 <- feols(spec_pool, data = dt_inf[STJR == 3])
summary(reg_3)


help(etable)
etable(reg_pool, reg_dumb, reg_0, reg_1, reg_2, reg_3, se.below = TRUE, digits = 4, fitstat = ~ n + sq.cor + r2, digits.stats = 4, tex = TRUE, file = "Tables/2013-2022/reg_pool.tex", signif.code = "letters", replace = TRUE)

# fixed effect regression
formula_fe <- as.formula(paste0(add_log(var_input), "~", paste(c(add_log(var_output)), collapse = "+"), "| FI"))
print(formula_fe)
reg_fe <- feols(formula_fe, data = dt_inf, cluster = "FI")
summary(reg_fe)

formula_plm <- as.formula(paste0(add_log(var_input), "~", paste(c(add_log(var_output)), collapse = "+"), "-1"))
reg_wg <- plm(formula_plm, data = dt_inf, effect = "individual", index = c("FI", "AN"), model = "within")
summary(reg_wg, robust = TRUE)
reg_fd <- plm(formula_plm, data = dt_inf, effect = "individual", index = c("FI", "AN"), model = "fd")
summary(reg_fd, robust = TRUE)


# GMM
var_output <- c("SEJ_MCO", "SEANCES_MED")
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), 2)), collapse = "+")
formula_gmm <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
print(formula_gmm)
reg_gmm <- pgmm(formula_gmm, data = dt_inf, effect = "individual", index = c("FI", "AN"), transformation = "ld", model = "twosteps", collapse = TRUE)
summary(reg_gmm)
