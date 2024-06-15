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

# Pooling regression
# sort of replicate the regression in Table 6 col 4

var_input <- c("ETP_INF")
var_output <- c("SEJ_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD", "SEJ_PSY")
var_control <- c("STJR")

var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), 2)), collapse = "+")

formula_pool <- as.formula(paste0(var_y, "~", var_x))
print(formula_pool)
formula_pool_iv <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
print(formula_pool_iv)

reg_pool <- feols(formula_pool, data = dt_inf)
summary(reg_pool)
reg_pool_iv <- plm(formula_pool_iv, data = dt_inf, index = c("FI", "AN"), model = "pooling")
summary(reg_pool_iv)


# Dummy variable regression
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output), var_control), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), 2), var_control), collapse = "+")
formula_dumb <- as.formula(paste0(var_y, "~", var_x))
print(formula_dumb)
formula_dumb_iv <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
print(formula_dumb_iv)

reg_dumb <- feols(formula_dumb, data = dt_inf)
summary(reg_dumb)
reg_dumb_iv <- plm(formula_dumb_iv, data = dt_inf, index = c("FI", "AN"), model = "pooling")
summary(reg_dumb_iv)

# Separate regression
reg_0 <- feols(formula_pool, data = dt_inf[STJR == 0])
summary(reg_0)
reg_1 <- feols(formula_pool, data = dt_inf[STJR == 1])
summary(reg_1)
reg_2 <- feols(formula_pool, data = dt_inf[STJR == 2])
summary(reg_2)
reg_3 <- feols(formula_pool, data = dt_inf[STJR == 3])
summary(reg_3)

etable(reg_pool, reg_dumb, reg_0, reg_1, reg_2, reg_3, se.below = TRUE, digits = 4, fitstat = ~ n + sq.cor + r2, digits.stats = 4, tex = TRUE, file = "Tables/2013-2022/reg_pool.tex", signif.code = "letters", replace = TRUE)

# Fixed effect regression
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), 2)), collapse = "+")
formula_fe <- as.formula(paste(var_y, "~", var_x, "|FI"))
print(formula_fe)
reg_fe <- feols(formula_fe, data = dt_inf, cluster = "FI")
summary(reg_fe)

formula_plm <- as.formula(paste(var_y, "~", var_x))
print(formula_plm)
reg_wg <- plm(formula_plm, data = dt_inf, effect = "individual", index = c("FI", "AN"), model = "within")
summary(reg_wg, robust = TRUE)
formula_plm <- as.formula(paste(var_y, "~", var_x, "-1"))
print(formula_plm)
reg_fd <- plm(formula_plm, data = dt_inf, effect = "individual", index = c("FI", "AN"), model = "fd")
summary(reg_fd, robust = TRUE)

# GMM
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), "2")), collapse = "+")
formula_gmm <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
print(formula_gmm)
reg_gmm <- pgmm(formula_gmm, data = dt_inf, effect = "individual", index = c("FI", "AN"), transformation = "ld", collapse = TRUE)

summary(reg_gmm)

help(pgmm)
mtest(reg_gmm, order = 1L)
mtest(reg_gmm, order = 2L)

# as a test
data("EmplUK", package = "plm")
ar <- pgmm(
    log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
        lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
    data = EmplUK, effect = "twoways", model = "twosteps"
)
mtest(ar, order = 1L)
mtest(ar, order = 2L, vcov = vcovHC)
