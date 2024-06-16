rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")

dt_inf <- readRDS("Data/Out/dt_inf_pool.rds")
dt_inf[, ETP_INF := ETP_INF + ETP_AID] # registered and assistant nurses
dt_inf <- dt_inf[ETP_INF > 0]
dt_inf[, SEJ_MCO := SEJHC_MCO + SEJHP_MCO]
dt_inf[, SEJ_PSY := VEN_TOT + SEJ_HTP_TOT]
dt_inf[, MCO := (SEJHC_MCO > 1)]
dt_inf[, PSY := (SEJ_PSY > 2)]
level_order <- c(1, 2, 3, 0) # use public as base level
dt_inf$STJR <- factor(dt_inf$STJR, levels = level_order)
colnames(dt_inf)

# ---- 1. Input and output variables ---- #
var_input <- c("ETP_INF")
# we distinguish 8 types of output (combine psychiatric inpatient and outpatient)
var_output <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD", "SEJ_PSY")
# var_output <- c("SEJ_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD", "SEJ_PSY")
var_control <- c("STJR")
pd_inf <- panel(data = dt_inf, panel.id = ~ FI + AN, time.step = ) # set the time and id index
help(panel)
# ---- 2. Regression (without individual fixed effects) ---- #

# ---- 2.1a Pooling ---- #
# sort of replicate the regression in Table 6 col 4

var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")

formula_pool <- as.formula(paste0(var_y, "~", var_x))
# print(formula_pool)
# log(ETP_INF) ~ log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED) +
#     log(CONSULT_EXT) + log(PASSU) + log(ENTSSR) + log(SEJ_HAD) +
#     log(SEJ_PSY)

reg_pool <- feols(formula_pool, data = dt_inf)
summary(reg_pool)

# ---- 2.1b Pooling IV ---- #
var_z <- paste(c(add_log(add_l(var_output, 1))), collapse = "+")
#  "log(l(SEJHC_MCO,1))+log(l(SEJHP_MCO,1))+log(l(SEANCES_MED,1))+log(l(CONSULT_EXT,1))+log(l(PASSU,1))+log(l(ENTSSR,1))+log(l(SEJ_HAD,1))+log(l(SEJ_PSY,1))"
formula_pool_iv <- as.formula(paste0(var_y, "~", 1, "|", var_x, "~", var_z))
# log(ETP_INF) ~ 1 | log(SEJHC_MCO)... ~ log(l(SEJHC_MCO, 1)) ...

reg_pool_iv <- feols(formula_pool_iv, data = pd_inf)
# reg_pool_iv <- plm(formula_pool_iv, data = dt_inf, index = c("FI", "AN"), model = "pooling")
summary(reg_pool_iv)

# ---- 2.2a Dummy variable regression ---- #
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output), var_control), collapse = "+")
formula_dum <- as.formula(paste0(var_y, "~", var_x))
print(formula_dum)
# log(ETP_INF) ~ log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED) +
#     log(CONSULT_EXT) + log(PASSU) + log(ENTSSR) + log(SEJ_HAD) +
#     log(SEJ_PSY) + STJR
reg_dum <- feols(formula_dum, data = pd_inf)
summary(reg_dum)

# ---- 2.2b Dummy variable IV regression ---- #
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_log(add_l(var_output, 1))), collapse = "+") # log(l(SEJHC_MCO,1))
formula_dum_iv <- as.formula(paste0(var_y, "~", var_control, "|", var_x, "~", var_z))
print(formula_dum_iv)
# log(ETP_INF) ~ STJR | log(SEJHC_MCO) ... ~ log(l(SEJHC_MCO, 1)) ...
reg_dum_iv <- feols(formula_dum_iv, data = pd_inf)
summary(reg_dum_iv)

header <- c("Pool", "Pool IV", "Dummy", "Dummy IV")
etable(reg_pool, reg_pool_iv, reg_dum, reg_dum_iv, headers = header, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_pool_dummy.tex", replace = TRUE)

# ---- 2.3 Separate regression by legal status ---- #
reg_0 <- feols(formula_pool, data = pd_inf[STJR == 0])
summary(reg_0)
reg_1 <- feols(formula_pool, data = pd_inf[STJR == 1])
summary(reg_1)
reg_2 <- feols(formula_pool, data = pd_inf[STJR == 2])
summary(reg_2)
reg_3 <- feols(formula_pool, data = pd_inf[STJR == 3])
summary(reg_3)

header <- c("Teaching", "Public", "Forprofit", "Nonprofit")
etable(reg_0, reg_1, reg_2, reg_3, headers = header, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_sep.tex", signif.code = "letters", replace = TRUE)


# ---- 3. Regression with individual fixed effects ---- #

# ---- 3.1a Within Group estimation (strict exogeneity) ---- #
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_wg <- as.formula(paste0(var_y, "~", var_x, "|FI"))
# log(ETP_INF) ~ log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED) +
#     log(CONSULT_EXT) + log(PASSU) + log(ENTSSR) + log(SEJ_HAD) +
#     log(SEJ_PSY) | FI

reg_wg <- feols(formula_wg, data = pd_inf, cluster = "FI")
summary(reg_wg)

# ---- 3.1b First difference estimation (relaxed assumption) ---- #
var_y <- (add_d(add_log(var_input)))
var_x <- paste((add_d(add_log(var_output))), collapse = "+")
formula_fd <- as.formula(paste0(var_y, "~", var_x, "-1"))
formula_fd
# d(log(ETP_INF)) ~ d(log(SEJHC_MCO)) + d(log(SEJHP_MCO)) + d(log(SEANCES_MED)) +
#     d(log(CONSULT_EXT)) + d(log(PASSU)) + d(log(ENTSSR)) + d(log(SEJ_HAD)) +
#     d(log(SEJ_PSY)) - 1

reg_fd <- feols(formula_fd, data = pd_inf, cluster = "FI")
summary(reg_fd)

# compare with the results from plm
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_plm <- as.formula(paste0(var_y, "~", var_x, -1))
reg_plm <- plm(formula_plm, data = dt_inf, index = c("FI", "AN"), model = "within")
summary(reg_plm)


for (i in c(var_input, var_output)) {
    pd_inf[, paste0("d_", i) := d(log(get(i)), 1)]
}
add_d <- function(x) {
    d <- rep("d_", length(x))
    return(paste0(d, x))
}
var_y <- (add_d(var_input))
var_x <- paste((add_d(var_output)), collapse = "+")
formula_fd <- as.formula(paste0(var_y, "~", var_x, "-1"))
print(formula_fd)
reg_fd <- feols(formula_fd, data = pd_inf, panel.id = ~ FI + AN, cluster = "FI")
summary(reg_fd)
add_lag <- function(var_list, lag) {
    len <- length(var_list)
    lag_left <- rep("l(", length(len))
    lag_right <- rep(")", length(len))
    lag_var_list <- paste0(lag_left, var_list, ", ", lag, lag_right)
    return(lag_var_list)
}

# FD GMM
var_z <- paste(add_log(add_lag(var_output, 2)), collapse = "+")
var_z
formula_fd_iv <- as.formula(paste0(var_y, "~", "-1", "|", var_x, "~", var_z))
print(formula_fd_iv)
reg_fd_iv <- feols(formula_fd_iv, data = pd_inf, panel.id = ~ FI + AN, cluster = "FI")
summary(reg_fd_iv)
# SYS GMM
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
for (i in var_output) {
    pd_inf[, paste0("ld_", i) := l(d(log(get(i)), 1), 1)]
}
add_ld <- function(x) {
    ld <- rep("ld_", length(x))
    return(paste0(ld, x))
}
add_diff <- function(x, lag) {
    len <- length(x)
    diff_left <- rep("d(", length(len))
    diff_right <- rep(")", length(len))
    diff_var_list <- paste0(diff_left, x, ", ", lag, diff_right)
    return(diff_var_list)
}
var_z <- paste((add_ld(var_output)), collapse = "+")
var_z
formula_fd_sys <- as.formula(paste0(var_y, "~", "-1", "|", var_x, "~", var_z))
print(formula_fd_sys)
reg_fd_sys <- feols(formula_fd_sys, data = pd_inf, panel.id = ~ FI + AN, cluster = "FI")
summary(reg_fd_sys)

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
sargan(ar)
