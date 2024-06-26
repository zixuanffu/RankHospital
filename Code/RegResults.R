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

# ---- 1. Define input and output variables ---- #
var_input <- c("ETP_INF")
# we distinguish 8 types of output (combine psychiatric inpatient and outpatient)
var_output <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD", "SEJ_PSY")
var_control <- c("STJR")
pd_inf <- panel(data = dt_inf, panel.id = ~ FI + AN, time.step = "consecutive") # set the time and id index


x <- c("STAC inpatient", "STAC outpatient", "Medical sessions", "External consultations", "Emergency", "Long-term \\& follow-up", "Home care", "Psychiatric care")
names(x) <- add_log(var_output)

y <- "Nurses"
names(y) <- add_log(var_input)

var_dict <- c(x, y, STJR2 = "Private Forprofit", STJR3 = "Private Nonprofit", STJR0 = "Teaching")
setFixest_dict(var_dict)
var_list <- as.list(var_dict)

# ---- 2. Regression (without individual fixed effects) ---- #

# ---- 2.1a Pooling ---- #
# sort of replicate the regression in Table 6 col 4

var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")

formula_pool <- as.formula(paste0(var_y, "~", var_x))

reg_pool <- feols(formula_pool, data = dt_inf)

# ---- 2.1b Pooling IV ---- #
var_z <- paste(c(add_log(add_l(var_output, 1))), collapse = "+")
formula_pool_iv <- as.formula(paste0(var_y, "~", 1, "|", var_x, "~", var_z))

reg_pool_iv <- feols(formula_pool_iv, data = pd_inf)

# ---- 2.2a Dummy variable regression ---- #
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output), var_control), collapse = "+")
formula_dum <- as.formula(paste0(var_y, "~", var_x))
print(formula_dum)
reg_dum <- feols(formula_dum, data = pd_inf)

# ---- 2.2b Dummy variable IV regression ---- #
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_log(add_l(var_output, 1))), collapse = "+") # log(l(SEJHC_MCO,1))
formula_dum_iv <- as.formula(paste0(var_y, "~", var_control, "|", var_x, "~", var_z))
print(formula_dum_iv)


reg_dum_iv <- feols(formula_dum_iv, data = pd_inf)
reg_pool_iv_ex <- feols(formula_pool_iv, data = pd_inf[STJR != 0])
reg_dum_iv_ex <- feols(formula_dum_iv, data = pd_inf[STJR != 0])
header <- c("Pool", "Pool exclude")
etable(reg_pool_iv, reg_pool_iv_ex, headers = header, vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_pool_iv_ex.tex", replace = TRUE)

etable(reg_dum_iv, reg_dum_iv_ex, headers = c("Dummy", "Dummy exclude"), vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_dummy_iv_ex.tex", replace = TRUE)

header <- c("Pool", "Pool IV", "Dummy", "Dummy IV")
etable(reg_pool, reg_pool_iv, reg_dum, reg_dum_iv, headers = header, vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_pool_dummy.tex", replace = TRUE)

etable(reg_dum, reg_dum_iv, dict = var_dict, headers = c("Dummy", "Dummy IV"), vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_dummy_iv.tex", replace = TRUE)

etable(reg_pool_iv, reg_dum_iv, headers = c("Pool IV", "Dummy IV"), vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_pool_dummy_iv.tex", replace = TRUE)
# ---- 2.3 Separate regression by legal status ---- #
reg_0 <- feols(formula_pool_iv, data = pd_inf[STJR == 0])
reg_1 <- feols(formula_pool_iv, data = pd_inf[STJR == 1])
reg_2 <- feols(formula_pool_iv, data = pd_inf[STJR == 2])
reg_3 <- feols(formula_pool_iv, data = pd_inf[STJR == 3])

header <- c("Teaching", "Public", "Forprofit", "Nonprofit")
etable(reg_0, reg_1, reg_2, reg_3, headers = header, vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_sep_iv.tex", replace = TRUE)
header <- c("Public", "Forprofit", "Nonprofit")
etable(reg_1, reg_2, reg_3, headers = header, vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_sep_iv_ex.tex", replace = TRUE)
# to generate a table with 2 columns...
header <- c("Within Group", "First Differece")
etable(reg_2, reg_3, headers = header, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_plm_fe.tex", replace = TRUE)


# ---- 3. Regression with individual fixed effects ---- #

# ---- 3.1a Within Group estimation (strict exogeneity) ---- #
# use fixest package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_wg <- as.formula(paste0(var_y, "~", var_x, "|FI"))
reg_wg <- feols(formula_wg, data = pd_inf, cluster = "FI")

# use plm package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_plm <- as.formula(paste0(var_y, "~", var_x, -1))
reg_wg_plm <- plm(formula_plm, data = pd_inf, index = c("FI", "AN"), model = "within")
reg_fd_plm <- plm(formula_plm, data = pd_inf, index = c("FI", "AN"), model = "fd")
texreg(list(reg_wg_plm, reg_fd_plm), digits = 3, custom.model.names = c("Within", "First Diff"), file = "Tables/2013-2022/reg_fe_plm.tex", booktabs = TRUE, table = FALSE)


# ---- 3.1b First difference estimation (relaxed assumption) ---- #
# use fixest package
var_y <- (add_d(add_log(var_input)))
var_x <- paste((add_d(add_log(var_output))), collapse = "+")
formula_fd <- as.formula(paste0(var_y, "~", var_x, "-1"))
reg_fd <- feols(formula_fd, data = pd_inf, cluster = "FI")
summary(reg_fd)

# use plm package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_plm <- as.formula(paste0(var_y, "~", var_x, -1))
reg_fd_plm <- plm(formula_plm, data = pd_inf, index = c("FI", "AN"), model = "fd")

# ---- 4.1 FD GMM (current error affect current and future regressors) ---- #
# use fixest package
var_y <- (add_d(add_log(var_input)))
var_x <- paste((add_d(add_log(var_output))), collapse = "+")
var_z <- paste(add_log(add_l(var_output, 2)), collapse = "+")

formula_fd_iv <- as.formula(paste0(var_y, "~", "-1", "|", var_x, "~", var_z))
print(formula_fd_iv)
reg_fd_iv <- feols(formula_fd_iv, data = pd_inf, cluster = "FI")
summary(reg_fd_iv)

# very weak instruments I have to say

# ---- 4.2 Sys GMM (orthogonal) ---- #
for (i in c(var_input, var_output)) {
    pd_inf[, paste0("d_", i) := d(log(get(i)), 1)]
}
for (i in c(var_input, var_output)) {
    pd_inf[, paste0("ld_", i) := l(get(paste0("d_", i)), 1)]
}

# use fixest package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(add_l(add_dd(var_output), 1), collapse = "+")
formula_fd_sys <- as.formula(paste0(var_y, "~", "-1", "|", var_x, "-1", "~", var_z, "-1"))
reg_fd_sys <- feols(formula_fd_sys, data = pd_inf, cluster = "FI")
summary(reg_fd_sys)
# seems like a better instrument:)
# seems like we can not get rid of the intercept for some reasons...
# maybe we can perform just identified GMM/IV with manually constructed instrument?

# use AER package
pacman::p_load(AER, gmm) # to run iv regression
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(paste("ld", var_output, sep = "_"), collapse = "+")
formula <- as.formula(paste0(var_y, "~", var_x, "-1|", var_z))
reg_iv <- ivreg(formula, data = as.data.frame(pd_inf), na.action = na.omit)
summary(reg_iv)
var_list <- as.list(var_dict)
texreg(list(reg_iv), file = "Tables/2013-2022/reg_iv.tex", custom.coef.map = var_list, booktabs = TRUE, table = FALSE)

# use plm package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), "2")), collapse = "+")
formula_gmm <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
reg_gmm_sys <- pgmm(formula_gmm, data = dt_inf, effect = "individual", index = c("FI", "AN"), transformation = "ld", robust = TRUE, collapse = FALSE)
reg_gmm_fd <- pgmm(formula_gmm, data = dt_inf, effect = "individual", index = c("FI", "AN"), transformation = "d", robust = TRUE, collapse = FALSE)
summary(reg_gmm_sys, robust = TRUE)
summary(reg_gmm_fd, robust = TRUE)
# check moment conditions
W <- reg_gmm_sys$W
str(W[1]) # 15 x 16

texreg(list(reg_gmm_fd, reg_gmm_sys), file = "Tables/2013-2022/reg_gmm.tex", booktabs = TRUE, table = FALSE)
texreg(list(reg_wg_plm, reg_fd_plm, reg_gmm_sys), custom.coef.map = var_list, file = "Tables/2013-2022/reg_wg_fd_gmm_b.tex", booktabs = TRUE, table = FALSE)
texreg(list(reg_wg_plm, reg_fd_plm, reg_iv), custom.coef.map = var_list, file = "Tables/2013-2022/reg_wg_fd_iv_b.tex", booktabs = TRUE, table = FALSE)

# to generate a table with 3 columns
header <- c("Within Group", "First Difference", "System GMM")
etable(reg_1, reg_2, reg_3, headers = header, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_wg_fd_gmm.tex", replace = TRUE)

# ---- 4. manually calculate the residuals from regression results ---- #
coef <- reg_gmm_sys$coefficients
mat <- log(as.matrix(dt_inf[, ..var_output]))
coef <- as.vector(coef)
fitted_value <- mat %*% coef

dt_inf[, fitted := fitted_value]
dt_inf[, residual := log(ETP_INF) - fitted]
dt_inf[, FixedEffect := mean(residual), by = (FI)]
dt_inf[, Res := residual - FixedEffect]
pdt_used <- dt_inf[, .(AN, FI, STJR, fitted, residual, FixedEffect, Res)]
saveRDS(pdt_used, "Data/Out/pdt_used_gmm_sys.rds")
saveRDS(pdt_used, "Results/2013-2022/pdt_used_gmm_sys.rds")

# manually calculate the residuals for modifed system gmm
coef <- reg_iv$coefficients
mat <- log(as.matrix(dt_inf[, ..var_output]))
coef <- as.vector(coef)
fitted_value <- mat %*% coef

dt_inf[, fitted := fitted_value]
dt_inf[, residual := log(ETP_INF) - fitted]
dt_inf[, FixedEffect := mean(residual), by = (FI)]
dt_inf[, Res := residual - FixedEffect]
pdt_used <- dt_inf[, .(AN, FI, STJR, fitted, residual, FixedEffect, Res)]
saveRDS(pdt_used, "Data/Out/pdt_used_gmm_sys.rds")
saveRDS(pdt_used, "Results/2013-2022/pdt_used_gmm_sys_m.rds")

# ---- 5. Remove teaching hospitals and run regression ---- #
# remove teaching hospitals
pd_inf_ex <- pd_inf[STJR != 0]

# pooling

var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_pool <- as.formula(paste0(var_y, "~", var_x))
reg_pool <- feols(formula_pool, data = pd_inf_ex)

# ---- Pooling IV ---- #
var_z <- paste(c(add_log(add_l(var_output, 1))), collapse = "+")
formula_pool_iv <- as.formula(paste0(var_y, "~", 1, "|", var_x, "~", var_z))
reg_pool_iv <- feols(formula_pool_iv, data = pd_inf_ex)

# ---- Dummy variable regression ---- #
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output), var_control), collapse = "+")
formula_dum <- as.formula(paste0(var_y, "~", var_x))
reg_dum <- feols(formula_dum, data = pd_inf_ex)

# ---- Dummy variable IV regression ---- #
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_log(add_l(var_output, 1))), collapse = "+") # log(l(SEJHC_MCO,1))
formula_dum_iv <- as.formula(paste0(var_y, "~", var_control, "|", var_x, "~", var_z))
reg_dum_iv <- feols(formula_dum_iv, data = pd_inf_ex)

header <- c("Pool", "Pool IV", "Dummy", "Dummy IV")


x <- c("STAC inpatient", "STAC outpatient", "Medical sessions", "External consultations", "Emergency", "Long-term \\& follow-up", "Home care", "Psychiatric care")
names(x) <- add_log(var_output)

y <- "Nurses"
names(y) <- add_log(var_input)

var_dict <- c(x, y, STJR2 = "Private Forprofit", STJR3 = "Private Nonprofit")
setFixest_dict(var_dict)
etable(reg_pool, reg_pool_iv, reg_dum, reg_dum_iv, headers = header, vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_pool_dummy_ex.tex", replace = TRUE)
etable(reg_dum, reg_dum_iv, dict = var_dict, headers = c("Dummy", "Dummy IV"), vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_dummy_iv_ex.tex", replace = TRUE)
etable(reg_pool_iv, reg_dum_iv, headers = c("Pool IV", "Dummy IV"), vcov = "hetero", se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_pool_dummy_iv_ex.tex", replace = TRUE)


# ---- 3.1b First difference estimation (relaxed assumption) ---- #

# use plm package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
formula_plm <- as.formula(paste0(var_y, "~", var_x, -1))
reg_wg_plm <- plm(formula_plm, data = pd_inf_ex, index = c("FI", "AN"), model = "within")
reg_fd_plm <- plm(formula_plm, data = pd_inf_ex, index = c("FI", "AN"), model = "fd")
# texreg(list(reg_wg_plm, reg_fd_plm), digits = 3, custom.model.names = c("Within", "First Diff"), file = "Tables/2013-2022/reg_fe_plm.tex", booktabs = TRUE, table = FALSE)

# ---- 4.1 FD GMM (current error affect current and future regressors) ---- #

for (i in c(var_input, var_output)) {
    pd_inf_ex[, paste0("d_", i) := d(log(get(i)), 1)]
}
for (i in c(var_input, var_output)) {
    pd_inf_ex[, paste0("ld_", i) := l(get(paste0("d_", i)), 1)]
}

# use AER package
pacman::p_load(AER, gmm) # to run iv regression
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(paste("ld", var_output, sep = "_"), collapse = "+")
formula <- as.formula(paste0(var_y, "~", var_x, "-1|", var_z))
reg_iv <- ivreg(formula, data = as.data.frame(pd_inf_ex), na.action = na.omit)
summary(reg_iv)

# texreg(list(reg_iv), file = "Tables/2013-2022/reg_iv.tex", custom.coef.map = var_list, booktabs = TRUE, table = FALSE)

# use plm package
var_y <- add_log(var_input)
var_x <- paste(c(add_log(var_output)), collapse = "+")
var_z <- paste(c(add_lag(add_log(var_output), "2")), collapse = "+")
formula_gmm <- as.formula(paste0(var_y, "~", var_x, "|", var_z))
formula_gmm
reg_gmm_sys <- pgmm(formula_gmm, data = pd_inf_ex, effect = "individual", index = c("FI", "AN"), transformation = "ld", robust = TRUE)
reg_gmm_fd <- pgmm(formula_gmm, data = pd_inf_ex, effect = "individual", index = c("FI", "AN"), transformation = "d", robust = TRUE)
# reg_gmm_fd_c<- pgmm(formula_gmm, data = pd_inf_ex, effect = "individual", index = c("FI", "AN"), transformation = "d", robust = TRUE, collapse = TRUE)
summary(reg_gmm_fd, robust = TRUE)
summary(reg_gmm_sys, robust = TRUE)
# texreg(list(reg_gmm_fd, reg_gmm_fd_c), file = "Tables/2013-2022/reg_fd_gmm_ex.tex", booktabs = FALSE, table = FALSE)

help(pgmm)

model_names <- c("Within Group", "First Difference", "FD GMM", "SYS GMM")
texreg(list(reg_wg_plm, reg_fd_plm, reg_gmm_fd, reg_gmm_sys), custom.model.names = model_names, custom.coef.map = var_list, file = "Tables/2013-2022/reg_wg_fd_gmm_b.tex", table = FALSE)
texreg(list(reg_wg_plm, reg_fd_plm, reg_iv), custom.coef.map = var_list, file = "Tables/2013-2022/reg_wg_fd_iv_b.tex", booktabs = TRUE, table = FALSE)
help(texreg)
# to generate a table with 3 columns
header <- c("Within Group", "First Difference", "FD GMM", "SYS GMM")
etable(reg_1, reg_2, reg_3, headers = header, se.below = TRUE, digits = 3, fitstat = ~ n + r2 + war2, digits.stats = 3, tex = TRUE, file = "Tables/2013-2022/reg_wg_fd_gmm.tex", replace = TRUE)

# ---- 4. manually calculate the residuals from regression results ---- #
dt_inf <- dt_inf[STJR != 0]
coef <- reg_gmm_fd$coefficients
mat <- log(as.matrix(dt_inf[, ..var_output]))
coef <- as.vector(coef)
fitted_value <- mat %*% coef

dt_inf[, fitted := fitted_value]
dt_inf[, residual := log(ETP_INF) - fitted]
dt_inf[, FixedEffect := mean(residual), by = (FI)]
dt_inf[, Res := residual - FixedEffect]
pdt_used <- dt_inf[, .(AN, FI, STJR, fitted, residual, FixedEffect, Res)]
saveRDS(pdt_used, "Data/Out/pdt_used_gmm_fd.rds")
saveRDS(pdt_used, "Results/2013-2022/pdt_used_gmm_fd.rds")
