rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")
colnames(dt_inf)
dt_inf <- readRDS("Data/Out/dt_inf_pool.rds")
dt_inf <- dt_inf[ETP_INF > 1]
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
var_output <- c("SEJ_MCO", "SEANCES_MED", "CONSULT_EXT", "SEJ_PSY", "ENTSSR", "SEJ_HAD", "ENTSSR", "PASSU")
var_output <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "SEJ_PSY", "ENTSSR", "SEJ_HAD")
var_control <- c("STJR")
spec_pool <- as.formula(paste0(add_log(var_input), "~", paste(c(add_log(var_output)), collapse = "+"), "-1"))
print(spec_pool)
spec_dumb <- as.formula(paste0(add_log(var_input), "~", paste(c(add_log(var_output), var_control), collapse = "+"), "-1"))
print(spec_dumb)




reg_pool <- feols(spec_pool, data = dt_inf)
summary(reg_pool)
reg_dumb <- feols(spec_dumb, data = dt_inf)
summary(reg_dumb)
reg_0 <- feols(spec_pool, data = dt_inf[STJR == 0])
summary(reg_0)
reg_1 <- feols(spec_pool, data = dt_inf[STJR == 1])
summary(reg_1)
reg_2 <- feols(spec_pool, data = dt_inf[STJR == 2])
summary(reg_2)
reg_3 <- feols(spec_pool, data = dt_inf[STJR == 3])
summary(reg_3)

help(etable)
etable(reg_pool, reg_dumb, reg_0, reg_1, reg_2, reg_3, se.below = TRUE, digits = 4, fitstat = ~ n + sq.cor + pr2, digits.stats = 4, tex = TRUE, file = "Tables/2013-2022/reg_pool.tex", signif.code = "letters", replace = TRUE)
