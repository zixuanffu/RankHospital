rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")

dt_inf <- readRDS("Data/Out/dt_inf.rds")
colnames(dt_inf)
# replicate the regression in Table 6 col 4
varr <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "ENTSSR", "SEJ_HAD", "PLA_MCO")
