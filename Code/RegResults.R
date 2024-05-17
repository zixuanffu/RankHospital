source("Code/RegX.R")

# For registered nurses
dt <- readRDS("Data/Out/combineddata_2016_2022.rds")
FI_stat_change <- unique(dt[, .(FI, STJR)])
FI_stat_change <- FI_stat_change[, .N, by = .(FI)]
FI_stat_change <- FI_stat_change[N > 1]
dt <- dt[!FI %in% FI_stat_change$FI]

pdt <- panel(dt, panel.id = ~ FI + AN, time.step = "consecutive", duplicate.method = "first")
pdt[, `:=`(SEJHC_MCO_l1 = l(SEJHC_MCO, 1), SEJHP_MCO_l1 = l(SEJHP_MCO, 1), SEANCES_MED_l1 = l(SEANCES_MED, 1))]

varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
reg_inf_ols_FI <- reg_X(pdt, "ETP_INF", varr1)
# add residuals to the panel
obs_removed <- reg_inf_ols_FI$obs_selection$obsRemoved * (-1)
pdt_used <- pdt[!obs_removed, ]
residuals <- reg_inf_ols_FI$residuals
pdt_used[, `:=`(Res = residuals)] # add residuals to the panel
# add fixed effect to the panel
FE <- reg_inf_ols_FI$sumFE
pdt_used[, `:=`(FixedEffect = FE)]
saveRDS(pdt_used, "Results/2016-2022/pdt_ols.rds")
saveRDS(reg_inf_ols_FI, "Results/2016-2022/reg_inf_ols_FI.rds")

# For medical doctors
reg_md_olf_FI <- reg_X(pdt, "EFF_MD", varr1)
saveRDS(reg_md_olf_FI, "Results/2016-2022/reg_md_olf_FI.rds")
