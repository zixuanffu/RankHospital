# Regression with 5 years of value 2017 2018 2019 2021 2022
rm(list = ls())
pacman::p_load(data.table, fixest, ggplot2)

dt <- readRDS("Data/Out/combineddata_2016_2022.rds")
FI_stat_change <- unique(dt[, .(FI, STJR)])
FI_stat_change <- FI_stat_change[, .N, by = .(FI)]
FI_stat_change <- FI_stat_change[N > 1]
dt <- dt[!FI %in% FI_stat_change$FI]
dt$FI <- as.factor(dt$FI)
dt$AN <- as.numeric(dt$AN)
dt[is.na(dt)] <- 0

pdt <- panel(dt, panel.id = ~ FI + AN, time.step = "consecutive", duplicate.method = "first")
pdt[, `:=`(SEJHC_MCO_l1 = l(SEJHC_MCO, 1), SEJHP_MCO_l1 = l(SEJHP_MCO, 1), SEANCES_MED_l1 = l(SEANCES_MED, 1))]
varl <- c("EFF_MD", "EFF_INF", "EFF_AID", "EFF_TOT_HORS_SOINS")

varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD", "SEJ_HTP_TOT", "VEN_TOT")
varr1_log <- c()
i <- 1
for (j in varr1) {
    varr1_log[i] <- paste0("log(", j, ")")
    i <- i + 1
}

varr1_less <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varr1_less_log <- c()
i <- 1
for (j in varr1_less) {
    varr1_less_log[i] <- paste0("log(", j, ")")
    i <- i + 1
}

## Section 1: regression
setFixest_fml(..RHS_less = ~ log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED))
setFixest_fml(..RHS_IV = ~ log(SEJHC_MCO_l1) + log(SEJHP_MCO_l1) + log(SEANCES_MED_l1))

# Model 1: Pseudo Poisson
# 1.Pseudo poisson with log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED)+CASEMIX
reg_inf_pois_FI <- fepois(ETP_INF ~ ..RHS_less + CASEMIX | FI, data = pdt, vcov = ~FI)
reg_inf_pois_FIEJ <- fepois(ETP_INF ~ ..RHS_less + CASEMIX | FI_EJ, data = pdt, vcov = ~FI_EJ)
etable(reg_inf_pois_FI, reg_inf_pois_FIEJ)
etable(reg_inf_pois_FI, reg_inf_pois_FIEJ, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = "Tables/2016-2022/reg_inf_pois.tex", signif.code = "letters", replace = TRUE)

# 2. one year lagged value with log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED)+CASEMIX
reg_inf_pois_lag_FI <- fepois(ETP_INF ~ CASEMIX | FI | ..RHS_less ~ ..RHS_IV, data = pdt, vcov = ~FI)
reg_inf_pois_lag_FIEJ <- fepois(ETP_INF ~ CASEMIX | FI_EJ | ..RHS_less ~ ..RHS_IV, data = pdt, vcov = ~FI_EJ)
etable(reg_inf_pois_lag_FI, reg_inf_pois_lag_FIEJ)
etable(reg_inf_pois_lag_FI, reg_inf_pois_lag_FIEJ, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = "Tables/2016-2022/reg_inf_pois_lag.tex", signif.code = "letters", replace = TRUE)

# Model 2: Log OLS

reg_inf_ols_FI <- feols(log(ETP_INF) ~ ..RHS_less + CASEMIX | FI, data = pdt, vcov = ~FI)
reg_inf_ols_FIEJ <- feols(log(ETP_INF) ~ ..RHS_less + CASEMIX | FI_EJ, data = pdt, vcov = ~FI_EJ)
etable(reg_inf_ols_FI, reg_inf_ols_FIEJ)
etable(reg_inf_ols_FI, reg_inf_ols_FIEJ, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = "Tables/2016-2022/reg_inf_ols.tex", signif.code = "letters", replace = TRUE)

# 2. one year lagged value with log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED)+CASEMIX

reg_inf_lag_FI <- feols(log(ETP_INF) ~ CASEMIX | FI | ..RHS_less ~ ..RHS_IV, data = pdt, vcov = ~FI)
reg_inf_lag_FIEJ <- feols(log(ETP_INF) ~ CASEMIX | FI_EJ | ..RHS_less ~ ..RHS_IV, data = pdt, vcov = ~FI_EJ)
etable(reg_inf_lag_FI, reg_inf_lag_FIEJ)
etable(reg_inf_lag_FI, reg_inf_lag_FIEJ, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = "Tables/2016-2022/reg_inf_lag.tex", signif.code = "letters", replace = TRUE)

# what about with only PUB and PLU hostpitals?
pdt_pub <- pdt[STJR == 1 | STJR == 2]
reg_inf_ols_FI_pub <- feols(log(ETP_INF) ~ ..RHS_less + CASEMIX | FI, data = pdt_pub, vcov = ~FI)
reg_inf_ols_FIEJ_pub <- feols(log(ETP_INF) ~ ..RHS_less + CASEMIX | FI_EJ, data = pdt_pub, vcov = ~FI_EJ)
etable(reg_inf_ols_FI_pub, reg_inf_ols_FIEJ_pub)



## Section 2: fixed effect
FE_ols_FI <- fixef(reg_inf_ols_FI)
# plot(FE_ols_FI)
dt_ols_FI <- data.table(FI = names(FE_ols_FI$FI), FixedEffect = unlist(FE_ols_FI$FI))
status <- readRDS("Data/Out/status_2016_2022.rds")
status <- unique(status[, c("FI", "FI_EJ", "STJR", "STJR_LABEL")])
setkey(dt_ols_FI, FI)
setkey(status, FI)
dt_ols_FI_status <- dt_ols_FI[status[, .(FI, STJR, STJR_LABEL)], on = "FI", nomatch = 0]
dt_ols_FI_status[, Rank := rank(FixedEffect)]
p_olg_FI <- ggplot(dt_ols_FI_status, aes(x = Rank, y = FixedEffect)) +
    geom_point(aes(color = STJR_LABEL), size = 1) +
    theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
ggsave("Figures/2016-2022/FE_ols_FI.pdf", p_olg_FI, width = 6, height = 4, dpi = 300)
ggsave("Figures/2016-2022/FE_ols_FI.png", p_olg_FI, width = 6, height = 4, dpi = 300)

p_olg_FI_e <- ggplot(dt_ols_FI_status, aes(x = Rank, y = exp(FixedEffect))) +
    geom_point(aes(color = STJR_LABEL), size = 1) +
    theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
ggsave("Figures/2016-2022/FE_ols_FI_e.pdf", p_olg_FI_e, width = 6, height = 4, dpi = 300)
ggsave("Figures/2016-2022/FE_ols_FI_e.png", p_olg_FI_e, width = 6, height = 4, dpi = 300)

FE_ols_FIEJ <- fixef(reg_inf_ols_FIEJ)
dt_ols_FIEJ <- data.table(FI_EJ = names(FE_ols_FIEJ$FI_EJ), FixedEffect = unlist(FE_ols_FIEJ$FI_EJ))
dt_ols_FIEJ_status <- dt_ols_FIEJ[status[, .(FI_EJ, STJR, STJR_LABEL)], on = "FI_EJ", nomatch = 0]
dt_ols_FIEJ_status[, Rank := rank(FixedEffect)]
p_olg_FIEJ <- ggplot(dt_ols_FIEJ_status, aes(x = Rank, y = FixedEffect)) +
    geom_point(aes(color = STJR_LABEL), size = 1) +
    theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
ggsave("Figures/2016-2022/FE_ols_FIEJ.pdf", p_olg_FIEJ, width = 6, height = 4, dpi = 300)
