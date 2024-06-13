rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")

# ---- load the dataset ---- #
dt1 <- readRDS("Data/Out/combineddata_2016_2022.rds") # 2016-2022
dt2 <- readRDS("Data/Out/combineddata_2013_2015.rds") # 2013-2015
id <- c("AN", "FI", "FI_EJ", "STJR")
input <- c("ETP_INF", "EFF_MD")
output <- c(
    "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "ENTSSR", "SEJ_HAD",
    "LIT_MCO", "PLA_MCO"
)
control <- c("CASEMIX", "CANCER", "TEACHING", "RESEARCH")
cols <- c(id, input, output, control) # variables of interest
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
varr1 <- output
varl <- "ETP_INF"
dt_inf <- dt_inf[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt_inf[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_inf[, Nobs := .N, by = .(FI)]
dt_inf <- dt_inf[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 1526
# check if there are duplicates
# check<-dt_inf[, .N, by = .(AN,FI)] # 330780503 # 370007569
saveRDS(dt_inf, "Data/Out/dt_inf.rds")

# ---- prepare the formula ---- #
dt_inf <- readRDS("Data/Out/dt_inf.rds")
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varr2 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "PLA_MCO")
varr3 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "PLA_MCO", "ENTSSR", "SEJ_HAD")
varl <- "ETP_INF"
rhs1 <- paste(c(add_log(varr1), "CASEMIX-1"), collapse = " + ")
rhs2a <- paste(c(add_log(varr2), "CANCER", "CASEMIX-1"), collapse = " + ")
rhs2b <- paste(c(add_log(varr2), "CANCER", "log(PLA_MCO)*CASEMIX", "CASEMIX -1"), collapse = " + ")
rhs3 <- paste(c(add_log(varr3), "CANCER", "log(PLA_MCO)*CASEMIX", "CASEMIX-1"), collapse = " + ")

lhs <- add_log(varl)
formula1 <- as.formula(paste(lhs, "~", rhs1))
formula1
formula2a <- as.formula(paste(lhs, "~", rhs2a))
formula2a
formula2b <- as.formula(paste(lhs, "~", rhs2b))
formula2b
formula3 <- as.formula(paste(lhs, "~", rhs3))
formula3

# introduce instrumental variables
varl <- "ETP_INF"
lhs <- add_log(varl)
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
add_log(varr1)
paste(c(add_log(varr1), "CASEMIX"), collapse = " + ")
add_lag(add_log(varr1), "2:99")
rhs_x <- paste(c(add_log(varr1), "CASEMIX"), collapse = " + ")
rhs_z <- paste(c(add_lag(add_log(c(varr1)), "2")), collapse = " + ")
rhs <- paste0(rhs_x, " | ", rhs_z) # " + lag(log(ETP_INF),1)",
rhs
formula_gmm <- as.formula(paste(lhs, "~", rhs))
formula_gmm
gmm <- pgmm(formula_gmm, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(gmm)
help(pgmm)
help(sargan)
# ---- estimate the regression model ---- #

# ---- 1. assume strict exogeneity ---- #

zz_fd <- plm(formula1, data = dt_inf, index = c("FI", "AN"), model = "fd")
summary(zz_fd)


RegX_exo <- function(formula, dt) {
    # ---- fixed effects-within group estimator ---- #

    # need to calculate the correct R squared
    zz_wg <- plm(formula, data = dt, index = c("FI", "AN"), model = "within")
    se_zz_wg <- vcovHC(zz_wg, method = c("arellano"), cluster = "group")

    zz_wg_gls <- pggls(formula, data = dt, index = c("FI", "AN"), effect = "individual", model = "within")
    se_zz_wg_gls <- vcov(zz_wg_gls, method = c("arellano"), cluster = "group")

    # ---- fixed effects-first difference estimator ---- #

    # need to calculate the correct R squared
    zz_fd <- plm(formula, data = dt, index = c("FI", "AN"), model = "fd")
    se_zz_fd <- vcovHC(zz_fd, method = c("arellano"), cluster = "group")

    zz_fd_gls <- pggls(formula, data = dt, index = c("FI", "AN"), effect = "individual", model = "fd")
    se_zz_fd_gls <- vcov(zz_fd_gls, method = c("arellano"), cluster = "group")

    return(list(wg = zz_wg, se_wg = se_zz_wg, wg_gls = zz_wg_gls, se_wg_gls = se_zz_wg_gls, fd = zz_fd, se_fd = se_zz_fd, fd_gls = zz_fd_gls, se_fd_gls = se_zz_fd_gls))
}

# ---- specification 1 ---- #
res1 <- RegX_exo(formula1, dt_inf)
models1 <- list(extract.plm(res1$wg, vcov = res1$se_wg), extract.pggls(res1$wg_gls, vcov = res1$se_wg_gls), extract.plm(res1$fd, vcov = res1$se_fd), extract.pggls(res1$fd_gls, vcov = res1$se_fd_gls))
htmlreg(models1, star.symbol = "*", caption = "Estimation results", digits = 4, custom.model.names = c("Within-group", "Within-group (GLS)", "First difference", "First difference (GLS)"), file = "Tables/2013-2022/reg_inf_m1.html")

# ---- specification 2a ---- #
res2a <- RegX_exo(formula2a, dt_inf)
models2a <- list(extract.plm(res2a$wg, vcov = res2a$se_wg), extract.pggls(res2a$wg_gls, vcov = res2a$se_wg_gls), extract.plm(res2a$fd, vcov = res2a$se_fd), extract.pggls(res2a$fd_gls, vcov = res2a$se_fd_gls))
htmlreg(models2a, star.symbol = "*", caption = "Estimation results", digits = 4, custom.model.names = c("Within-group", "Within-group (GLS)", "First difference", "First difference (GLS)"), file = "Tables/2013-2022/reg_inf_m2a.html")

# ---- specification 2b ---- #
res2b <- RegX_exo(formula2b, dt_inf)
models2b <- list(extract.plm(res2b$wg, vcov = res2b$se_wg), extract.pggls(res2b$wg_gls, vcov = res2b$se_wg_gls), extract.plm(res2b$fd, vcov = res2b$se_fd), extract.pggls(res2b$fd_gls, vcov = res2b$se_fd_gls))
htmlreg(models2b, star.symbol = "*", caption = "Estimation results", digits = 4, custom.model.names = c("Within-group", "Within-group (GLS)", "First difference", "First difference (GLS)"), file = "Tables/2013-2022/reg_inf_m2b.html")

# ---- specification 3 ---- #
res3 <- RegX_exo(formula3, dt_inf)
models3 <- list(extract.plm(res3$wg, vcov = res3$se_wg), extract.pggls(res3$wg_gls, vcov = res3$se_wg_gls), extract.plm(res3$fd, vcov = res3$se_fd), extract.pggls(res3$fd_gls, vcov = res3$se_fd_gls))
htmlreg(models3, star.symbol = "*", caption = "Estimation results", digits = 4, custom.model.names = c("Within-group", "Within-group (GLS)", "First difference", "First difference (GLS)"), file = "Tables/2013-2022/reg_inf_m3.html")

# ---- compare with fixest results ---- #
start_year <- 2013
end_year <- 2022
formula3 <- as.formula(paste(lhs, "~", rhs3, "|FI"))
zz_fixest <- feols(formula3, data = dt_inf, cluster = "FI")

obs_removed <- zz_fixest$obs_selection$obsRemoved * (-1) # get the observations removed
pdt_used <- dt_inf[!obs_removed, ] # keep only the observations used

# ---- add residuals to the panel
RES <- zz_fixest$residuals
pdt_used[, `:=`(Res = RES)] # add residuals to the panel
# ---- add fixed effect to the panel
FE <- zz_fixest$sumFE
pdt_used[, `:=`(FixedEffect = FE)]

dir.create(paste0("Resu1lts/", start_year, "-", end_year, "/Spec3"), showWarnings = FALSE)
saveRDS(pdt_used, paste0("Results/", start_year, "-", end_year, "/Spec3/pdt_inf_ols_FI.rds"))
saveRDS(zz_fixest, paste0("Results/", start_year, "-", end_year, "/Spec3/reg_inf_ols_FI.rds"))
# ---- plot the fixed effect ---- #
reg_inf_ols_FI <- readRDS(paste0("Results/", start_year, "-", end_year, "/Spec3/reg_inf_ols_FI.rds"))
status_stable <- readRDS(paste0("Data/Out/status_stable_", start_year, "_", end_year, ".rds"))
p_res <- plot_FE(reg_inf_ols_FI, "FI", status_stable,
    year_start = start_year, year_end = end_year,
    filename = "Figures/2013-2022/Spec3/FixedEffect_zz_fixest", format = "png"
)
p <- p_res[[1]]
p_e <- p_res[[2]]
print(p)


# ---- 2. assume some feedback ---- #

# ---- test the functions used ---- #
# past errors affect future inputs

data("EmplUK", package = "plm")

## Arellano and Bond (1991), table 4 col. b
z1 <- pgmm(
    log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
        + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
    data = EmplUK, effect = "twoways", model = "twosteps"
)
summary(z1, robust = FALSE)

## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
z2 <- pgmm(
    log(emp) ~ lag(log(emp), 1) + lag(log(wage), 0:1) +
        lag(log(capital), 0:1) | lag(log(emp), 2:99) +
        lag(log(wage), 2:99) + lag(log(capital), 2:99),
    data = EmplUK, effect = "twoways", model = "onestep",
    transformation = "ld"
)
summary(z2, robust = TRUE)



# replicate Baltagi (2005, 2013), table 7.4; Baltagi (2021), table 7.5
data("Wages", package = "plm")
ht <- plm(
    lwage ~ wks + south + smsa + married + exp + I(exp^2) +
        bluecol + ind + union + sex + black + ed |
        bluecol + south + smsa + ind + sex + black |
        wks + married + union + exp + I(exp^2),
    data = Wages, index = 595,
    random.method = "ht", model = "random", inst.method = "baltagi"
)
summary(ht)

# ----
