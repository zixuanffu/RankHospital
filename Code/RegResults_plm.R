rm(list = ls())
pacman::p_load(data.table, plm, texreg, fixest)
pacman::p_load(pdynmc)
source("Code/RegX_fixest.R")
source("Code/RegX_plm.R")

# ---- prepare the formula ---- #

# baseline specification
dt_inf <- readRDS("Data/Out/dt_inf.rds")


# formula for WG and FD
# lhs
varl <- "ETP_INF"
lhs <- add_log(varl)
# rhs
varr <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")

# WG
rhs <- paste(c(add_log(varr1), "CASEMIX"), collapse = " + ")
formula_wg <- as.formula(paste(lhs, "~", rhs))
z_wg <- plm(formula_wg, data = dt_inf, index = c("FI", "AN"), model = "within")
z_rd <- plm(formula_wg, data = dt_inf, index = c("FI", "AN"), model = "random")
summary(z_wg, robust = TRUE)
# FD
rhs <- paste(c(add_log(varr), "CASEMIX-1"), collapse = " + ")
formula_fd <- as.formula(paste(lhs, "~", rhs))
formula_fd
z_fd <- plm(formula_fd, data = dt_inf, index = c("FI", "AN"), model = "fd")
summary(z_fd, robust = TRUE)

phtest(z_fd, z_rd)
phtest(z_wg, z_rd)

data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
wi <- plm(form, data = Gasoline, model = "within")
re <- plm(form, data = Gasoline, model = "random")
phtest(wi, re)


# Assume same variance matrix of the errors for all groups
# The usual assumptions is "heteroskedasticity $\varepsilon_{it}$ and serial/auto-correlation within individuals but not across them."
z_fd_gls <- pggls(formula_fd, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "fd")
View(z_fd_gls$sigma)

# formula for AH
formula_ah <- as.formula("log(ETP_INF) ~ log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED) + CASEMIX-1 | lag(log(SEJHC_MCO), 0)+lag(log(SEJHP_MCO), 0)+lag(log(SEANCES_MED), 0)+CASEMIX")
formula_ah
z_ah <- plm(formula_ah, data = dt_inf, index = c("FI", "AN"), model = "fd")
summary(z_ah, robust = TRUE)
# formula for GMM
# lhs
varl <- "ETP_INF"
lhs <- add_log(varl)
# rhs
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
rhs1_x <- paste(c(add_log(varr1), "CASEMIX"), collapse = " + ")
rhs1_z <- paste(c(add_lag(add_log(c(varr1)), "2")), collapse = " + ")
rhs <- paste0(rhs1_x, " | ", rhs1_z)

formual_gmm <- as.formula(paste(lhs, "~", rhs))
formual_gmm
# formula_test<-as.formula("log(ETP_INF) ~ log(SEJHC_MCO) + log(SEJHP_MCO) + log(SEANCES_MED) + CASEMIX | lag(log(SEJHC_MCO), 2)")

# formula for GMM with lagged dependent variable
rhs1_x <- paste(c(add_log(varr1), add_lag(add_log(varl), 1), "CASEMIX"), collapse = " + ")
rhs1_x
rhs1_z <- paste(c(add_lag(add_log(c(varr1, varl)), "2")), collapse = " + ")
rhs1_z
rhs <- paste0(rhs1_x, " | ", rhs1_z)
formula_gmm_y <- as.formula(paste(lhs, "~", rhs))
formula_gmm_y

# Diff GMM
z_gmm_fd <- pgmm(formual_gmm, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twostep", transformation = "d", collapse = TRUE)
summary(z_gmm_fd)

z_gmm_fd_y <- pgmm(formula_gmm_y, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "d", collapse = TRUE)
summary(z_gmm_fd_y, robust = TRUE)

# Sys GMM
z_gmm_sys <- pgmm(formual_gmm, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "ld")
summary(z_gmm_sys, robust = TRUE)
z_gmm_sys_y <- pgmm(formula_gmm_y, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(z_gmm_sys_y, robust = TRUE)

# Full fledged specification
# lhs
varl <- "ETP_INF"
lhs <- add_log(varl)
# rhs
varr <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "PLA_MCO", "LIT_MCO")
rhs1_x <- paste(c(add_log(varr), "CASEMIX", "CANCER"), collapse = " + ")
rhs1_z <- paste(c(add_lag(add_log(c(varr)), "2")), collapse = " + ")
rhs <- paste0(rhs1_x, " | ", rhs1_z)
formula_gmm_full <- as.formula(paste(lhs, "~", rhs))
formula_gmm_full

zz_gmm_sys_full <- pgmm(formula_gmm_full, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(zz_gmm_sys_full, robust = TRUE)

rhs1_x <- paste(c(add_log(varr), add_lag(add_log(varl), 1), "CASEMIX", "CANCER"), collapse = " + ")
rhs1_z <- paste(c(add_lag(add_log(c(varr, varl)), "2")), collapse = " + ")
rhs <- paste0(rhs1_x, " | ", rhs1_z)
formula_gmm_full_y <- as.formula(paste(lhs, "~", rhs))
formula_gmm_full_y
zz_gmm_sys_full_y <- pgmm(formula_gmm_full_y, data = dt_inf, index = c("FI", "AN"), effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(zz_gmm_sys_full_y, robust = TRUE)


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
