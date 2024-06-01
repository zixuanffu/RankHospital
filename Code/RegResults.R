rm(list = ls())
source("Code/RegX.R")
# ---- load the dataset ---- #
dt1 <- readRDS("Data/Out/combineddata_2016_2022.rds") # 2016-2022
dt2 <- readRDS("Data/Out/combineddata_2013_2015.rds") # 2013-2015
cols <- c("AN", "FI", "FI_EJ", "EFF_MD", "ETP_INF", "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CASEMIX", "STJR") # variables of interest
dt <- rbind(dt1[, ..cols], dt2[, ..cols])

# ---- select only those with stable legal status ---- #
FI_stat_change <- unique(dt[, .(FI, STJR)])
FI_stat_change <- FI_stat_change[, .N, by = .(FI)]
FI_stat_change <- FI_stat_change[N > 1]
dt <- dt[!FI %in% FI_stat_change$FI]

# ---- Prepare the panel for nurses ---- #
# ---- filter out observations with possible coding errors
# 2022 760000166
# 2016 910001973
dt_inf <- dt[!(FI == 760000166 & AN == 2022) | !(FI == 910001973 & AN == 2016)]
# ---- filter out zero values on the LHS ---- #
dt_inf <- dt_inf[ETP_INF > 0]
# ---- add one to the RHS to avoid zero values in taking log ---- #
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varl <- "ETP_INF"
dt_inf <- dt_inf[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt_inf[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_inf[, Nobs := .N, by = .(FI)]
dt_inf <- dt_inf[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 1526

# ---- set data.table in panel data format ---- #
pdt <- panel(dt_inf, panel.id = ~ FI + AN, time.step = "consecutive", duplicate.method = "first")
# construct lagged variables
# pdt[, `:=`(SEJHC_MCO_l1 = l(SEJHC_MCO, 1), SEJHP_MCO_l1 = l(SEJHP_MCO, 1), SEANCES_MED_l1 = l(SEANCES_MED, 1))]

# ---- Regression ---- #
start_year <- 2013
end_year <- 2022
# ---- OLS regression with all types of hospitals ---- #
# ---- With nurses ---- #
reg_inf_ols_FI <- reg_X(pdt, varl, varr1)
summary(reg_inf_ols_FI)
str(reg_inf_ols_FI)

obs_removed <- reg_inf_ols_FI$obs_selection$obsRemoved * (-1) # get the observations removed
pdt_used <- pdt[!obs_removed, ] # keep only the observations used

# ---- add residuals to the panel
RES <- reg_inf_ols_FI$residuals
pdt_used[, `:=`(Res = RES)] # add residuals to the panel
# ---- add fixed effect to the panel
FE <- reg_inf_ols_FI$sumFE
pdt_used[, `:=`(FixedEffect = FE)]

saveRDS(pdt_used, paste0("Results/", start_year, "-", end_year, "/pdt_inf_ols_FI.rds"))
saveRDS(reg_inf_ols_FI, paste0("Results/", start_year, "-", end_year, "/reg_inf_ols_FI.rds"))
# ---- plot the fixed effect ---- #
reg_inf_ols_FI <- readRDS(paste0("Results/", start_year, "-", end_year, "/reg_inf_ols_FI.rds"))
status_stable <- readRDS(paste0("Data/Out/status_stable_", start_year, "_", end_year, ".rds"))
p_res <- plot_FE(reg_inf_ols_FI, "FI", status_stable, year_start = start_year, year_end = end_year)
p <- p_res[[1]]
p_e <- p_res[[2]]

# ---- OLS regression with only public and private hospitals ---- #
reg_inf_ols_FI_pub <- reg_X(pdt[STJR == 1 | STJR == 2], varl, varr1)
summary(reg_inf_ols_FI_pub)
p_res <- plot_FE(reg_inf_ols_FI_pub, "FI", status_stable, year_start = start_year, year_end = end_year)
p <- p_res[[1]]
p_e <- p_res[[2]]


# ---- Prepare the panel for doctors ---- #
# ---- filter out observations with possible coding errors
dt_md <- dt[!(FI == 760000166 & AN == 2022) | !(FI == 910001973 & AN == 2016)]
# ---- filter out zero values on the LHS ---- #
dt_md <- dt_md[EFF_MD > 0]
# ---- add one to the RHS to avoid zero values in taking log ---- #
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varl <- "EFF_MD"
dt_md <- dt_md[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt_md[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt_md[, Nobs := .N, by = .(FI)]
dt_md <- dt_md[Nobs >= 6] # keep only those with at least 6 observations
num_hospital <- length(unique(dt$FI)) # 2231

# ---- set data.table in panel data format ---- #
pdt <- panel(dt_md, panel.id = ~ FI + AN, time.step = "consecutive", duplicate.method = "first")
# construct lagged variables
# pdt[, `:=`(SEJHC_MCO_l1 = l(SEJHC_MCO, 1), SEJHP_MCO_l1 = l(SEJHP_MCO, 1), SEANCES_MED_l1 = l(SEANCES_MED, 1))]

varl <- "EFF_MD"
reg_md_ols_FI <- reg_X(pdt, varl, varr1)
summary(reg_md_ols_FI)
# ---- remove observations that are not used in the regression ---- #
obs_removed <- reg_md_ols_FI$obs_selection$obsRemoved * (-1)
pdt_used <- pdt[!obs_removed, ]
# ---- add residuals to the panel
residuals <- reg_md_ols_FI$residuals
pdt_used[, `:=`(Res = residuals)] # add residuals to the panel
# ---- add fixed effect to the panel
FE <- reg_md_ols_FI$sumFE
pdt_used[, `:=`(FixedEffect = FE)]
saveRDS(pdt_used, paste0("Results/", start_year, "-", end_year, "/pdt_md_ols_FI.rds"))
saveRDS(reg_md_ols_FI, paste0("Results/", start_year, "-", end_year, "/reg_md_ols_FI.rds"))

# ---- plot the fixed effect by legal status ---- #
reg_md_ols_FI <- readRDS(paste0("Results/", start_year, "-", end_year, "/reg_md_ols_FI.rds"))
status_stable <- readRDS(paste0("Data/Out/status_stable_", start_year, "_", end_year, ".rds"))
p_res <- plot_FE(reg_md_ols_FI, "FI", status_stable, year_start = start_year, year_end = end_year)
p <- p_res[[1]]
p_e <- p_res[[2]]

# ---- IGNORE: Preliminary test with Poisson regression ----- #
# Multiplicative model Pseudo Poisson regression
# If we actually assume Poisson distribution of the multiplicative error term, we are in the case of heterogeneous known variance, where the variance stabilizing transformation gives $z_{it} =\sqrt{\frac{y_it}{m_{it}}} \sim \caln (\theta_i, \frac{1}{w_{it}}= \frac{1}{4m_{it}})$

# 1. estimate m_{it}
reg_inf_pois_FI <- reg_X(pdt, varl, varr1, method = "pois")
summary(reg_inf_pois_FI)
check <- data.table()
check[, test1 := reg_inf_pois_FI$fitted.values + reg_inf_pois_FI$residuals]
check[, y := pdt$ETP_INF]

coef <- as.matrix(reg_inf_pois_FI$coefficients)
x <- as.matrix(pdt[, .(log(SEJHC_MCO), log(SEJHP_MCO), log(SEANCES_MED), CASEMIX)])
m_it <- exp(x %*% coef)

check[, test2 := m_it * exp(reg_inf_pois_FI$sumFE) + reg_inf_pois_FI$residuals]
check[, fe := exp(reg_inf_pois_FI$sumFE)]
check[, m_it := m_it]
View(check)
# this is so stupid...it's like everything is fixed effect...

panel <- data.table(FI = pdt$FI, AN = pdt$AN, m_it = m_it, y_it = pdt$ETP_INF, FE = exp(reg_inf_pois_FI$sumFE))
# 2. construct z_{it} =\sqrt{\frac{y_it}{m_{it}}}
panel[, `:=`(w_it = (4 * m_it))]
panel[, `:=`(z_it = sqrt(y_it / m_it))]
# 3. construct T_i= \sum w_{it}z_{it}/w_i \sim N(\theta_i, 1/w_i)
T_i <- panel[, .(T_i = sum(z_it / w_it) / sum(w_it), FE, w_i = sum(w_it)), by = .(FI)]
View(T_i[, .(T_i, 1 / sqrt(w_i))])
saveRDS(T_i, "Results/2016-2022/T_i.rds")
# 3.2 construct a data.frame to feed into the fit1d function

# to transform the data.frame into a matrix of 3d dimension [1:1526,1:6,1:2]
d <- panel[, .(FI, AN, w_it, z_it, y_it, m_it)]
d_vec <- as.vector(t(d[, .(y_it, m_it)]))
dim1 <- length(unique(d$FI))
dim2 <- length(unique(d$AN))
dim3 <- 2
dim <- c(dim1, dim2, dim3)
d_mat <- array(d_vec, dim)
saveRDS(d_mat, "Results/2016-2022/d_mat.rds")

# 4. NPMLE to estimate G while the variance is known and is equal to 1/4m_{it}
# We can input the T_i$T_i and variance as 1/T_i$w_i
# Then we can estimate G by NPMLE
pacman::p_load(REBayes)
# help(GLmix)
f <- GLmix(x = T_i$T_i, v = 300, sigma = 1 / sqrt(T_i$w_i))
str(f)
pdf("Figures/2016-2022/GLmix_pois.pdf", width = 8, height = 5)
plot(f, xlab = expression(mu), main = "Estimated Location Mixing Density")
dev.off()

fs <- KWsmooth(f, bw)
pdf("Figures/2016-2022/GLmix_pois_smoothed.pdf", width = 8, height = 5)
plot(fs, xlab = expression(mu), main = "Estimated Smoothed Location Mixing Density")
dev.off()

# Note: The distribution of the statistics T_i is a convolution of G and the normal distribution N(\theta_i, 1/w_i))
