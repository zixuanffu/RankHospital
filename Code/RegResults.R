rm(list = ls())
source("Code/RegX.R")

# For registered nurses
dt <- readRDS("Data/Out/combineddata_2016_2022.rds")
# hospitals that have changed status
FI_stat_change <- unique(dt[, .(FI, STJR)])
FI_stat_change <- FI_stat_change[, .N, by = .(FI)]
FI_stat_change <- FI_stat_change[N > 1]
dt <- dt[!FI %in% FI_stat_change$FI]
# those observations with possible coding errors...
# 2022 760000166
# 2016 910001973
dt <- dt[!(FI == 760000166 & AN == 2022) | !(FI == 910001973 & AN == 2016)]
# dt[,Y:=ETP_INF*EFF_MD]
dt <- dt[ETP_INF > 0]
varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")
varl <- "ETP_INF"
dt <- dt[SEJHC_MCO > 1 | SEJHP_MCO > 1 | SEANCES_MED > 1]
dt[, (varr1) := lapply(.SD, function(x) x <- x + 1), .SDcols = varr1]
dt[, Nobs := .N, by = .(FI)]
dt <- dt[Nobs == 6]
length(unique(dt$FI))
# dt[, (varr1) := lapply(.SD, function(x) ifelse(x == 0, 1, x)), .SDcols = varr1] # add one to ever column to avoid zero value issue

pdt <- panel(dt, panel.id = ~ FI + AN, time.step = "consecutive", duplicate.method = "first")
# construct lagged variables
# pdt[, `:=`(SEJHC_MCO_l1 = l(SEJHC_MCO, 1), SEJHP_MCO_l1 = l(SEJHP_MCO, 1), SEANCES_MED_l1 = l(SEANCES_MED, 1))]



# OLS regression
reg_inf_ols_FI <- reg_X(pdt, varl, varr1)
summary(reg_inf_ols_FI)
# add residuals to the panel
obs_removed <- reg_inf_ols_FI$obs_selection$obsRemoved * (-1)
pdt_used <- pdt[!obs_removed, ]
residuals <- reg_inf_ols_FI$residuals
pdt_used[, `:=`(Res = residuals)] # add residuals to the panel
# add fixed effect to the panel
FE <- reg_inf_ols_FI$sumFE
pdt_used[, `:=`(FixedEffect = FE)]
saveRDS(pdt_used, "Results/2016-2022/pdt_inf_ols.rds")
saveRDS(reg_inf_ols_FI, "Results/2016-2022/reg_inf_ols_FI.rds")
# plot
reg_inf_ols_FI <- readRDS("Results/2016-2022/reg_inf_ols_FI.rds")
status <- readRDS("Data/Out/status_stable_2016_2022.rds")
plot_FE(reg_inf_ols_FI, "FI", status)


# dropping CHU and PNL
reg_inf_ols_FI_pub <- reg_X(pdt[STJR == 1 | STJR == 2], varl, varr1)
summary(reg_inf_ols_FI_pub)
plot_FE(reg_inf_ols_FI_pub, "FI", status)

# For medical doctors
reg_md_ols_FI <- reg_X(pdt, "EFF_MD", varr1)
# add residuals to the panel
obs_removed <- reg_md_ols_FI$obs_selection$obsRemoved * (-1)
pdt_used <- pdt[!obs_removed, ]
residuals <- reg_md_ols_FI$residuals
pdt_used[, `:=`(Res = residuals)] # add residuals to the panel
# add fixed effect to the panel
FE <- reg_md_ols_FI$sumFE
pdt_used[, `:=`(FixedEffect = FE)]
saveRDS(pdt_used, "Results/2016-2022/pdt_md_ols.rds")
saveRDS(reg_md_ols_FI, "Results/2016-2022/reg_md_ols_FI.rds")

# plot
reg_md_ols_FI <- readRDS("Results/2016-2022/reg_md_ols_FI.rds")
status <- readRDS("Data/Out/status_stable_2016_2022.rds")
plot_FE(reg_md_ols_FI, "FI", status)



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
