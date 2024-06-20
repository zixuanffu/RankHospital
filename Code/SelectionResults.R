# Z$S is the statistics that follow the normal distribution with mean unknown and variance estimated (assume to be the true variance) the mean is to be estimated (knowning the G), which gives posterior mean. the mean is to be used in computed the probability that it is larger than a certain value, which gives the posterior tail probability.
# All that is needed is Z$f Z$fs  Z$S which is the statistics Z$s which is the estimated known variance

rm(list = ls())
pacman::p_load(data.table, REBayes, lattice, ggplot2, docstring)
source("Code/SelectX.R")
# ---- load regression results ---- #


library(data.table)
pdt <- readRDS("Results/2013-2022/pdt_used_gmm_sys_m.rds")
Z <- fit1d(pdt)

sL <- selectL1d(Z, alpha = 0.22, gamma = 0.05)
sL <- select1d(Z, alpha = 0.22, gamma = 0.05, "L")
sR <- selectR1d(Z, alpha = 0.22, gamma = 0.05)
sR <- select1d(Z, alpha = 0.22, gamma = 0.05, "R")
save(Z, sL, sR, file = "Results/2013-2022/ZsLsR.Rda")



# ---- extra plot for estimated G ---- #

pdf("Figures/2013-2022/GMM_m/GLmix.pdf", height = 5, width = 8)
plot(Z$f, xlab = expression(theta), main = "Estimated Location Mixing Density with Heterogeneous Known Variance")
dev.off()

pdf("Figures/2013-2022/GMM_m/GLmix_s.pdf", height = 5, width = 8)
plot(Z$fs, xlab = expression(theta), main = "Estimated Location smoothed G with Heterogenous Known Variance")
dev.off()

apply(sL$A, 2, sum)
apply(sL$B, 2, sum)

apply(sR$A, 2, sum)
apply(sR$B, 2, sum)
