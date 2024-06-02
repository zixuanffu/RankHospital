# Z$S is the statistics that follow the normal distribution with mean unknown and variance estimated (assume to be the true variance) the mean is to be estimated (knowning the G), which gives posterior mean. the mean is to be used in computed the probability that it is larger than a certain value, which gives the posterior tail probability.
# All that is needed is Z$f Z$fs  Z$S which is the statistics Z$s which is the estimated known variance

rm(list = ls())
pacman::p_load(data.table, REBayes, lattice, ggplot2, docstring)
source("Code/SelectX.R")

# ---- load regression results ---- #

pdt <- readRDS("Results/2013-2022/pdt_inf_ols_FI.rds")

Z <- fit1d(pdt)
sL <- selectL1d(Z, alpha = 0.22, gamma = 0.2)
sR <- selectR1d(Z, alpha = 0.04, gamma = 0.2)
save(Z, sL, sR, file = "Results/2013-2022/ZsLsR.Rda")
