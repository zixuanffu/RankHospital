rm(list = ls())
pacman::p_load(REBayes)
source("Code/SelectX_GLVmix.R")

load("Results/2013-2022/pdt_dt1_dt2.Rda")
Z <- fit2d(pdt)
Z <- append(Z, list(W = Z$s))
sL <- select2d(Z, alpha = 0.22, gamma = 0.10, tail = "L")
sR <- select2d(Z, alpha = 0.22, gamma = 0.10, tail = "R")

u_min <- min(Z$S)
u_max <- max(Z$S)
v_min <- min(Z$W)
v_max <- max(Z$W)

pdf("Figures/2013-2022/UnknownVariance/Right_0.22_0.10.pdf", height = 4.5 * 3, width = 8)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot_2d(Z, sR, alpha = 0.22, gamma = 0.10, tail = "R", cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200), xlim = c(u_min, u_max), ylim = c(v_min, v_max))
}
dev.off()
