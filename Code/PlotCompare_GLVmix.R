rm(list = ls())
pacman::p_load(REBayes)
source("Code/SelectX_GLVmix.R")

pdt <- readRDS("Results/2013-2022/pdt_used_gmm_sys.rds")
Z <- fit2d(pdt)
alpha <- 0.2
gamma <- 0.2
Z <- append(Z, list(W = Z$s))
sL <- select2d(Z, alpha = alpha, gamma = gamma, tail = "L")
sR <- select2d(Z, alpha = alpha, gamma = gamma, tail = "R")
apply(sL$A, 2, sum)
apply(sL$B, 2, sum)

u_min <- min(Z$S)
u_max <- max(Z$S)
v_min <- min(Z$W)
v_max <- max(Z$W)

# ---- Compare selection results given by different rules ---- #

dir.create("Figures/2013-2022/GMM/GLVmix", showWarnings = FALSE)
pdf("Figures/2013-2022/GMM/GLVmix/Right_0.20_0.10.pdf", height = 4.50 * 3, width = 8.00)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot_2d(Z, sR, alpha = alpha, gamma = gamma, tail = "R", cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200), xlim = c(u_min, u_max), ylim = c(v_min, v_max))
}
dev.off()

pdf("Figures/2013-2022/GMM/GLVmix/Left_0.20_0.20.pdf", height = 4.50 * 3, width = 8.00)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot_2d(Z, sL, alpha = alpha, gamma = gamma, tail = "L", cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200), xlim = c(u_min, u_max), ylim = c(v_min, v_max))
}
dev.off()
