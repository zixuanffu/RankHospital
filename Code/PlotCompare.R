rm(list = ls())
pacman::p_load(REBayes)
source("Code/SelectX.R")
load("Results/2013-2022/ZsLsR.Rda")
Z <- append(Z, list(W = Z$s))
# ---- Find the min and max of the density ---- #
u_min <- min(Z$S)
u_max <- max(Z$S)
v_min <- min(Z$W)
v_max <- max(Z$W)


# --- Right tail --- #

# --- pdf --- #
pdf("Figures/2013-2022/TPKWs_MLE_R.pdf", height = 4.5, width = 8)
par(mfrow = c(1, 2))
TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

pdf("Figures/2013-2022/TPKWs_PMKWs_R.pdf", height = 4.5, width = 8)
par(mfrow = c(1, 2))
TPKWs_PMKWs <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

pdf("Figures/2013-2022/TPKWs_JS_R.pdf", height = 4.5, width = 8)
par(mfrow = c(1, 2))
TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

# --- png --- #
png("Figures/2013-2022/TPKWs_MLE_R.png", height = 450, width = 800)
par(mfrow = c(1, 2))
TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

png("Figures/2013-2022/TPKWs_PMKWs_R.png", height = 450, width = 800)
par(mfrow = c(1, 2))
TPKWs_PMKWs <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

png("Figures/2013-2022/TPKWs_JS_R.png", height = 450, width = 800)
par(mfrow = c(1, 2))
TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

# --- Left tail --- #

png("Figures/2013-2022/TPKWs_MLE_L.pdf", height = 450, width = 800)
par(mfrow = c(1, 2))
TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.2, tail = "L", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()
png("Figures/2013-2022/TPKWs_JS_L.pdf", height = 450, width = 800)
par(mfrow = c(1, 2))
TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()
