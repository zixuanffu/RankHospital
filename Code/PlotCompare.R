rm(list = ls())
pacman::p_load(REBayes)
source("Code/SelectX.R")
load("Results/2013-2022/Spec3/ZsLsR.Rda")
Z <- append(Z, list(W = Z$s)) #
# ---- Find the min and max of the density ---- #
u_min <- min(Z$S)
u_max <- max(Z$S)
v_min <- min(Z$W)
v_max <- max(Z$W)


# # --- Right tail --- #

# # --- pdf --- #
# pdf("Figures/2013-2022/TPKWs_MLE_R.pdf", height = 4.5, width = 8)
# par(mfrow = c(1, 2))
# TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# pdf("Figures/2013-2022/TPKWs_PMKWs_R.pdf", height = 4.5, width = 8)
# par(mfrow = c(1, 2))
# TPKWs_PMKWs <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# pdf("Figures/2013-2022/TPKWs_JS_R.pdf", height = 4.5, width = 8)
# par(mfrow = c(1, 2))
# TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# # --- png --- #
# png("Figures/2013-2022/TPKWs_MLE_R.png", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# png("Figures/2013-2022/TPKWs_PMKWs_R.png", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_PMKWs <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# png("Figures/2013-2022/TPKWs_JS_R.png", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# # --- Left tail --- #

# png("Figures/2013-2022/TPKWs_MLE_L.pdf", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.2, tail = "L", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()
# png("Figures/2013-2022/TPKWs_JS_L.pdf", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# # --- png --- #
# png("Figures/2013-2022/TPKWs_MLE_L.png", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_MLE <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# png("Figures/2013-2022/TPKWs_PMKWs_L.png", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_PMKWs <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()

# png("Figures/2013-2022/TPKWs_JS_L.png", height = 450, width = 800)
# par(mfrow = c(1, 2))
# TPKWs_JS <- level_plot(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
# dev.off()


# --- test: right--- #
grid <- grid_select(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4, 5, 6), xgrid = seq(u_min, u_max, length = 600), ygrid = seq(v_min, v_max, length = 200))
png("Figures/2013-2022/test_R25.png", height = 450, width = 800)
par(mfrow = c(1, 2))
level_plot(Z, sR, grid, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()
png("Figures/2013-2022/test_R24.png", height = 450, width = 800)
par(mfrow = c(1, 2))
level_plot(Z, sR, grid, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

# --- test: left--- #
grid <- grid_select(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 4, 5, 6), xgrid = seq(u_min, u_max, length = 600), ygrid = seq(v_min, v_max, length = 200))
png("Figures/2013-2022/test_L25.png", height = 450, width = 800)
par(mfrow = c(1, 2))
level_plot(Z, sL, grid, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 5), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()
png("Figures/2013-2022/test_L24.png", height = 450, width = 800)
par(mfrow = c(1, 2))
level_plot(Z, sL, grid, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 4), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()
png("Figures/2013-2022/test_L26.png", height = 450, width = 800)
par(mfrow = c(1, 2))
level_plot(Z, sL, grid, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 6), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
dev.off()

# ---- generate plot for right/left selection comparing rule (2, 4) and (2, 5) and (2, 6) ---- #

# ---- left tail ---- #
grid_L <- grid_select(Z, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, 4, 5, 6), xgrid = seq(u_min, u_max, length = 600), ygrid = seq(v_min, v_max, length = 200))
pdf("Figures/2013-2022/Spec3/Left_0.22_0.05.pdf", height = 4.5 * 3, width = 8)
png("Figures/2013-2022/Spec3/Left_0.22_0.05.png", height = 450 * 3, width = 800)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot(Z, sL, grid_L, alpha = 0.22, gamma = 0.05, tail = "L", cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
}
dev.off()

# ---- right tail ---- #
grid_R <- grid_select(Z, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, 4, 5, 6), xgrid = seq(u_min, u_max, length = 600), ygrid = seq(v_min, v_max, length = 200))
pdf("Figures/2013-2022/Spec3/Right_0.22_0.05.pdf", height = 4.5 * 3, width = 8)
png("Figures/2013-2022/Spec3/Right_0.22_0.05.png", height = 450 * 3, width = 800)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot(Z, sR, grid_R, alpha = 0.22, gamma = 0.05, tail = "R", cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
}
dev.off()
