rm(list = ls())
pacman::p_load(REBayes, data.table)
source("Code/SelectX.R")
pdt <- readRDS("Results/2013-2022/pdt_used_gmm_sys_m.rds")
Z <- fit1d(pdt)
alpha <- 0.20
gamma <- 0.20
sL <- select1d(Z, alpha = alpha, gamma = gamma, "L")
sR <- select1d(Z, alpha = alpha, gamma = gamma, "R")
# ---- Find the min and max of the density ---- #
Z <- append(Z, list(W = Z$s)) #
u_min <- min(Z$S)
u_max <- max(Z$S)
v_min <- min(Z$W)
v_max <- max(Z$W)

# ---- generate plot for right/left selection comparing rule (2, 4) and (2, 5) and (2, 6) ---- #

# ---- left tail ---- #
tail <- "L"
grid_L <- grid_select(Z, alpha = alpha, gamma = gamma, tail = "L", cindex = c(2, 4, 5, 6), xgrid = seq(u_min, u_max, length = 600), ygrid = seq(v_min, v_max, length = 200))
dir.create("Figures/2013-2022/GMM_m/GLmix", showWarnings = TRUE)
pdf(paste0("Figures/2013-2022/GMM_m/GLmix/Left_", alpha, "_", gamma, ".pdf"), height = 4.5 * 3, width = 8)
# png(paste0("Figures/2013-2022/GMM_m/GLmix/Left_", alpha, "_", gamma, ".png"), height = 450 * 3, width = 800)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot(Z, sL, grid_L, alpha = alpha, gamma = gamma, tail = tail, cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
}
dev.off()

Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
tail <- "L"
for (rule_index in c(4, 5, 6)) {
    filepath <- paste0("Figures/2013-2022/GMM_m/GLmix/Contour_Left_", alpha, "_", gamma, "_", Rules[2], "_", Rules[rule_index], ".pdf")
    print(filepath)
    pdf(filepath, height = 4.5, width = 8)
    par(mfrow = c(1, 2))
    level_plot(Z, sL, grid_L, alpha = alpha, gamma = gamma, tail = "L", cindex = c(2, rule_index), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
    dev.off()
}


# ---- right tail ---- #
tail <- "R"
grid_R <- grid_select(Z, alpha = alpha, gamma = gamma, tail = "R", cindex = c(2, 4, 5, 6), xgrid = seq(u_min, u_max, length = 600), ygrid = seq(v_min, v_max, length = 200))
pdf(paste0("Figures/2013-2022/GMM_m/GLmix/Right_", alpha, "_", gamma, ".pdf"), height = 4.5 * 3, width = 8)
# png(paste0("Figures/2013-2022/GMM_m/GLmix/Right_", alpha, "_", gamma, ".png"), height = 450 * 3, width = 800)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot(Z, sR, grid_R, alpha = alpha, gamma = gamma, tail = tail, cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
}
dev.off()


for (rule_index in c(2, 4, 5)) {
    filepath <- paste0("Figures/2013-2022/GMM_m/GLmix/Contour_Right_", alpha, "_", gamma, "_", Rules[2], "_", Rules[rule_index], ".pdf")
    print(filepath)
    pdf(filepath, height = 4.5, width = 8)
    par(mfrow = c(1, 2))
    level_plot(Z, sR, grid_R, alpha = alpha, gamma = gamma, tail = tail, cindex = c(2, rule_index), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200))
    dev.off()
}

# ---- Focus on left tail probability ---- #

Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
alpha <- 0.20
gamma <- 0.20
tail <- "L"
Z <- fit1d(pdt)
sL <- select1d(Z, alpha = alpha, gamma = gamma, tail = tail)
for (rule_index in c(2, 4, 5, 6)) {
    filepath <- paste0("Figures/2013-2022/GMM_m/GLmix/Left_", alpha, "_", gamma, "_", Rules[rule_index], ".pdf")
    print(filepath)
    select_plot_1d(Z, sL, alpha = alpha, gamma = gamma, tail = tail, rule_index, sub = FALSE, filepath, format = "pdf")
}
