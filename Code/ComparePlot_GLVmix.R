rm(list = ls())
pacman::p_load(REBayes)
source("Code/SelectX_GLVmix.R")

pdt <- readRDS("Results/2013-2022/pdt_used_gmm_fd.rds")
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

dir.create("Figures/2013-2022/GMM_fd/GLVmix", showWarnings = FALSE)
Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
tail <- "L"
for (rule_index in c(4, 5, 6)) {
    filepath <- paste0("Figures/2013-2022/GMM_fd/GLVmix/Left_", alpha, "_", gamma, "_", Rules[2], "_", Rules[rule_index], ".pdf")
    print(filepath)
    pdf(filepath, height = 4.5, width = 8)
    par(mfrow = c(1, 2))
    level_plot_2d(Z, sL, alpha = alpha, gamma = gamma, tail = "L", cindex = c(2, rule_index), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200), xlim = c(u_min, u_max), ylim = c(v_min, v_max))
    dev.off()
}


pdf(paste0("Figures/2013-2022/GMM_fd/GLVmix/Left_", alpha, "_", gamma, ".pdf"), height = 4.50 * 3, width = 8.00)
par(mfrow = c(3, 2))
for (i in c(4, 5, 6)) {
    level_plot_2d(Z, sL, alpha = alpha, gamma = gamma, tail = "L", cindex = c(2, i), constraint = c("cap", "fdr"), seq(u_min, u_max, length = 600), seq(v_min, v_max, length = 200), xlim = c(u_min, u_max), ylim = c(v_min, v_max))
}
dev.off()


# ---- Focus on left tail probability ---- #

Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
alpha <- 0.20
gamma <- 0.1
tail <- "L"
Z <- fit2d(pdt)
sL <- select2d(Z, alpha = alpha, gamma = gamma, tail = tail)
for (rule_index in c(2, 4, 5, 6)) {
    filepath <- paste0("Figures/2013-2022/GMM_fd/GLVmix/Left_", alpha, "_", gamma, "_", Rules[rule_index], ".pdf")
    print(filepath)
    select_plot_2d(Z, sL, alpha = alpha, gamma = gamma, tail = tail, rule_index, sub = FALSE, filepath, format = "pdf")
}
