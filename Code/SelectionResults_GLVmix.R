# compared selected sets
# model
# T_i ~ N(theta_i, sigma_i^2/T_i)
# S_i ~ gamma(r_i, sigma_i^2/r_i)
# r_i = (T_i - 1)/2
# G is the prior of (theta, sigma^2) (we don't assume independence here.)

require(REBayes)
source("Code/SelectX_GLVmix.R")

# ---- NPMLE estimate the prior distritbuion of (theta, sigma^2) ---- #
pdt <- readRDS("Results/2013-2022/pdt_used_gmm_sys_m.rds")
Z <- fit2d(pdt)
# ---- Perform the selection ---- #
alpha <- 0.5
gamma <- 0.20
sL <- select2d(Z, alpha = alpha, gamma = gamma, tail = "L")
sR <- select2d(Z, alpha = alpha, gamma = gamma, tail = "R")




fg <- Z$fs
# ---- 浅浅看一下smooth的效果 ---- #
g <- expand.grid(theta = fg$u, sigma = fg$v)
g$fuv <- fg$fuv
require(lattice)
pdf("Figures/2013-2022/GMM_m/GLVmix_s.pdf", width = 8, height = 5)
pl <- cloud(fuv ~ theta * sigma,
    data = g, type = "h", lwd = 2,
    zlim = c(0, max(g$fuv)), xlim = c(min(g$theta), max(g$theta)),
    ylim = c(min(g$sigma), max(g$sigma)), scales = list(
        arrows = FALSE,
        xlab = expression(theta), ylab = expression(sigma), zlab = "density",
        screen = list(z = 10, x = -70)
    )
)
print(pl)
dev.off()
# 你别说，这个smooth的效果还真挺好看的哈


fg <- Z$f
g <- expand.grid(theta = fg$u, sigma = fg$v)
g$fuv <- fg$fuv
require(lattice)
pdf("Figures/2013-2022/GMM_m/GLVmix.pdf", width = 8, height = 5)
pl <- cloud(fuv ~ theta * sigma,
    data = g, type = "h", lwd = 2,
    xlim = c(min(g$theta), max(g$theta)),
    ylim = c(min(g$sigma), max(g$sigma)),
    zlim = c(0, max(g$fuv)), scales = list(
        arrows = FALSE,
        xlab = expression(theta), ylab = expression(sigma), zlab = "density",
        screen = list(z = 10, x = -70)
    )
)
print(pl)
dev.off()
