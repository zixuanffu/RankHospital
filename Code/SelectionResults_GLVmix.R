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
sL <- select2d(Z, alpha = 0.20, gamma = 0.10, tail = "L")
sR <- select2d(Z, alpha = 0.20, gamma = 0.10, tail = "R")
save(Z, sL, sR, file = "Results/2013-2022/Spec3/ZsLsR_GLVmix.Rda")




fg <- Z$fs
# ---- 浅浅看一下smooth的效果 ---- #
g <- expand.grid(theta = fg$u, sigma = fg$v)
g$fuv <- fg$fuv
require(lattice)
pdf("Figures/2013-2022/GMM_m/GLVmix_s.pdf", width = 8, height = 5)
pl <- cloud(fuv ~ theta * sigma,
    data = g, type = "h", lwd = 2,
    zlim = c(0, max(g$fuv)), scales = list(
        arrows = FALSE,
        xlab = expression(theta), ylab = expression(sigma), zlab = "density",
        screen = list(z = 10, x = -70)
    )
)
print(pl)
dev.off()
# 你别说，这个smooth的效果还真挺好看的哈





# ---- Caculate the threshold and cutoff ---- #
alpha <- 0.20
gamma <- 0.1

pm <- PM(t = dt2$hat_mu, s = dt2$Var_res1, m = dt2$Nobs, w = dt2$Nobs, G = fg)
Gcnull <- qKW2(fg, 1 - alpha)
tp <- Lfdr.GLVmix_temp(t = dt2$hat_mu, s = dt2$Var_res1, m = dt2$Nobs, w = dt2$Nobs, G = fg, cnull = Gcnull)

T01 <- quantile(tp, 1 - alpha)
T11 <- try(Finv(gamma, ThreshFDR, interval = c(0.01, 0.9), stat = tp, v = tp), silent = TRUE)

T02 <- quantile(pm, 1 - alpha)
T12 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)

# ---- plot the selection results of two rules ---- #

if (!dir.exists("Figures/2013-2022/UnknownVariance")) dir.create("Figures/2013-2022/UnknownVariance")
pdf("Figures/2013-2022/UnknownVariance.pdf", height = 5, width = 10)
par(mfrow = c(1, 2))

criteria.cap <- list(Tailp = which(tp >= T01), Tweedie = which(pm > T02))
agree <- intersect(criteria.cap[[1]], criteria.cap[[2]])
dis1 <- setdiff(criteria.cap[[1]], criteria.cap[[2]])
dis2 <- setdiff(criteria.cap[[2]], criteria.cap[[1]])

s <- dt2$Var_res1
t <- dt2$hat_mu
u_min <- min(fg$u)
u_max <- max(fg$u)
v_min <- min(fg$v)
v_max <- max(fg$v)

plot(log(s[agree]), t[agree],
    col = "grey", cex = 0.5,
    xlab = expression(sqrt(s)), ylab = "y", xlim = c(log(v_min), log(v_max)),
    ylim = c(u_min, u_max), main = "Capacity constraint"
)
points(log(s[dis1]), t[dis1], col = 2, cex = 0.5, pch = 3)
points(log(s[dis2]), t[dis2], col = 3, cex = 0.5, pch = 4)
legend("topleft", c("All agreed", "Tailp extra", "PM extra"), col = c(1, 2, 3), pch = c(1, 3, 4))

criteria <- list(
    Tailp = which(tp >= max(T01, T11)),
    Tweedie = which(pm > max(T02, T12))
    # true = which(loc.scale[, 1] >= Gcnull)
)
agree <- intersect(criteria[[1]], criteria[[2]])
dis1 <- setdiff(criteria[[1]], criteria[[2]])
dis2 <- setdiff(criteria[[2]], criteria[[1]])

plot(log(s[agree]), t[agree],
    col = "grey", cex = 0.5,
    xlab = expression(sqrt(s)), ylab = "y", xlim = c(log(v_min), log(v_max)),
    ylim = c(u_min, u_max), main = "FDR constraint"
)
points(log(s[dis1]), t[dis1], col = 2, cex = 0.5, pch = 3)
points(log(s[dis2]), t[dis2], col = 3, cex = 0.5, pch = 4)
legend("topleft", c("All agreed", "Tailp extra", "PM extra"), col = c(1, 2, 3), pch = c(1, 3, 4))

dev.off()
