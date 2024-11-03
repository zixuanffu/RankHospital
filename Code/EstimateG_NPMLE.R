rm(list = ls())
pacman::p_load(data.table, REBayes, lattice, ggplot2)
source("Code/SelectX_GLVmix.R")
# ---- load regression results ---- #
pdt <- readRDS("Results/2013-2022/pdt_used_gmm_sys_m.rds")
Z <- fit2d(pdt)
# ---- 4 ways to estimate the mixing density ---- #

# ---- 1. homogeneous known variance ---- #
help(GLmix) # GLmix(x, v = 300, sigma = 1, hist = FALSE, histm = 300, weights = NULL, ...)
f <- GLmix(dt2$hat_mu, verb = 5) # assume sigma = 1
pdf("Figures/2013-2022/GMM/HomoKnownVar.pdf", width = 8, height = 5)
plot(f, xlab = expression(mu), main = "Estimated Location Mixing Density with Homogeneous Known Variance (sigma^2 = 1)")
dev.off()

# ---- 2. heterogeneous known variance ---- #
dir.create("Figures/2013-2022/GMM", showWarnings = FALSE)
help(GLmix)
f <- GLmix(dt2$hat_mu, sigma = sqrt(dt2$Var_res1), verb = 5) # assume the estimated variance is the known variance
pdf("Figures/2013-2022/GMM/HeteroKnownVar.pdf", width = 8, height = 5)
plot(f, xlab = expression(mu), main = "Estimated Location Mixing Density with Heterogeneous Known Variance")
dev.off()

# ---- 3. heterogeneous unkonwn variance (independent) ---- #
# the following methdo is introduced in \citet{GuKoenker2017}
# A Kiefer-Wolfowitz NPMLE procedure for estimation of a Gaussian model with independent mean and variance components with weighted longitudinal data. This version exploits a Student t decomposition of the likelihood.
help(WTLVmix)

# A Kiefer-Wolfowitz NPMLE procedure for estimation of a Gaussian model with independent mean and variance prior components with weighted longitudinal data. This version iterates back and forth from Gamma and Gaussian forms of the likelihood.
help(WLVmix)

# ---- check for high variance units
# View(Z$pdt)
# dt <- readRDS("Data/Out/dt_inf_pool.rds")
# View(dt)

f_wlv <- WLVmix(y = Z$pdt$hat_mu, id = Z$pdt$id, u = 300, v = 300)
f_wtlv <- WTLVmix(y = Z$pdt$hat_mu, id = Z$pdt$id, u = 300, v = 300)


X11(width = 8, height = 5)
par(mfrow = c(1, 2))
pdf("Figures/2013-2022/GMM_fd/WTLVmix_u.pdf", width = 8, height = 5)
plot(f_wtlv$u, f_wtlv$fu,
    main = expression(paste("Density of ", theta, sep = "")),
    xlab = expression(theta), ylab = expression(f(theta)), type = "l"
)
dev.off()
pdf("Figures/2013-2022/GMM_fd/WTLVmix_v.pdf", width = 8, height = 5)
plot(f_wtlv$v, f_wtlv$fv,
    main = expression(paste("Density of ", sigma, sep = "")),
    xlab = expression(sigma), ylab = expression(f(sigma)), type = "l"
)
dev.off()


# ---- 4. heterogeneous unkonwn variance (dependent) ---- #
# Longitudinal Gaussian mean and variances model
help(WGLVmix) # WGLVmix(y, id, w, u = 30, v = 30, ...)
str(Z$pdt)
f_g <- WGLVmix(y = Z$pdt$y, id = Z$pdt$id)
str(f_g)

require(lattice)
g <- expand.grid(theta = f_g$u, sigma = f_g$v)
g$fuv <- f_g$fuv
pdf("Figures/2013-2022/GMM_fd/WGLVmix.pdf", width = 8, height = 5)
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


# Gaussian Location-Scale Mixture Model
help(GLVmix) # GLVmix(t, s, m, u = 30, v = 30, ...)
fg <- GLVmix(t = Z$dt2$hat_mu, s = Z$dt2$Var_res1, m = Z$dt2$Nobs)
str(fg)
require(lattice) # a visualisation package
g <- expand.grid(theta = fg$u, sigma = fg$v)
g$fuv <- fg$fuv
pdf("Figures/2013-2022/GMM_fd/GLVmix.pdf", width = 8, height = 5)
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

# smooth the estimated GLVmix
fg_s <- KW2smooth(fg, bw = bwKW2(fg, 2))
g$fuv <- fg_s$fuv
pdf("Figures/2013-2022/GMM/GLVmix_s.pdf", width = 8, height = 5)
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
# the two methods give slightly (?) differnet results.

# ---- Estimate G separately for each category of hospitals ---- #
# ---- 2.1 heterogeneous known variance ---- #
dt2 <- pdt[, .(hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI, STJR)]

for (i in 0:3) {
    assign(paste0("f", i), GLmix(dt2[STJR == i]$hat_mu, sigma = sqrt(dt2[STJR == i]$Var_res1), verb = 5)) # assume the estimated variance is the known variance
}

png("Figures/2013-2022/GMM/HeteroKnownVar_sep.png", width = 800, height = 500 * 4)
par(mfrow = c(4, 1))
for (i in 0:3) {
    plot(get(paste0("f", i)), xlab = expression(mu), main = paste("Estimated G (STJR = ", i, ")", sep = ""))
}
dev.off()

# ---- 2.2 smoothed G ---- #
png("Figures/2013-2022/GMM/HeteroKnownVar_sep_s.png", width = 800, height = 500 * 4)
par(mfrow = c(4, 1))
for (i in 0:3) {
    f <- get(paste0("f", i))
    assign(paste0("f", i, "s"), KWsmooth(f, bw = bwKW(f, 2)))
    plot(get(paste0("f", i, "s")), xlab = expression(mu), main = paste("Estimated G smoothed (STJR = ", i, ")", sep = ""))
}
dev.off()
