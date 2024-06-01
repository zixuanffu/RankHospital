# code to test REBayes packages
rm(list = ls())
pacman::p_load(REBayes, data.table, lattice, ggplot2)
reg_inf_ols_FI <- readRDS("Results/2016-2022/reg_inf_ols_FI.rds")
summary(reg_inf_ols_FI)
pdt <- readRDS("Results/2016-2022/pdt_inf_ols.rds")

# sufficient statistics for \theta_i
pdt[, `:=`(hat_mu = FixedEffect + Res), by = .(FI)]
# sufficient statistics for \sigma_i^2
pdt[, `:=`(Var_res1 = var(Res)), by = .(FI)]
# another way to estimate the variance
pdt[, `:=`(Mean_res = mean(Res)), by = .(FI)]
pdt[, `:=`(Demeaned_res = Res - Mean_res), by = .(FI)]
pdt[, `:=`(Var_res2 = var(Demeaned_res)), by = .(FI)]

pdt[, Nobs := .N, by = .(FI)]
pdt <- pdt[Nobs >= 5]
pdt[, id := as.numeric(as.factor(FI))]
dt1 <- pdt[, .(AN, FI, id, SEJHC_MCO, SEJHP_MCO, SEANCES_MED, ETP_INF, EFF_MD, FixedEffect, Res, hat_mu, Var_res1, Var_res2, N)]
dim(dt) # 9130 13

# three ways to estimate the mixing density
help(GLmix)

# GLmix
dt2 <- pdt[, .(hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI)]
f <- GLmix(dt$hat_mu, verb = 5)
pdf("Figures/2016-2022/GLmix.pdf", width = 8, height = 5)
plot(f, xlab = expression(mu), main = "Estimated Location Mixing Density")
dev.off()

# GLVmix
help(GLVmix)
dt2 <- pdt[, .(hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI)]
saveRDS(dt2, "Results/2016-2022/theta_sigma.rds")
f <- GLVmix(t = dt2$hat_mu, s = dt2$Var_res1, m = dt2$Nobs)
require(lattice) # a visualisation package
str(f)
g <- expand.grid(alpha = f$u, theta = f$v)
g$fuv <- f$fuv
pl <- cloud(fuv ~ alpha * theta,
    data = g, type = "h", lwd = 2,
    zlim = c(0, max(g$fuv)), scales = list(
        arrows = FALSE,
        xlab = expression(alpha), ylab = expression(theta), zlab = "density",
        screen = list(z = 10, x = -70)
    )
)
print(pl)

# TGLVmix

# WGLVmix
help(WGLVmix)


f <- WGLVmix(y = pdt$hat_mu, id = pdt$id)
require(lattice) # a visualisation package
str(f)
g <- expand.grid(alpha = f$u, theta = f$v)
g$fuv <- f$fuv

png("Figures/2016-2022/WGLVmix.png", width = 800, height = 500)
pl <- cloud(fuv ~ alpha * theta,
    data = g, type = "h", lwd = 2,
    zlim = c(0, max(g$fuv)), scales = list(
        arrows = FALSE,
        xlab = expression(alpha), ylab = expression(theta), zlab = "density",
        screen = list(z = 10, x = -70)
    )
)
print(pl)

# Close the PNG device
dev.off()

# WTGVmix
help(WTLVmix)
f_t <- WTLVmix(y = pdt$hat_mu, id = pdt$id, u = 300, v = 300)

# plot(x=f_t$u,y=f_t$fu, xlab = expression(mu), main = "Estimated Location Mixing Density")
X11(width = 8, height = 5)
par(mfrow = c(1, 2))
pdf("Figures/2016-2022/WTLVmix_u.pdf", width = 8, height = 5)
plot(f_t$u, f_t$fu,
    main = expression(paste("Density of ", alpha, sep = "")),
    xlab = expression(alpha), ylab = expression(f(alpha)), type = "l"
)
dev.off()
pdf("Figures/2016-2022/WTLVmix_v.pdf", width = 8, height = 5)
plot(f_t$v[1:299], f_t$fv,
    main = expression(paste("Density of ", theta, sep = "")),
    xlab = expression(theta), ylab = expression(f(theta)), type = "l"
)
dev.off()
warnings() # check the warnings later.


help(KWsmooth)
help(Pmix)
