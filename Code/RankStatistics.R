rm(list = ls())
pacman::p_load(REBayes, data.table)
source("Code/SelectX.R")

# ---- Section 1: Different tail gives different ranking statistics ---- #
load("Results/2013-2022/ZsLsR.Rda")
alpha1 <- 0.4
tp_1 <- data.table(Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = qKW(Z$fs, 1 - alpha1), tail = "R"))
tp_1[, Order := order(V1)]
View(tp_1)
alpha2 <- 0.5
tp_2 <- data.table(Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = qKW(Z$fs, 1 - alpha2), tail = "R"))
tp_2[, Order := order(V1)]
View(tp_2)

View(tp_1$Order)
View(tp_2$Order)

# tail probability...is tricky...not as simple as I thought...

tp <- cbind(tp_1, tp_2)
colnames(tp) <- c("V1_1", "Order_1", "V1_2", "Order_2")
tp[, SAME := Order_1 == Order_2] # not the same order...why?
tp[, MON := (V1_1 <= V1_2)] # monotone in \alpha for sure


# ---- Section 2: Constrast different tails using normal discrete ---- #
# References/19304_Data_and_Programs/figs/thresh_eg_normaldiscrete_zeronull.R

# Throughout G denotes the mixing density for the Gaussian sequence model
# G is encoded as a GLmix object, e.g.
# normal-discrete
# contrast rejection region between conventional zero null vs. tail null and compare power

require(REBayes)


# So posterior means, and so forth can be computed with predict, e.g.

PM <- function(x, G, s, cnull) predict(G, x, newsigma = s)

# ---- calcaulte the tail probablity given the cutoff cnull ---- #
# ---- given cnull, calculate P(\theta>null|y_i,\sigma_i) ---- #
Lfdr <- function(x, G, s, cnull) {
    v <- G$x
    fv <- G$y
    A <- dnorm(outer(x, v, "-"), sd = s)
    if (sum(A) == 0) {
        1
    } else {
        1 - c((A %*% (fv * (v < cnull))) / (A %*% fv))
    }
}

# Some generic code for computing inverses with uniroot
# Useage Examples:
# x0 = Finv(2, F = qnorm, sd = 2)
# x1 = mapply(Finv, 1:5/10, MoreArgs = list(F = qnorm, sd = 2))

Finv <- function(y, F, interval = c(0, 1), ...) {
    uniroot(function(x) F(x, ...) - y, interval, extendInt = "yes")$root
}

# Generic thresholding functions
# From now on, threshold is for ranking statistics v_cnull(x). Cutoff is for the real value x.


# ---- generate the capacity control level given a threshold $\lambda$ ---- #
Thresh0 <- function(lam, Fun, G, s, fs, domain, cnull) {
    ns <- length(s)
    nu <- rep(NA, ns)
    for (j in 1:ns) {
        cut <- Finv(lam, Fun, interval = domain, G = G, s = s[j], cnull)
        nu[j] <- sum(G$y * (1 - pnorm(cut - G$x, sd = s[j])))
    }
    crossprod(nu, fs)
}

# ---- generate the fdr level given a threshold $\lambda$ ---- #
Thresh1 <- function(lam, Fun, G, s, fs, domain, cnull, znull) {
    # find value such that Fun criterion is met
    ns <- length(s)
    nu <- rep(NA, ns)
    cnu <- rep(NA, ns)
    # if(cnull == 4) browser()
    for (j in 1:ns) {
        cut <- Finv(lam, Fun, interval = domain, G = G, s = s[j], cnull)
        nu[j] <- sum(G$y * (1 - pnorm(cut - G$x, sd = s[j])))
        cnu[j] <- sum(G$y * (G$x < znull) * (1 - pnorm(cut - G$x, sd = s[j])))
    }
    crossprod(cnu, fs) / crossprod(nu, fs)
}


# heterogeneous known variance
# DGP: normal - discrete
G <- list(x = c(-1, 0.5, 5), y = c(0.85, 0.1, 0.05), sigma = 1)
# note the smaller null effect is change from 2 to 1 in this example so that threshold TLfdr3 does not become 1
class(G) <- "GLmix"
rG <- function(n, G, s) rnorm(n, sample(G$x, prob = G$y, replace = TRUE), s)
s <- seq(0.5, 4, length = 200) # sigma values
x <- rG(200, G, s)
# scaled beta (density of s monotone increasing on the support)
# fs = dbeta(s/max(s), 6,1)/sum(dbeta(s/max(s), 6,1))
fs <- rep(1 / length(s), length(s)) # sigma density
postmean <- predict(G, x, newsigma = s)

# CASE 1: FDR and capacity control

# using cnull = 4
# given the capacity control level, calculate the threhold for $\lambda$.
TLfdr0 <- Finv(0.05, Thresh0, c(0.01, 0.6),
    Fun = Lfdr, G = G, s = s,
    fs = fs, domain = c(-20, 20), cnull = 4
)
cLfdr0 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr0[i] <- Finv(TLfdr0, Lfdr, interval = c(-20, 20), G = G, s = s[i], cnull = 4)
}
# given the fdr control level, calculate the threshold for $\lambda$
TLfdr1 <- Finv(0.1, Thresh1, c(0.001, 0.8),
    Fun = Lfdr, G = G, s = s,
    fs = fs, domain = c(-20, 20), cnull = 4, znull = 4
)
cLfdr1 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr1[i] <- Finv(TLfdr1, Lfdr, interval = c(-20, 20), G = G, s = s[i], cnull = 4)
}

# using cnull = 0
# given the capacity control level, calculate the threhold for $\lambda$.
TLfdr2 <- Finv(0.05, Thresh0, c(0.1, 0.9999),
    Fun = Lfdr, G = G, s = s,
    fs = fs, domain = c(-20, 20), cnull = 0
)
cLfdr2 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr2[i] <- Finv(TLfdr2, Lfdr, interval = c(-20, 20), G = G, s = s[i], cnull = 0)
}
# given the fdr control level, calculate the threshold for $\lambda$
TLfdr3 <- Finv(0.1, Thresh1, c(0.1, 0.99999999),
    Fun = Lfdr, G = G, s = s,
    fs = fs, domain = c(-20, 20), cnull = 0, znull = 4
)
cLfdr3 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr3[i] <- Finv(TLfdr3, Lfdr, interval = c(-20, 20), G = G, s = s[i], cnull = 0)
}

# consider only capacity control level = 0.05
dir_name <- "Figures/Simulation/"
if (!dir.exists(dir_name)) {
    dir.create(dir_name)
}
png(paste0(dir_name, "thresh_eg_normal_discrete_alphazero_cap.png"), width = 800, height = 800)
pdf(paste0(dir_name, "thresh_eg_normal_discrete_alphazero_cap.pdf"))
plot(s, cLfdr0, ylim = c(0, 18), type = "l", lwd = 2, ylab = "Threshold", xlab = expression(sigma))
# lines(s, cLfdr1)
# red for cnull = 0
lines(s, cLfdr2, col = 2, lty = 2)
# lines(s, cLfdr3, col = 2, lwd = 2, lty = 2)
legend("topleft", c("zeroNull", "tailNull"), col = c(2, 1), lty = c(2, 1), lwd = c(2, 2))
mtext("capacity constraint = 0.05")
## highlight polygon
loc <- which.min(abs(cLfdr0 - cLfdr2))
polygon(x = c(s[1], s[1:loc], s[1:loc]), y = c(cLfdr0[1], cLfdr2[1:loc], cLfdr0[1:loc]), density = 20, angle = 90, lty = 2)
polygon(x = c(s[loc], s[(loc + 1):length(s)], s[length(s)], s[length(s):(loc + 1)]), y = c(cLfdr2[loc], cLfdr0[(loc + 1):length(s)], cLfdr2[length(s)], cLfdr2[length(s):(loc + 1)]), , density = 20, angle = 90, col = 4)
dev.off()

png(paste0(dir_name, "thresh_eg_normal_discrete_alphazero_capfdr.png"), width = 800, height = 800)
pdf(paste0(dir_name, "thresh_eg_normal_discrete_alphazero_capfdr.pdf"))
cLfdr_alpha <- rep(NA, length(s))
cLfdr_alpha <- pmax(cLfdr0, cLfdr1)
cLfdr_null <- rep(NA, length(s))
cLfdr_null <- pmax(cLfdr2, cLfdr3)

# black for cnull=4
# red for cnull = 0
plot(s, cLfdr_alpha, ylim = c(0, 18), type = "l", lwd = 2, ylab = "Threshold", xlab = expression(sigma))
lines(s, cLfdr_null, col = 2, lwd = 2, lty = 2)
legend("topleft", c("zeroNull", "tailNull"), col = c(2, 1), lty = c(2, 1), lwd = c(2, 2))
mtext("capacity = 0.05, gamma = 0.1")
## highlight polygon
loc <- which.min(abs(cLfdr_alpha - cLfdr_null))
polygon(x = c(s[1], s[1:loc], s[1:loc]), y = c(cLfdr_alpha[1], cLfdr_null[1:loc], cLfdr_alpha[1:loc]), density = 20, angle = 90, lty = 2)
polygon(x = c(s[loc], s[(loc + 1):length(s)], s[length(s)], s[length(s):(loc + 1)]), y = c(cLfdr_null[loc], cLfdr_alpha[(loc + 1):length(s)], cLfdr_null[length(s)], cLfdr_null[length(s):(loc + 1)]), , density = 20, angle = 90, col = 4)
dev.off()

# ---- calculate the power ---- #
power <- function(cut, s, fs, G, znull) {
    # P(non-null case & reject)/P(non-null) : interpret as among all non-null cases, what's the probability of correct rejection.
    ns <- length(s)
    cnu <- rep(NA, ns)
    # if(cnull == 4) browser()
    for (j in 1:ns) {
        cnu[j] <- sum(G$y * (G$x > znull) * (1 - pnorm(cut[j] - G$x, sd = s[j])))
    }
    nnprop <- sum(G$y * (G$x > znull))
    crossprod(cnu, fs) / nnprop
}

power(cLfdr0, s, fs, G, znull = 4)
power(cLfdr1, s, fs, G, znull = 4)
power(cLfdr2, s, fs, G, znull = 4)
power(cLfdr3, s, fs, G, znull = 4)

# ---- Section 3: Selection ---- #
# check selection
A <- matrix(NA, 200, 2)
View(x > cLfdr_alpha)
A[which(x > cLfdr_alpha), 1] <- 1
A[which(x > cLfdr_null), 2] <- 1

Aagree <- intersect(which(A[, 1] == 1), which(A[, 2] == 1))
Adis1 <- setdiff(which(A[, 1] == 1), which(A[, 2] == 1))
Adis2 <- setdiff(which(A[, 2] == 1), which(A[, 1] == 1))

pdf("Figures/Simulation/Selection_alphazero.pdf")
par(mfrow = c(1, 1))
# p<-plot(1, 1, type = "n", xlab = "", ylab = "", xlim = 0:1, ylim = 0:1)
plot(x[Aagree], s[Aagree], col = "grey", cex = 0.5)
points(x[Adis1], s[Adis1], col = 4, cex = 0.5)
points(x[Adis2], s[Adis2], col = 2, cex = 0.5)

text <- c(
    "Agree", paste("alpha", " extra", sep = ""),
    paste("null", " extra", sep = "")
)
cols <- c("grey", "red", "blue")
legend("topleft", text, col = cols, pch = 1, cex = 0.95, bty = "n")
dev.off()
# too few observations...don't bother...

# ---- Section 4: Simplified expression for the constraint ---- #
# ---- only consider cap constraint ---- #
# using cnull = 4
# given the capacity control level, calculate the threhold for $\lambda$.
# heterogeneous known variance
# DGP: normal - discrete
G <- list(x = c(-1, 0.5, 5), y = c(0.85, 0.1, 0.05), sigma = 1)
# note the smaller null effect is change from 2 to 1 in this example so that threshold TLfdr3 does not become 1
class(G) <- "GLmix"
rG <- function(n, G, s) rnorm(n, sample(G$x, prob = G$y, replace = TRUE), s)
s <- seq(0.5, 4, length = 200) # sigma values
x <- rG(200, G, s)
# scaled beta (density of s monotone increasing on the support)
# fs = dbeta(s/max(s), 6,1)/sum(dbeta(s/max(s), 6,1))
fs <- rep(1 / length(s), length(s)) # sigma density

tp <- Lfdr.GLmix_temp(x, G, s, cnull = 4)
# each tp is conditioned on the sigma_i
alpha <- 0.05
TLfdr0 <- quantile(tp, 1 - alpha) # in the previous method 0.262, now 0.111
cLfdr0 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr0[i] <- Finv(TLfdr0, Lfdr.GLmix_temp, interval = c(-20, 20), G = G, s = s[i], cnull = 4)
}

# using cnull = 0
# given the capacity control level, calculate the threhold for $\lambda$.
tp_null <- Lfdr.GLmix_temp(x, G, s, cnull = 0)
TLfdr2 <- quantile(tp_null, 1 - alpha) # in the previous method 0.551,now 0.481
cLfdr2 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr2[i] <- Finv(TLfdr2, Lfdr.GLmix_temp, interval = c(-20, 20), G = G, s = s[i], cnull = 0)
}

# consider only capacity control level = 0.05
dir_name <- "Figures/Simulation/"
if (!dir.exists(dir_name)) {
    dir.create(dir_name)
}
pdf(paste0(dir_name, "thresh_eg_normal_discrete_alphazero_capr.pdf"))
plot(s, cLfdr0, ylim = c(0, 18), type = "l", lwd = 2, ylab = "Threshold", xlab = expression(sigma))
# lines(s, cLfdr1)

# red for cnull = 0
lines(s, cLfdr2, col = 2, lty = 2)
# lines(s, cLfdr3, col = 2, lwd = 2, lty = 2)

legend("topleft", c("zeroNull", "tailNull"), col = c(2, 1), lty = c(2, 1), lwd = c(2, 2))
mtext("capacity constraint = 0.05")
## highlight polygon
loc <- which.min(abs(cLfdr0 - cLfdr2))
polygon(x = c(s[1], s[1:loc], s[1:loc]), y = c(cLfdr0[1], cLfdr2[1:loc], cLfdr0[1:loc]), density = 20, angle = 90, lty = 2)
polygon(x = c(s[loc], s[(loc + 1):length(s)], s[length(s)], s[length(s):(loc + 1)]), y = c(cLfdr2[loc], cLfdr0[(loc + 1):length(s)], cLfdr2[length(s)], cLfdr2[length(s):(loc + 1)]), , density = 20, angle = 90, col = 4)
dev.off()
# similar results:)

# ---- consider both constraints ---- #
source("Code/SelectX.R")
# using cnull = 4
# given the fdr control level, calculate the threshold for $\lambda$
TLfdr1 <- Finv(0.1, Thresh1, c(0.001, 0.8),
    Fun = Lfdr, G = G, s = s,
    fs = fs, domain = c(-20, 20), cnull = 4, znull = 4
)
cLfdr1 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr1[i] <- Finv(TLfdr1, Lfdr, interval = c(-20, 20), G = G, s = s[i], cnull = 4)
}

# using cnull = 0

# given the fdr control level, calculate the threshold for $\lambda$
TLfdr3 <- Finv(0.1, Thresh1, c(0.1, 0.99999999),
    Fun = Lfdr, G = G, s = s,
    fs = fs, domain = c(-20, 20), cnull = 0, znull = 4
)
cLfdr3 <- rep(NA, length(s))
for (i in 1:length(s)) {
    cLfdr3[i] <- Finv(TLfdr3, Lfdr, interval = c(-20, 20), G = G, s = s[i], cnull = 0)
}

pdf(paste0(dir_name, "thresh_eg_normal_discrete_alphazero_capfdrr.pdf"))
cLfdr_alpha <- rep(NA, length(s))
cLfdr_alpha <- pmax(cLfdr0, cLfdr1)
cLfdr_null <- rep(NA, length(s))
cLfdr_null <- pmax(cLfdr2, cLfdr3)

# black for cnull=4
# red for cnull = 0
plot(s, cLfdr_alpha, ylim = c(0, 18), type = "l", lwd = 2, ylab = "Threshold", xlab = expression(sigma))
lines(s, cLfdr_null, col = 2, lwd = 2, lty = 2)
legend("topleft", c("zeroNull", "tailNull"), col = c(2, 1), lty = c(2, 1), lwd = c(2, 2))
mtext("capacity = 0.05, gamma = 0.1")
## highlight polygon
loc <- which.min(abs(cLfdr_alpha - cLfdr_null))
polygon(x = c(s[1], s[1:loc], s[1:loc]), y = c(cLfdr_alpha[1], cLfdr_null[1:loc], cLfdr_alpha[1:loc]), density = 20, angle = 90, lty = 2)
polygon(x = c(s[loc], s[(loc + 1):length(s)], s[length(s)], s[length(s):(loc + 1)]), y = c(cLfdr_null[loc], cLfdr_alpha[(loc + 1):length(s)], cLfdr_null[length(s)], cLfdr_null[length(s):(loc + 1)]), , density = 20, angle = 90, col = 4)
dev.off()
