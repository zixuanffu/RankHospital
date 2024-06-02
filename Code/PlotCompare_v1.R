rm(list = ls())
pacman::p_load(REBayes)
load("Results/2013-2022/ZsLsR.Rda")
source("Code/SelectX.R")
Z <- append(Z, list(W = (1 / Z$s)))

# ---- the x-axis and y-axis limits for the plots ---- #
x_min <- min(Z$S)
x_max <- max(Z$S)
y_min <- min(Z$W)
y_max <- max(Z$W)

pdf("Figures/2013-2022/conflict1.pdf", height = 4, width = 8)
par(mfrow = c(1, 2))

# ---- plot the level sets of TPKW and MLE under cap and under cap+fdr constraints ---- #
L11 <- level_plot(Z, alpha = 0.22, gamma = 0.2, tail = "L", cindex = c(1, 5), constraint = "cap", ygrid = seq(x_min, x_max, length = 900), wgrid = seq(y_min, y_max, length = 300))
title("(a)")
# there are definitely ways to simply the computation...
L12 <- level_plot(Z, alpha = 0.22, gamma = 0.2, tail = "L", cindex = c(1, 5), constraint = "fdr", ygrid = seq(x_min, x_max, length = 900), wgrid = seq(y_min, y_max, length = 300))
title("(b)")

# ---- plot the level sets of TPKW and PMKW under cap and under cap+fdr constraints ---- #
pdf("Figures/2013-2022/conflict2.pdf", height = 4, width = 8)
par(mfrow = c(1, 2))
L21 <- level_plot(Z, alpha = 0.22, gamma = 0.2, tail = "L", cindex = c(1, 3), constraint = "cap", ygrid = seq(x_min, x_max, length = 900), wgrid = seq(y_min, y_max, length = 300))
title("(c)")

dev.off()

# Warning messages:
# 1: In log(theta[2] + s^2) : NaNs produced
# 2: In log(theta[2] + s^2) : NaNs produced
# 3: In log(theta[2] + s^2) : NaNs produced
# 4: In log(theta[2] + s^2) : NaNs produced
# 5: In log(theta[2] + s^2) : NaNs produced
# 6: In log(theta[2] + s^2) : NaNs produced


# estimated FDR level of MLE rule under capacity constraint
cnull <- qKW(Z$fs, 0.22)
tp <- Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$S), cnull = cnull, tail = "L")
estfdr_MLE <- ThreshFDR(-L25$thresh[5, 1], stat = -Z$S, v = tp)

A <- sL$A # cap and fdr constraint
B <- sL$B # cap constraint
cols <- c("grey", "blue", "red")
Aagree <- intersect(which(A[, 2] == 1), which(B[, 5] == 1)) # compare TP under fdr control vs MLE with just cap control.
Adis1 <- setdiff(which(A[, 2] == 1), which(B[, 5] == 1))
Adis2 <- setdiff(which(B[, 5] == 1), which(A[, 2] == 1))
ygrid <- seq(0.5, 1, length = 300)
wgrid <- 8 * (1:100)
contour(ygrid, wgrid, L25$cls[, , 2],
    lwd = 2, col = 4,
    levels = round(L25$thresh[2, 2], digits = 3),
    xlab = "T", ylab = "W", drawlabels = FALSE
)
abline(v = L25$thresh[5, 1], lwd = 2, col = 2)
points(Z$S[Aagree], Z$W[Aagree], col = "grey", cex = 0.5)
points(Z$S[Adis1], Z$W[Adis1], col = 4, cex = 0.5)
points(Z$S[Adis2], Z$W[Adis2], col = 2, cex = 0.5)
text <- c(
    "Agree", paste("TP", " extra", sep = ""),
    paste("MLE", " extra", sep = "")
)
legend("topleft", text,
    col = cols, pch = 1, cex = 0.95, bty = "n"
)
title("(b)")

L21 <- level_plot(Z,
    alpha = 0.22, gamma = 0.2, tail = "L", ygrid = seq(0.5, 1, length = 300),
    wgrid = 8 * (1:100), cindex = c(1, 3), constraint = "cap"
)
title("(c)")

# level_plot(Z, alpha = 0.22, gamma = 0.2, tail = "L", ygrid = seq(0.5, 1, length = 300),
# 	   wgrid = 8*(1:100), cindex = c(2, 8), constraint = "fdr")  # JS controls fdr by using the Gaussian G estimates
# title("(d)")

# estimated FDR level of JS rule under just capacity constraint
est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
estmean <- est[1]
estvar <- est[2]
# Linear rule
R <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
estfdr_JS <- ThreshFDR(-L25$thresh[8, 1], stat = -R, v = tp)


Aagree <- intersect(which(A[, 2] == 1), which(B[, 8] == 1)) # compare TP under fdr control vs JS with just cap control.
Adis1 <- setdiff(which(A[, 2] == 1), which(B[, 8] == 1))
Adis2 <- setdiff(which(B[, 8] == 1), which(A[, 2] == 1))
ygrid <- seq(0.5, 1, length = 300)
wgrid <- 8 * (1:100)
contour(ygrid, wgrid, L25$cls[, , 2],
    lwd = 2, col = 4,
    levels = round(L25$thresh[2, 2], digits = 3),
    xlab = "T", ylab = "W", drawlabels = FALSE
)

contour(ygrid, wgrid, L25$cls[, , 8],
    lwd = 2,
    levels = round(L25$thresh[8, 1], digits = 3),
    xlab = "T", ylab = "W", add = TRUE, col = 2, drawlabels = FALSE
)

points(Z$S[Aagree], Z$W[Aagree], col = "grey", cex = 0.5)
points(Z$S[Adis1], Z$W[Adis1], col = 4, cex = 0.5)
points(Z$S[Adis2], Z$W[Adis2], col = 2, cex = 0.5)
text <- c(
    "Agree", paste("TP", " extra", sep = ""),
    paste("JS", " extra", sep = "")
)
legend("topleft", text,
    col = cols, pch = 1, cex = 0.95, bty = "n"
)
title("(d)")

dev.off()
