pacman::p_load(REBayes)

fit1d <- function(pdt, bwt = 2, rtol = 1e-20, ...) {
    # ---- Location \theta_i and scale \sigma_i^2 parameters ---- #
    # ---- sufficient statistics for \theta_i ---- #
    pdt[, `:=`(hat_mu = FixedEffect + Res), by = .(FI)]
    # ---- sufficient statistics for \sigma_i^2 ---- #
    pdt[, `:=`(Var_res1 = var(Res)), by = .(FI)]
    # ---- another way to estimate the variance ---- #
    pdt[, `:=`(Mean_res = mean(Res)), by = .(FI)]
    pdt[, `:=`(Demeaned_res = Res - Mean_res), by = .(FI)]
    pdt[, `:=`(Var_res2 = var(Demeaned_res)), by = .(FI)]
    # ---- remove FI with less than 6 observations ---- #
    # this is actually already done in the preparation step in RegResults.R script
    pdt[, Nobs := .N, by = .(FI)]
    pdt <- pdt[Nobs >= 6]
    pdt[, id := as.numeric(as.factor(FI))]
    dt2 <- pdt[, .(id = first(id), hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI)]
    # ---- Hetereogeneous Known Variance ---- #
    f <- GLmix(dt2$hat_mu, sigma = sqrt(dt2$Var_res1), verb = 5) # assume the estimated variance is the known variance
    fs <- KWsmooth(f, bw = bwKW(f, bwt))

    list(f = f, fs = fs, S = dt2$hat_mu, s = dt2$Var_res1, id = dt2$id)
}

Lfdr.GLmix_temp <- function(x, G, s, cnull, tail = "R") { # Modified for outliers??
    # changed for left tail selection
    v <- G$x
    fv <- G$y
    A <- dnorm(outer(x, v, "-"), sd = s)
    if (tail == "R") {
        v <- 1 - c((A %*% (fv * (v < cnull))) / (A %*% fv))
    } else {
        v <- 1 - c((A %*% (fv * (v >= cnull))) / (A %*% fv))
    }
    v
}

P <- function(a, d) 1 / (2 * (a + d)^2) # 1/Var(S)
psi <- function(a, s, d) a - sum((s - d) * P(a, d)) / sum(P(a, d))
lik <- function(theta, x, s) {
    sum(log(theta[2] + s^2)) + sum((x - theta[1])^2 / (theta[2] + s^2))
}


EMrule <- function(x, s, mean = 0, estvar) { # Efron & Morris 1974 Rand Memo, allowing non-Zero location
    if (mean != 0) x <- x - mean
    if (missing(estvar)) estvar <- uniroot(psi, c(0.001, 4), s = x^2, d = s^2, extendInt = "yes")$root
    n <- length(x)
    z <- rep(0, n)
    a <- rep(estvar, n)
    toler <- 1e-05
    it <- 0
    while (sum(abs(a - z)) > toler) {
        a <- z
        it <- it + 1
        for (i in 1:n) {
            p <- P(a, s^2)
            p[i] <- 3 / (2 * (a[i] + s[i]^2)^2)
            c <- x^2 - s^2
            c[i] <- (x[i]^2 - 3 * s[i]^2) / 3
            z[i] <- sum(c * p) / sum(p)
        }
    }
    khat <- rep(0, n)
    for (i in 1:n) {
        khat[i] <- sum((a[i] + s[i]^2)^2 / (a[i] + s^2)^2)
    }
    chat <- pmax((khat - 2) / (khat + 2), 0)
    Bhat <- chat * s^2 / (a + s^2)
    v <- (1 - Bhat) * x + mean
    V <- estvar
    list(v = v, V = V, Bhat = Bhat)
}

ThreshFDREM <- function(lam, mean = 0, estvar, Bhat, s, alpha, tail = "L") {
    # assume G = N(0, V)
    gx <- seq(mean - 12 * sqrt(estvar), mean + 12 * sqrt(estvar), length = 100)
    G <- list(x = gx, y = dnorm(gx, mean = mean, sd = sqrt(estvar)), sd = sqrt(estvar))
    if (tail == "R") {
        cnull <- qnorm(1 - alpha, mean = mean, sd = sqrt(estvar))
        n <- length(s)
        nu <- rep(NA, n)
        cnu <- rep(NA, n)
        for (j in 1:n) {
            cut <- (lam - mean) / (1 - Bhat[j]) + mean
            nu[j] <- sum(G$y * (1 - pnorm(cut - G$x, sd = s[j])))
            cnu[j] <- sum(G$y * (G$x < cnull) * (1 - pnorm(cut - G$x, sd = s[j])))
        }
        mean(cnu) / mean(nu)
    } else {
        cnull <- qnorm(alpha, mean = mean, sd = sqrt(estvar))
        n <- length(s)
        nu <- rep(NA, n)
        cnu <- rep(NA, n)
        for (j in 1:n) {
            cut <- (lam - mean) / (1 - Bhat[j]) + mean
            nu[j] <- sum(G$y * (pnorm(cut - G$x, sd = s[j])))
            cnu[j] <- sum(G$y * (G$x >= cnull) * (pnorm(cut - G$x, sd = s[j])))
        }
        mean(cnu) / mean(nu)
    }
}

selectR1d <- function(Z, alpha = 0.04, gamma = 0.10) {
    Rules <- c("TPKW", "TP", "PMKW", "PMKWs", "MLE1", "P-val", "E&M", "JS")
    A <- matrix(0, length(Z$S), length(Rules)) # FDR rules
    B <- matrix(0, length(Z$S), length(Rules)) # Capacity rules
    dimnames(A)[[2]] <- Rules
    dimnames(B)[[2]] <- Rules
    for (k in 1:2) { # KW rules
        # Posterior Tail Probability Selection
        G <- if (k == 1) Z$f else Z$fs
        cnull <- qKW(G, 1 - alpha)
        tp <- Lfdr.GLmix_temp(Z$S, G, sqrt(Z$s), cnull = cnull)
        t0 <- quantile(tp, 1 - alpha)
        t1 <- try(Finv(gamma, ThreshFDR,
            interval = c(0.1, 0.9), stat = tp,
            v = tp
        ), silent = TRUE)
        if (inherits(t1, "try-error")) t1 <- NULL
        A[which(tp > max(t0, t1)), k] <- 1
        B[which(tp > t0), k] <- 1
        # Posterior Mean Selection
        pm <- predict(G, Z$S, newsigma = sqrt(Z$s))
        t0 <- quantile(pm, 1 - alpha)
        t1 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)
        if (inherits(t1, "try-error")) t1 <- NULL
        A[which(pm > max(t0, t1)), 2 + k] <- 1
        B[which(pm > t0), 2 + k] <- 1
    }
    # Naive Rules (All use tp from fs fit)
    for (k in 1:2) {
        R <- with(Z, switch(k,
            S,
            ((S - 1) * 1 / sqrt(s))
        ))
        t0 <- quantile(R, 1 - alpha)
        t1 <- try(Finv(gamma, ThreshFDR, stat = R, v = tp), silent = TRUE)
        if (inherits(t1, "try-error")) t1 <- NULL
        A[which(R > max(t0, t1)), 4 + k] <- 1
        B[which(R > t0), 4 + k] <- 1
    }

    # Efron and Morris
    # estmean = mean(Z$S)
    # estvar = uniroot(psi, c(0.001, 2), s = (Z$S-estmean)^2, d = Z$s, extendInt = "yes")$root
    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    R <- EMrule(Z$S, sqrt(Z$s), mean = estmean, estvar = estvar)
    t0 <- quantile(R$v, 1 - alpha)
    t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = R$Bhat, s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(R$v > max(t0, t1)), 7] <- 1
    B[which(R$v > t0), 7] <- 1
    R <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
    t0 <- quantile(R, 1 - alpha)
    t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + Z$s), s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(R > max(t0, t1)), 8] <- 1
    B[which(R > t0), 8] <- 1
    list(A = A, B = B)
}

selectL1d <- function(Z, alpha = 0.20, gamma = 0.20) {
    Rules <- c("TPKW", "TP", "PMKW", "PMKWs", "MLE1", "P-val", "E&M", "JS")
    A <- matrix(0, length(Z$S), length(Rules)) # FDR rules
    B <- matrix(0, length(Z$S), length(Rules)) # Capacity rules
    dimnames(A)[[2]] <- Rules
    dimnames(B)[[2]] <- Rules
    for (k in 1:2) { # KW rules
        # Posterior Tail Probability Selection
        G <- if (k == 1) Z$f else Z$fs
        cnull <- qKW(G, alpha)
        tp <- Lfdr.GLmix_temp(Z$S, G, sqrt(Z$s), cnull = cnull, tail = "L")
        t0 <- quantile(tp, 1 - alpha)
        t1 <- try(Finv(gamma, ThreshFDR,
            interval = c(0.01, 0.99), stat = tp,
            v = tp
        ), silent = TRUE)
        if (inherits(t1, "try-error")) t1 <- NULL
        A[which(tp > max(t0, t1)), k] <- 1
        B[which(tp > t0), k] <- 1
        # Posterior Mean Selection
        pm <- predict(G, Z$S, newsigma = sqrt(Z$s))
        t0 <- quantile(pm, alpha)
        t1 <- try(Finv(gamma, ThreshFDR,
            interval = c(-1.6, -0.8),
            stat = -pm, v = tp
        ), silent = TRUE)
        if (inherits(t1, "try-error")) t1 <- NULL
        A[which(-pm > max(-t0, t1)), 2 + k] <- 1
        B[which(-pm > -t0), 2 + k] <- 1
    }
    # Naive Rules (All use tp from fs fit)
    for (k in 1:2) {
        int <- if (k < 2) c(-1.45, -0.75) else c(0.1, 0.999999)
        R <- with(Z, switch(k,
            S,
            (-(S - 1) * 1/sqrt(s))
        ))
        if (k < 2) {
            t0 <- quantile(R, alpha)
            t1 <- try(Finv(gamma, ThreshFDR,
                interval = int,
                stat = -R, v = tp
            ), silent = TRUE)
            if (inherits(t1, "try-error")) t1 <- NULL
            A[which(-R > max(-t0, t1)), 4 + k] <- 1
            B[which(-R > -t0), 4 + k] <- 1
        } else {
            t0 <- quantile(R, 1 - alpha)
            t1 <- try(Finv(gamma, ThreshFDR,
                interval = int,
                stat = R, v = tp
            ), silent = TRUE)
            if (inherits(t1, "try-error")) t1 <- NULL
            A[which(R > max(t0, t1)), 4 + k] <- 1
            B[which(R > t0), 4 + k] <- 1
        }
    }
    # Efron and Morris
    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    R <- EMrule(Z$S, sqrt(Z$s), mean = estmean, estvar = estvar)
    t0 <- quantile(R$v, alpha)
    t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = R$Bhat, s = sqrt(Z$s), alpha = alpha, tail = "L"), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(-R$v > max(-t0, -t1)), 7] <- 1
    B[which(-R$v > -t0), 7] <- 1
    # Linear rule
    R <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
    t0 <- quantile(R, alpha)
    t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + Z$s), s = sqrt(Z$s), alpha = alpha, tail = "L"), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(-R > max(-t0, -t1)), 8] <- 1
    B[which(-R > -t0), 8] <- 1
    list(A = A, B = B)
}

level_plot <- function(Z, alpha = 0.04, gamma = 0.2, tail = "R", cindex = c(2, 5), constraint = "cap",
                       ygrid = seq(1, 1.6, length = 300), wgrid = 8 * (1:100)) {
    cols <- c("grey", "blue", "red")
    Rules <- c("TPKW", "TP", "PMKW", "PMKWs", "MLE", "Z-score", "E&M", "JS")
    # estmean = mean(Z$S)
    # estvar = uniroot(psi, c(0.001, 4), s = (Z$S-estmean)^2, d = Z$s, extendInt = "yes")$root
    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    pmr <- predict(Z$f, Z$S, newsigma = sqrt(Z$s))
    pm <- predict(Z$fs, Z$S, newsigma = sqrt(Z$s))
    em <- EMrule(Z$S, sqrt(Z$s), mean = estmean, estvar = estvar)
    linear <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
    cls <- array(NA, c(length(ygrid), length(wgrid), length(Rules)))
    if (7 %in% cindex) { # more costly to do EM level curve
        for (i in 1:length(ygrid)) {
            for (j in 1:length(wgrid)) {
                cls[i, j, 7] <- EMrule(c(ygrid[i], Z$S), c(sqrt(1 / wgrid[j]), sqrt(Z$s)), mean = estmean, estvar = estvar)$v[1]
            }
            print(i)
        }
    }
    if (tail == "R") {
        sR <- selectR1d(Z, alpha = alpha, gamma = gamma)
        zv <- ((Z$S - 1) * sqrt(1/Z$s))
        cnullr <- qKW(Z$f, 1 - alpha)
        cnull <- qKW(Z$fs, 1 - alpha)
        tpr <- Lfdr.GLmix_temp(Z$S, Z$f, sqrt(Z$s), cnull = cnullr)
        tp <- Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = cnull)
        ttpr0 <- quantile(tpr, 1 - alpha)
        ttpr1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tpr, v = tpr), silent = TRUE)
        ttp0 <- quantile(tp, 1 - alpha)
        ttp1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tp, v = tp), silent = TRUE)
        tMLE0 <- quantile(Z$S, 1 - alpha)
        tMLE1 <- try(Finv(gamma, ThreshFDR, stat = Z$S, v = tp), silent = TRUE)
        tpmr0 <- quantile(pmr, 1 - alpha)
        tpmr1 <- try(Finv(gamma, ThreshFDR, stat = pmr, v = tpr), silent = TRUE)
        tpm0 <- quantile(pm, 1 - alpha)
        tpm1 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)
        tzv0 <- quantile(zv, 1 - alpha)
        tzv1 <- try(Finv(gamma, ThreshFDR, stat = zv, v = tp), silent = TRUE)
        tem0 <- quantile(em$v, 1 - alpha)
        tem1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = em$Bhat, s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
        tlm0 <- quantile(linear, 1 - alpha)
        tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
        thresh <- matrix(NA, length(Rules), 2)
        thresh[1, ] <- c(ttpr0, max(ttpr0, ttpr1))
        thresh[2, ] <- c(ttp0, max(ttp0, ttp1))
        thresh[3, ] <- c(tpmr0, max(tpmr0, tpmr1))
        thresh[4, ] <- c(tpm0, max(tpm0, tpm1))
        thresh[5, ] <- c(tMLE0, max(tMLE0, tMLE1))
        thresh[6, ] <- c(tzv0, max(tzv0, tzv1))
        thresh[7, ] <- c(tem0, max(tem0, tem1))
        thresh[8, ] <- c(tlm0, max(tlm0, tlm1))
        for (i in 1:length(ygrid)) {
            for (j in 1:length(wgrid)) {
                cls[i, j, 1] <- Lfdr.GLmix_temp(ygrid[i], Z$f, 1 / sqrt(wgrid[j]), cnull = cnullr)
                cls[i, j, 2] <- Lfdr.GLmix_temp(ygrid[i], Z$fs, 1 / sqrt(wgrid[j]), cnull = cnull)
                cls[i, j, 3] <- predict(Z$f, ygrid[i], newsigma = 1 / sqrt(wgrid[j]))
                cls[i, j, 4] <- predict(Z$fs, ygrid[i], newsigma = 1 / sqrt(wgrid[j]))
                cls[i, j, 5] <- ygrid[i]
                cls[i, j, 6] <- ((ygrid[i] - 1) * sqrt(wgrid[j]))
                cls[i, j, 8] <- estmean + (ygrid[i] - estmean) * estvar / (estvar + (1 / wgrid[j]))
            }
        }
        A <- sR$A # cap and fdr constraint
        B <- sR$B # cap constraint
        Bagree <- intersect(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
        Bdis1 <- setdiff(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
        Bdis2 <- setdiff(which(B[, cindex[2]] == 1), which(B[, cindex[1]] == 1))
        Aagree <- intersect(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
        Adis1 <- setdiff(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
        Adis2 <- setdiff(which(A[, cindex[2]] == 1), which(A[, cindex[1]] == 1))
        # par(mfrow=c(1,2))
        # cap constraint
        if (constraint == "cap") {
            if (cindex[1] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[1]]],
                    lwd = 2, col = 4,
                    levels = round(thresh[cindex[1], 1], digits = 3),
                    xlab = "T", ylab = "W", drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[1], 1], lwd = 2, col = 4)
            }
            if (cindex[2] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[2]]],
                    lwd = 2,
                    levels = round(thresh[cindex[2], 1], digits = 3),
                    xlab = "T", ylab = "W", add = TRUE, col = 2, drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[2], 1], lwd = 2, col = 4)
            }
            points(Z$S[Bagree], Z$W[Bagree], col = "grey", cex = 0.5)
            points(Z$S[Bdis1], Z$W[Bdis1], col = 4, cex = 0.5)
            points(Z$S[Bdis2], Z$W[Bdis2], col = 2, cex = 0.5)
            text <- c(
                "Agree", paste(Rules[cindex[1]], " extra", sep = ""),
                paste(Rules[cindex[2]], " extra", sep = "")
            )
            legend("topleft", text,
                col = cols, pch = 1, cex = 0.95, bty = "n"
            )
        }
        # fdr constraint
        else {
            if (cindex[1] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[1]]],
                    lwd = 2, col = 4,
                    levels = round(thresh[cindex[1], 2], digits = 3),
                    xlab = "T", ylab = "W", drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[1], 2], lwd = 2, col = 4)
            }
            if (cindex[2] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[2]]],
                    lwd = 2,
                    levels = round(thresh[cindex[2], 2], digits = 3),
                    xlab = "T", ylab = "W", add = TRUE, col = 2, drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[2], 2], lwd = 2, col = 4)
            }
            points(Z$S[Aagree], Z$W[Aagree], col = "grey", cex = 0.5)
            points(Z$S[Adis1], Z$W[Adis1], col = 4, cex = 0.5)
            points(Z$S[Adis2], Z$W[Adis2], col = 2, cex = 0.5)
            text <- c(
                "Agree", paste(Rules[cindex[1]], " extra", sep = ""),
                paste(Rules[cindex[2]], " extra", sep = "")
            )
            legend("topright", text, col = cols, pch = 1, cex = 0.95, bty = "n")
        }
    }
    if (tail == "L") {
        sL <- selectL1d(Z, alpha = alpha, gamma = gamma)
        zv <- ((1 - Z$S) * sqrt(1/Z$s))
        cnullr <- qKW(Z$f, alpha)
        cnull <- qKW(Z$fs, alpha)
        tpr <- Lfdr.GLmix_temp(Z$S, Z$f, sqrt(Z$s), cnull = cnullr, tail = "L")
        tp <- Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = cnull, tail = "L")
        ttpr0 <- quantile(tpr, 1 - alpha)
        ttpr1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tpr, v = tpr), silent = TRUE)
        ttp0 <- quantile(tp, 1 - alpha)
        ttp1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tp, v = tp), silent = TRUE)
        tMLE0 <- quantile(Z$S, alpha)
        tMLE1 <- -try(Finv(gamma, ThreshFDR, interval = c(-1.45, -0.75), stat = -Z$S, v = tp), silent = TRUE) # control FDR by plugging smoothed KW estimates
        tpmr0 <- quantile(pmr, alpha)
        tpmr1 <- -try(Finv(gamma, ThreshFDR, interval = c(-1.6, -0.8), stat = -pmr, v = tpr), silent = TRUE)
        tpm0 <- quantile(pm, alpha)
        tpm1 <- -try(Finv(gamma, ThreshFDR, interval = c(-1.6, -0.8), stat = -pm, v = tp), silent = TRUE)
        tzv0 <- quantile(zv, 1 - alpha)
        tzv1 <- try(Finv(gamma, ThreshFDR, stat = zv, v = tp), silent = TRUE) # control FDR by plugging smoothed KW estimates
        tem0 <- quantile(em$v, alpha)
        tem1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = em$Bhat, s = sqrt(Z$s), alpha = alpha, tail = "L"), silent = TRUE) # control FDR by plugging Gaussian G estimates
        tlm0 <- quantile(linear, alpha)
        tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = "L"), silent = TRUE) # control FDR by plugging Gaussian G estimates

        thresh <- matrix(NA, length(Rules), 2)
        thresh[1, ] <- c(ttpr0, max(ttpr0, ttpr1))
        thresh[2, ] <- c(ttp0, max(ttp0, ttp1))
        thresh[3, ] <- c(tpmr0, min(tpmr0, tpmr1))
        thresh[4, ] <- c(tpm0, min(tpm0, tpm1))
        thresh[5, ] <- c(tMLE0, min(tMLE0, tMLE1))
        thresh[6, ] <- c(tzv0, max(tzv0, tzv1))
        thresh[7, ] <- c(tem0, min(tem0, tem1))
        thresh[8, ] <- c(tlm0, min(tlm0, tlm1))
        for (i in 1:length(ygrid)) {
            for (j in 1:length(wgrid)) {
                cls[i, j, 1] <- Lfdr.GLmix_temp(ygrid[i], Z$f, 1 / sqrt(wgrid[j]), cnull = cnullr, tail = "L")
                cls[i, j, 2] <- Lfdr.GLmix_temp(ygrid[i], Z$fs, 1 / sqrt(wgrid[j]), cnull = cnull, tail = "L")
                cls[i, j, 3] <- predict(Z$f, ygrid[i], newsigma = 1 / sqrt(wgrid[j]))
                cls[i, j, 4] <- predict(Z$fs, ygrid[i], newsigma = 1 / sqrt(wgrid[j]))
                cls[i, j, 5] <- ygrid[i]
                cls[i, j, 6] <- ((1 - ygrid[i]) * sqrt(wgrid[j]))
                cls[i, j, 8] <- estmean + (ygrid[i] - estmean) * estvar / (estvar + (1 / wgrid[j]))
            }
        }
        A <- sL$A # cap and fdr constraint
        B <- sL$B # cap constraint
        Bagree <- intersect(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
        Bdis1 <- setdiff(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
        Bdis2 <- setdiff(which(B[, cindex[2]] == 1), which(B[, cindex[1]] == 1))
        Aagree <- intersect(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
        Adis1 <- setdiff(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
        Adis2 <- setdiff(which(A[, cindex[2]] == 1), which(A[, cindex[1]] == 1))
        # par(mfrow=c(1,2))
        # cap constraint
        if (constraint == "cap") {
            if (cindex[1] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[1]]],
                    lwd = 2, col = 4,
                    levels = round(thresh[cindex[1], 1], digits = 3),
                    xlab = "T", ylab = "W", drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[1], 1], lwd = 2, col = 2)
            }
            if (cindex[2] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[2]]],
                    lwd = 2,
                    levels = round(thresh[cindex[2], 1], digits = 3),
                    xlab = "T", ylab = "W", add = TRUE, col = 2, drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[2], 1], lwd = 2, col = 2)
            }
            points(Z$S[Bagree], Z$W[Bagree], col = "grey", cex = 0.5)
            points(Z$S[Bdis1], Z$W[Bdis1], col = 4, cex = 0.5)
            points(Z$S[Bdis2], Z$W[Bdis2], col = 2, cex = 0.5)
            text <- c(
                "Agree", paste(Rules[cindex[1]], " extra", sep = ""),
                paste(Rules[cindex[2]], " extra", sep = "")
            )
            legend("topleft", text, col = cols, pch = 1, cex = 0.95, bty = "n")
        } else {
            # fdr constraint
            if (cindex[1] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[1]]],
                    lwd = 2, col = 4,
                    levels = round(thresh[cindex[1], 2], digits = 3),
                    xlab = "T", ylab = "W", drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[1], 2], lwd = 2, col = 2)
            }
            if (cindex[2] != 5) {
                contour(ygrid, wgrid, cls[, , cindex[[2]]],
                    lwd = 2,
                    levels = round(thresh[cindex[2], 2], digits = 3),
                    xlab = "T", ylab = "W", add = TRUE, col = 2, drawlabels = FALSE
                )
            } else {
                abline(v = thresh[cindex[2], 2], lwd = 2, col = 2)
            }
            points(Z$S[Aagree], Z$W[Aagree], col = "grey", cex = 0.5)
            points(Z$S[Adis1], Z$W[Adis1], col = 4, cex = 0.5)
            points(Z$S[Adis2], Z$W[Adis2], col = 2, cex = 0.5)
            text <- c(
                "Agree", paste(Rules[cindex[1]], "extra", sep = ""),
                paste(Rules[cindex[2]], " extra", sep = "")
            )
            legend("topleft", text,
                col = cols, pch = 1, cex = 0.95, bty = "n"
            )
            # mtext("A rated centers", side = 3,line = -2,outer = TRUE)
        }
    }
    list(cls = cls, thresh = thresh)
}


# selectR1d <- function(Z, alpha = 0.4, gamma = 0.10) {
# ---- setting up the matrix to store the results ---- #
# Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "P-val", "E&M", "JS")
# nobs <- length(Z$id) # the number of entities
# nrule <- length(Rules) # the number of rules
# A <- matrix(0, nrow = nobs, ncol = nrule) # FDR rule + Capacity rule
# B <- matrix(0, nrow = nobs, ncol = nrule) # Capacity rule only
# dimnames(A)[[2]] <- Rules
# dimnames(B)[[2]] <- Rules
# # ----  KW smoothed and non smoothed  ---- #
# for (k in 1:2) {
#     # ---- k=1, KW, k=2, KWsmooth ---- #
#     G <- if (k == 1) Z$f else Z$fs # G is the estimate of the mixing density
#     for (t in 1:3) {
#         # ---- t=1, Posterior Tail Probability, t=2, Posterior Mean ---- #
#         # ---- 1. Posterior Tail Probability ---- #
#         if (t == 1) {
#             cnull <- qKW(G, 1 - alpha) # cnull is quantiles of KW fit
#             # ---- calculate the posterior tail probability given G ---- #
#             tp <- Lfdr.GLmix_temp(Z$S, G, s = Z$s, cnull = cnull)
#             # s is the known heterogenous variance
#             t0 <- quantile(tp, 1 - alpha) # t0 is the quantile of the posterior tail probability
#             t1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tp, v = tp), silent = TRUE)
#             # gamma is the FDR rate
#             if (inherits(t1, "try-error")) t1 <- NULL
#             # error handling: if t1 has class "try-error", then t1 = NULL
#             A[which(tp > max(t0, t1)), k + 2(t - 1)] <- 1 # FDR + Capacity
#             B[which(tp > t0), k + 2(t - 1)] <- 1 # Capacity only
#         }
#         # ---- 2. Posterior Mean ---- #
#         if (t == 2) {
#             pm <- predict(G, Z$S, newsigma = sqrt(Z$s)) # predict the posterior mean
#             t0 <- quantile(pm, 1 - alpha)
#             t1 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)
#             if (inherits(t1, "try-error")) t1 <- NULL
#             A[which(pm > max(t0, t1)), k + 2 * (t - 1)] <- 1
#             B[which(pm > t0), t + k - 1] <- 1
#         }
#         # ---- 3. MLE ---- #
#         if (t == 3) {
#             R <- with(Z, switch(k,
#                 S,
#                 ((S - 1) * sqrt(W))
#             )) # if S=k, then R = (S-1)*sqrt(W), otherwise R = S
#             t0 <- quantile(R, 1 - alpha)
#             t1 <- try(Finv(gamma, ThreshFDR, stat = R, v = tp), silent = TRUE)
#             if (inherits(t1, "try-error")) t1 <- NULL
#             A[which(R > max(t0, t1)), k + 2(t - 1)] <- 1
#             B[which(R > t0), k + 2(t - 1)] <- 1
#         }
#     }
# }
# # ---- 4. E&M ---- #

# est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
# estmean <- est[1]
# estvar <- est[2]
# R <- EMrule(Z$S, sqrt(Z$s), mean = estmean, estvar = estvar)
# t0 <- quantile(R$v, 1 - alpha)
# t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = R$Bhat, s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
# if (inherits(t1, "try-error")) t1 <- NULL
# A[which(R$v > max(t0, t1)), 7] <- 1
# B[which(R$v > t0), 7] <- 1

# # ---- 5. JS ---- #

# R <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
# t0 <- quantile(R, 1 - alpha)
# t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = 1 / Z$W / (estvar + 1 / Z$W), s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
# if (inherits(t1, "try-error")) t1 <- NULL
# A[which(R > max(t0, t1)), 8] <- 1
# B[which(R > t0), 8] <- 1



# return(list(A = A, B = B))
# }
