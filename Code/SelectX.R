pacman::p_load(data.table, REBayes, docstring)

# ---- fit the 1D Gaussian mixture model ---- #

fit1d <- function(pdt, bwt = 2, rtol = 1e-20, ...) {
    #' @title: Fit a 1D Gaussian mixture model
    #' @description: Fit a 1D Gaussian mixture model
    #' @param pdt: a data.table with columns FI, Fixed effect, Residuals
    #' @param bwt: bandwidth for kernel smoothing
    #' @param rtol: relative tolerance for optimization
    #' @return: a list with the following components: the KW estimates of G, the smoothe KW estimates, the sufficient statistics S for the location parameter, the sufficient statistics s for the scale parameter, the id of the each entity

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

Lfdr.GLmix_temp <- function(x, G, s, cnull, tail = "R") {
    # Modified for outliers??
    #' @Description: Given an estimated mixing distribution, G, Lfdr computes an estimated local false discovery rate at a specified set of points and threshold value cnull. The argument G can be specified as the fitted object from one of several possible fitting routines for nonparametric mixing distributions.
    #' @param x: a vector of estimates \hat{\theta} each following N(\theta_i, \sigma_i^2)
    #' @param G: the estimated distribution of $\theta_i$
    #' @param s: a vector of \sigma_i
    #' @param cnull: the threshold value

    # changed for left tail selection

    v <- G$x # the grid points
    fv <- G$y # the density values
    A <- dnorm(outer(x, v, "-"), sd = s)
    # generate a matrix A with dim = (length(x), length(v))
    # at each point (i,j), give the density function of a normal distribution N(0, s[i]^2) evaluated at x[i] - v[j]
    # because at each point (i,j) we have the assumption x[i] ~ N(v[j], s[i]^2)
    A <- dnorm(outer(x, v, "-"), sd = s)
    if (tail == "R") {
        # the denominator is the probability of observing x[i] given the estimated distribution G
        # the nominator is the probability of observing x[i] AND its true mean value is smaller than cnull.
        # 1- the ratio is the conditional probability of having the true mean value smaller than cnull given the observed value x[i] P(\theta_i < cnull | x[i])
        v <- 1 - c((A %*% (fv * (v < cnull))) / (A %*% fv))
        # v is
    } else {
        # 1- the ratio is the conditional probability of having the true mean value larger than cnull given the observed value x[i] P(\theta_i > cnull | x[i])
        v <- 1 - c((A %*% (fv * (v >= cnull))) / (A %*% fv))
    }
}

# ---- For James Stein Rule ---- #
P <- function(a, d) 1 / (2 * (a + d)^2) # 1/Var(S)
psi <- function(a, s, d) a - sum((s - d) * P(a, d)) / sum(P(a, d))
lik <- function(theta, x, s) {
    sum(log(theta[2] + s^2)) + sum((x - theta[1])^2 / (theta[2] + s^2))
}

# ---- For EM and JS, Compute the threshold when setting FDR rate to gamma ---- #

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
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE1", "JS")
    A <- matrix(0, length(Z$S), length(Rules)) # FDR rules
    B <- matrix(0, length(Z$S), length(Rules)) # Capacity rules
    dimnames(A)[[2]] <- Rules
    dimnames(B)[[2]] <- Rules
    for (k in 1:2) { # KW rules
        # Posterior Tail Probability Selection
        G <- if (k == 1) Z$f else Z$fs
        cnull <- qKW(G, 1 - alpha)
        tp <- Lfdr.GLmix_temp(Z$S, G, sqrt(Z$s), cnull = cnull) # to generate tail probability for each point in Z$S
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

    # Naive Rules MLE
    R <- Z$S
    t0 <- quantile(R, 1 - alpha)
    t1 <- try(Finv(gamma, ThreshFDR, stat = R, v = tp), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(R > max(t0, t1)), 5] <- 1
    B[which(R > t0), 5] <- 1

    # James-Stein Rule
    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    R <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
    t0 <- quantile(R, 1 - alpha)
    t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + Z$s), s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(R > max(t0, t1)), 6] <- 1
    B[which(R > t0), 6] <- 1

    list(A = A, B = B)
}

selectL1d <- function(Z, alpha = 0.20, gamma = 0.20) {
    Rules <- c("TPKW", "TP", "PMKW", "PMKWs", "MLE1", "JS")
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
        t1 <- -try(Finv(gamma, ThreshFDR,
            interval = c(-1.6, -0.8),
            stat = -pm, v = tp
        ), silent = TRUE)
        if (inherits(t1, "try-error")) t1 <- NULL
        A[which(-pm > max(-t0, -t1)), 2 + k] <- 1
        B[which(-pm > -t0), 2 + k] <- 1
    }

    # Naive Rules MLE
    R <- Z$S
    t0 <- quantile(R, alpha)
    t1 <- -try(Finv(gamma, ThreshFDR, stat = -R, v = tp), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(-R > max(-t0, -t1)), 5] <- 1
    B[which(-R > -t0), 5] <- 1


    # James-Stein Rule
    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    R <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
    t0 <- quantile(R, alpha)
    t1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + Z$s), s = sqrt(Z$s), alpha = alpha, tail = "L"), silent = TRUE)
    if (inherits(t1, "try-error")) t1 <- NULL
    A[which(-R > max(-t0, -t1)), 6] <- 1
    B[which(-R > -t0), 6] <- 1

    list(A = A, B = B)
}

# 怎么right tail left tail 这么混乱啊？？？
# 一个一个来吧。。。
level_plot <- function(Z, alpha = 0.04, gamma = 0.2, tail = "R", cindex = c(2, 5), constraint = "cap",
                       xgrid = seq(1, 1.6, length = 300), ygrid = 8 * (1:100)) {
    nrows <- length(cindex) / 2
    ncols <- 2
    # ---- define color scheme ---- #
    cols <- c("grey", "blue", "red")
    # ---- define rules ---- #
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")

    # ---- JS rule ---- #
    estmean <- mean(Z$S)
    estvar <- uniroot(psi, c(0.001, 4), s = (Z$S - estmean)^2, d = Z$s, extendInt = "yes")$root
    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]

    pmr <- predict(Z$f, Z$S, newsigma = sqrt(Z$s))
    pm <- predict(Z$fs, Z$S, newsigma = sqrt(Z$s))
    linear <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))
    cls <- array(NA, c(length(xgrid), length(ygrid), length(Rules)))

    # ---- calculate the threshold \theta_\alpha ---- #
    if (tail == "R") {
        sR <- selectR1d(Z, alpha = alpha, gamma = gamma)
        cnullr <- qKW(Z$f, 1 - alpha) # non-smoothed KW estimates
        cnull <- qKW(Z$fs, 1 - alpha) # smoothed KW estimates
    } else {
        sL <- selectL1d(Z, alpha = alpha, gamma = gamma)
        cnullr <- qKW(Z$f, alpha) # non-smoothed KW estimates
        cnull <- qKW(Z$fs, alpha) # smoothed KW estimates
    }



    # ---- Find the threshold given by two constraints ---- #

    # ---- Posterior Tail Probability rule ---- #
    # ---- If tail == "R", calculate the tail probability v_\alpha(y_i) = P(\theta_i > \theta_\alpha|y_i) ---- #
    # ---- If tail == "L", calculate the tail probability v_\alpha(y_i) = P(\theta_i < \theta_\alpha|y_i) ---- #
    tpr <- Lfdr.GLmix_temp(Z$S, Z$f, sqrt(Z$s), cnull = cnullr, tail = tail)
    tp <- Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = cnull, tail = tail)
    # ---- tp is  used as ranking statistics ---- #


    # ---- Non Smoothed KW estimates ---- #
    # ---- compute the threshold given by cap = alpha ---- #
    # if tail == "L", we select the left tail by
    ttpr0 <- quantile(tpr, 1 - alpha)
    # ---- compute the threshold given by FDR = gamma ---- #
    ttpr1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tpr, v = tpr), silent = TRUE)
    # ---- Smoothed KW estimates ---- #
    # ---- compute the threshold given by cap = alpha ---- #
    ttp0 <- quantile(tp, 1 - alpha)
    # ---- compute the threshold given by FDR = gamma ---- #
    ttp1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tp, v = tp), silent = TRUE)

    if (tail == "R") {
        # ---- Posterior Mean rule ---- #
        tpmr0 <- quantile(pmr, 1 - alpha)
        tpmr1 <- try(Finv(gamma, ThreshFDR, stat = pmr, v = tpr), silent = TRUE)
        tpm0 <- quantile(pm, 1 - alpha)
        tpm1 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)
        # ---- MLE rule ---- #
        tMLE0 <- quantile(Z$S, 1 - alpha)
        tMLE1 <- try(Finv(gamma, ThreshFDR, stat = Z$S, v = tp), silent = TRUE)
        tlm0 <- quantile(linear, 1 - alpha)
        tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = tail), silent = TRUE)
    } else {
        tpmr0 <- quantile(pmr, alpha)
        tpmr1 <- -try(Finv(gamma, ThreshFDR, interval = c(-1.6, -0.8), stat = -pmr, v = tpr), silent = TRUE)
        tpm0 <- quantile(pm, alpha)
        tpm1 <- -try(Finv(gamma, ThreshFDR, interval = c(-1.6, -0.8), stat = -pm, v = tp), silent = TRUE)
        tMLE0 <- quantile(Z$S, alpha)
        tMLE1 <- -try(Finv(gamma, ThreshFDR, interval = c(-1.45, -0.75), stat = -Z$S, v = tp), silent = TRUE) # control FDR by plugging smoothed KW estimates
        tlm0 <- quantile(linear, alpha)
        tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = tail), silent = TRUE)
    }



    # ---- Define the max of the the two tresholds (\lambda_1 for cal, \lambda_2 for fdr) ---- #
    tail <- "L"
    maxmin <- ifelse(tail == "R", max, min)
    thresh <- matrix(NA, length(Rules), 2)
    thresh[1, ] <- c(ttpr0, maxmin(ttpr0, ttpr1))
    thresh[2, ] <- c(ttp0, maxmin(ttp0, ttp1))
    thresh[3, ] <- c(tpmr0, maxmin(tpmr0, tpmr1))
    thresh[4, ] <- c(tpm0, maxmin(tpm0, tpm1))
    thresh[5, ] <- c(tMLE0, maxmin(tMLE0, tMLE1))
    thresh[6, ] <- c(tlm0, maxmin(tlm0, tlm1))

    # ---- For each grid point (\hat{\theta}_i, \sigma_i), compute the criteria value for each rule ---- #
    for (k in cindex) {
        for (i in 1:length(xgrid)) {
            for (j in 1:length(ygrid)) {
                switch(k,
                    {
                        cls[i, j, 1] <- Lfdr.GLmix_temp(xgrid[i], Z$f, sqrt((ygrid[j])), cnull = cnullr, tail = tail)
                    },
                    {
                        cls[i, j, 2] <- Lfdr.GLmix_temp(xgrid[i], Z$fs, sqrt((ygrid[j])), cnull = cnull, tail = tail)
                    },
                    {
                        cls[i, j, 3] <- predict(Z$f, xgrid[i], newsigma = sqrt(ygrid[j]))
                    },
                    {
                        cls[i, j, 4] <- predict(Z$fs, xgrid[i], newsigma = sqrt(ygrid[j]))
                    },
                    {
                        cls[i, j, 5] <- xgrid[i]
                    },
                    {
                        cls[i, j, 6] <- estmean + (xgrid[i] - estmean) * estvar / (estvar + ((ygrid[j])))
                    }
                )
            }
        }
    }

    if (tail == "R") {
        B <- sR$B # cap constraint
        A <- sR$A # cap and fdr constraint
    } else {
        B <- sL$B
        A <- sL$A
    }


    # ---- From the real dataset, find the agreement and disagreement between the two rules ---- #



    Bagree <- intersect(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
    Bdis1 <- setdiff(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
    Bdis2 <- setdiff(which(B[, cindex[2]] == 1), which(B[, cindex[1]] == 1))

    Aagree <- intersect(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
    Adis1 <- setdiff(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
    Adis2 <- setdiff(which(A[, cindex[2]] == 1), which(A[, cindex[1]] == 1))

    # ---- For each grid point, plot the selection results ---- #

    # The left graph compares the the two rules under cap constraint

    # When plotting, we want to rescale the y-axis by log(y) to make the plot more readable

    if ("cap" %in% constraint) {
        if (Rules[[cindex[1]]] != "MLE") {
            contour(xgrid, log(ygrid), cls[, , cindex[[1]]],
                lwd = 2, col = 4,
                levels = round(thresh[cindex[1], 1], digits = 3),
                xlab = expression(theta[i]), ylab = expression(log(sigma[i]^2)), sub = paste("Capacity ", Rules[cindex[1]], " vs ", Rules[cindex[2]], sep = ""), drawlabels = FALSE
            )
        } else {
            abline(v = thresh[cindex[1], 1], lwd = 2, col = 4)[] # MLE rule
        }
        if (Rules[[cindex[2]]] != "MLE") {
            contour(xgrid, log(ygrid), cls[, , cindex[[2]]],
                lwd = 2,
                levels = round(thresh[cindex[2], 1], digits = 3),
                xlab = expression(theta[i]), ylab = expression(log(sigma[i]^2)), add = TRUE, col = 2, drawlabels = FALSE
            )
        } else {
            abline(v = thresh[cindex[2], 1], lwd = 2, col = 2) # MLE rule
        }
        points(Z$S[Bagree], log(Z$W[Bagree]), col = "grey", cex = 0.5)
        points(Z$S[Bdis1], log(Z$W[Bdis1]), col = 4, cex = 0.5)
        points(Z$S[Bdis2], log(Z$W[Bdis2]), col = 2, cex = 0.5)
        text <- c(
            "Agree", paste(Rules[cindex[1]], " extra", sep = ""),
            paste(Rules[cindex[2]], " extra", sep = "")
        )
        legend("topleft", text,
            col = cols, pch = 1, cex = 0.95, bty = "n"
        )
        title("(a)")
    }

    # The right graph compares the two rules under cap+fdr constraint
    if ("fdr" %in% constraint) {
        if (Rules[[cindex[1]]] != "MLE") {
            contour(xgrid, log(ygrid), cls[, , cindex[[1]]],
                lwd = 2, col = 4,
                levels = round(thresh[cindex[1], 2], digits = 3),
                xlab = expression(theta[i]), ylab = expression(log(sigma[i]^2)), drawlabels = FALSE
            )
        } else {
            abline(v = thresh[cindex[1], 2], lwd = 2, col = 4)
        }
        if (Rules[[cindex[2]]] != "MLE") {
            contour(xgrid, log(ygrid), cls[, , cindex[[2]]],
                lwd = 2,
                levels = round(thresh[cindex[2], 2], digits = 3),
                xlab = expression(theta[i]), ylab = expression(log(sigma[i]^2)), add = TRUE, col = 2, sub = paste("Capacity and FDR ", Rules[cindex[1]], " vs ", Rules[cindex[2]], sep = ""), drawlabels = FALSE
            )
        } else {
            abline(v = thresh[cindex[2], 2], lwd = 2, col = 2)
        }
        points(Z$S[Aagree], log(Z$W[Aagree]), col = "grey", cex = 0.5)
        points(Z$S[Adis1], log(Z$W[Adis1]), col = 4, cex = 0.5)
        points(Z$S[Adis2], log(Z$W[Adis2]), col = 2, cex = 0.5)
        text <- c(
            "Agree", paste(Rules[cindex[1]], " extra", sep = ""),
            paste(Rules[cindex[2]], " extra", sep = "")
        )
        legend("topleft", text, col = cols, pch = 1, cex = 0.95, bty = "n")
        title("(b)")
    }


    list(cls = cls, thresh = thresh)
}
