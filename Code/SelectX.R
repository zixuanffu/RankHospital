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
    pdt <- pdt[Var_res1 > 0.001]
    pdt[, id := as.numeric(as.factor(FI))]
    dt2 <- pdt[, .(id = first(id), hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI)]
    # ---- Hetereogeneous Known Variance ---- #
    f <- GLmix(dt2$hat_mu, sigma = sqrt(dt2$Var_res1), verb = 5) # assume the estimated variance is the known variance
    fs <- KWsmooth(f, bw = bwKW(f, bwt))

    list(f = f, fs = fs, S = dt2$hat_mu, s = dt2$Var_res1, id = dt2$id, pdt = pdt, dt2 = dt2)
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

# ---- For EM/JS Rule ---- #
P <- function(a, d) 1 / (2 * (a + d)^2) # 1/Var(S)
psi <- function(a, s, d) a - sum((s - d) * P(a, d)) / sum(P(a, d))
lik <- function(theta, x, s) {
    sum(log(theta[2] + s^2)) + sum((x - theta[1])^2 / (theta[2] + s^2))
}

# ---- For EM/JS, compute the FDR rate when setting treshold lambda = lam ---- #

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

error_avoid <- function(x, pos = TRUE) {
    if (inherits(x, "try-error")) {
        x <- NA
    } else if (pos) {
        x <- x
    } else {
        x <- -x
    }
    return(x)
}
selectR1d <- function(Z, alpha = 0.20, gamma = 0.20) {
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
    # ---- store the ranking statistics, threshold_1, threshold_2 for teach rule ---- #
    stat <- matrix(NA, length(Z$S), length(Rules))
    thresh_0 <- matrix(NA, length(Rules))
    thresh_1 <- matrix(NA, length(Rules))
    thresh <- matrix(NA, length(Rules), 2)

    # ---- store the selection results ---- #
    A <- matrix(0, length(Z$S), length(Rules)) # FDR rules
    B <- matrix(0, length(Z$S), length(Rules)) # Capacity rules
    dimnames(A)[[2]] <- Rules
    dimnames(B)[[2]] <- Rules

    # ---- for each rule, calculate the ranking statistics---- #
    tpr <- Lfdr.GLmix_temp(Z$S, Z$f, sqrt(Z$s), cnull = qKW(Z$f, 1 - alpha), tail = "R")

    tp <- Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = qKW(Z$fs, 1 - alpha), tail = "R")

    pmr <- predict(Z$f, Z$S, newsigma = sqrt(Z$s))

    pm <- predict(Z$fs, Z$S, newsigma = sqrt(Z$s))

    R <- Z$S

    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    linear <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))


    # ---- for each rule, calculate the threshold ---- #
    ttpr0 <- quantile(tpr, 1 - alpha)
    ttpr1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tpr, v = tpr), silent = TRUE)
    ttp0 <- quantile(tp, 1 - alpha)
    ttp1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tp, v = tp), silent = TRUE)

    tpmr0 <- quantile(pmr, 1 - alpha)
    tpmr1 <- try(Finv(gamma, ThreshFDR, stat = pmr, v = tpr), silent = TRUE)
    tpmr1 <- error_avoid(tpmr1)
    tpm0 <- quantile(pm, 1 - alpha)
    tpm1 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)
    tpm1 <- error_avoid(tpm1)
    tMLE0 <- quantile(R, 1 - alpha)
    tMLE1 <- try(Finv(gamma, ThreshFDR, stat = R, v = tp), silent = TRUE) # control FDR by plugging smoothed KW estimates
    tMLE1 <- error_avoid(tMLE1)
    tlm0 <- quantile(linear, 1 - alpha)
    tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
    tlm1 <- error_avoid(tlm1)

    for (i in 1:length(Rules)) {
        statistics <- switch(i,
            {
                tpr
            },
            {
                tp
            },
            {
                pmr
            },
            {
                pm
            },
            {
                R
            },
            {
                linear
            }
        )
        threshold_0 <- switch(i,
            {
                ttpr0
            },
            {
                ttp0
            },
            {
                tpmr0
            },
            {
                tpm0
            },
            {
                tMLE0
            },
            {
                tlm0
            }
        )
        threshold_1 <- switch(i,
            {
                ttpr1
            },
            {
                ttp1
            },
            {
                tpmr1
            },
            {
                tpm1
            },
            {
                tMLE1
            },
            {
                tlm1
            }
        )
        stat[, i] <- statistics
        thresh_0[i] <- threshold_0
        thresh_1[i] <- threshold_1
        A[which(statistics > max(threshold_0, threshold_1)), i] <- 1
        B[which(statistics > threshold_0), i] <- 1
    }
    thresh <- cbind(thresh_0, pmax(thresh_0, thresh_1))
    list(A = A, B = B, stat = stat, thresh_0 = thresh_0, thresh_1 = thresh_1, thresh = thresh)
}

select1d <- function(Z, alpha = 0.20, gamma = 0.20, tail = "R") {
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
    # ---- store the ranking statistics, threshold_1, threshold_2 for teach rule ---- #
    stat <- matrix(NA, length(Z$S), length(Rules))
    thresh_0 <- matrix(NA, length(Rules))
    thresh_1 <- matrix(NA, length(Rules))
    thresh <- matrix(NA, length(Rules), 2)
    # ---- store the selection results ---- #
    A <- matrix(0, length(Z$S), length(Rules)) # FDR rules
    B <- matrix(0, length(Z$S), length(Rules)) # Capacity rules
    dimnames(A)[[2]] <- Rules
    dimnames(B)[[2]] <- Rules

    # ---- for each rule, calculate the ranking statistics---- #
    cnullr <- ifelse(tail == "R", qKW(Z$f, 1 - alpha), qKW(Z$f, alpha))
    cnull <- ifelse(tail == "R", qKW(Z$fs, 1 - alpha), qKW(Z$fs, alpha))
    tpr <- Lfdr.GLmix_temp(Z$S, Z$f, sqrt(Z$s), cnull = cnullr, tail = tail)
    tp <- Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = cnull, tail = tail)

    pmr <- predict(Z$f, Z$S, newsigma = sqrt(Z$s))

    pm <- predict(Z$fs, Z$S, newsigma = sqrt(Z$s))

    R <- Z$S

    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]
    linear <- estmean + (Z$S - estmean) * estvar / (estvar + (Z$s))


    # ---- for each rule, calculate the threshold ---- #
    ttpr0 <- quantile(tpr, 1 - alpha)
    ttpr1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tpr, v = tpr), silent = TRUE)
    ttpr1 <- error_avoid(ttpr1)
    ttp0 <- quantile(tp, 1 - alpha)
    ttp1 <- try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = tp, v = tp), silent = TRUE)
    ttp1 <- error_avoid(ttp1)

    if (tail == "R") {
        tpmr0 <- quantile(pmr, 1 - alpha)
        tpmr1 <- try(Finv(gamma, ThreshFDR, stat = pmr, v = tpr), silent = TRUE)
        tpmr1 <- error_avoid(tpmr1)
        tpm0 <- quantile(pm, 1 - alpha)
        tpm1 <- try(Finv(gamma, ThreshFDR, stat = pm, v = tp), silent = TRUE)
        tpm1 <- error_avoid(tpm1)
        tMLE0 <- quantile(R, 1 - alpha)
        tMLE1 <- try(Finv(gamma, ThreshFDR, stat = R, v = tp), silent = TRUE) # control FDR by plugging smoothed KW estimates
        tMLE1 <- error_avoid(tMLE1)
        tlm0 <- quantile(linear, 1 - alpha)
        tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = "R"), silent = TRUE)
        tlm1 <- error_avoid(tlm1)
    } else {
        tpmr0 <- quantile(pmr, alpha)
        tpmr1 <- try(Finv(gamma, ThreshFDR, interval = c(-1.6, -0.8), stat = -pmr, v = tpr), silent = TRUE)
        tpmr1 <- error_avoid(tpmr1, FALSE)
        tpm0 <- quantile(pm, alpha)
        tpm1 <- try(Finv(gamma, ThreshFDR, interval = c(-1.6, -0.8), stat = -pm, v = tp), silent = TRUE)
        tpm1 <- error_avoid(tpm1, FALSE)
        tMLE0 <- quantile(R, alpha)
        tMLE1 <- try(Finv(gamma, ThreshFDR, stat = -R, v = tp), silent = TRUE) # control FDR by plugging smoothed KW estimates
        tMLE1 <- error_avoid(tMLE1, FALSE)

        tlm0 <- quantile(linear, alpha)
        tlm1 <- try(Finv(gamma, ThreshFDREM, mean = estmean, estvar = estvar, Bhat = Z$s / (estvar + (Z$s)), s = sqrt(Z$s), alpha = alpha, tail = "L"), silent = TRUE)
    }
    for (i in 1:length(Rules)) {
        statistics <- switch(i,
            {
                tpr
            },
            {
                tp
            },
            {
                pmr
            },
            {
                pm
            },
            {
                R
            },
            {
                linear
            }
        )
        threshold_0 <- switch(i,
            {
                ttpr0
            },
            {
                ttp0
            },
            {
                tpmr0
            },
            {
                tpm0
            },
            {
                tMLE0
            },
            {
                tlm0
            }
        )
        threshold_1 <- switch(i,
            {
                ttpr1
            },
            {
                ttp1
            },
            {
                tpmr1
            },
            {
                tpm1
            },
            {
                tMLE1
            },
            {
                tlm1
            }
        )
        stat[, i] <- statistics
        thresh_0[i] <- threshold_0
        thresh_1[i] <- threshold_1

        if (tail == "R") {
            A[which(statistics > max(threshold_0, threshold_1)), i] <- 1
            B[which(statistics > threshold_0), i] <- 1
            thresh <- cbind(thresh_0, pmax(thresh_0, thresh_1))
        } else {
            if (i == 1 | i == 2) {
                A[which(statistics > max(threshold_0, threshold_1)), i] <- 1
                B[which(statistics > threshold_0), i] <- 1
                thresh[i, 1] <- threshold_0
                thresh[i, 2] <- max(threshold_0, threshold_1)
            } else {
                A[which(statistics < min(threshold_0, threshold_1)), i] <- 1
                B[which(statistics < threshold_0), i] <- 1
                thresh[i, 1] <- threshold_0
                thresh[i, 2] <- min(threshold_0, threshold_1)
            }
        }
    }
    list(A = A, B = B, stat = stat, thresh_0 = thresh_0, thresh_1 = thresh_1, thresh = thresh)
}
# ---- Level plot ---- #
grid_select <- function(Z, alpha, gamma, tail, cindex = c(2, 5), xgrid = seq(1, 1.6, length = 300), ygrid = 8 * (1:100)) {
    # ---- define rules ---- #
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
    # ---- define a grid for each rule ---- #
    # each rule has one grid
    # each point in the grid is a tuple (\hat{\theta}_i, \sigma_i)
    # for each rule, we will select a bunch of points in the grid
    # we will construct contour lines based on the selected points
    cnullr <- ifelse(tail == "R", qKW(Z$f, 1 - alpha), qKW(Z$f, alpha))
    cnull <- ifelse(tail == "R", qKW(Z$fs, 1 - alpha), qKW(Z$fs, alpha))

    est <- optim(c(0, 1), lik, x = Z$S, s = sqrt(Z$s))$par
    estmean <- est[1]
    estvar <- est[2]

    cls <- array(NA, c(length(xgrid), length(ygrid), length(Rules)))
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
    return(cls)
}
level_plot <- function(Z, Selection, cls, alpha = 0.04, gamma = 0.2, tail = "R", cindex = c(2, 5), constraint = "cap", xgrid = seq(1, 1.6, length = 300), ygrid = 8 * (1:100)) {
    # ---- define colors ---- #
    cols <- c("grey", "red", "blue")

    # ---- define rules ---- #
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
    # ---- read the selection results ---- #
    A <- Selection$A
    B <- Selection$B
    thresh <- Selection$thresh
    # ---- find the agreement and disagreement between the two selection rules ---- #
    Bagree <- intersect(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
    Bdis1 <- setdiff(which(B[, cindex[1]] == 1), which(B[, cindex[2]] == 1))
    Bdis2 <- setdiff(which(B[, cindex[2]] == 1), which(B[, cindex[1]] == 1))

    Aagree <- intersect(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
    Adis1 <- setdiff(which(A[, cindex[1]] == 1), which(A[, cindex[2]] == 1))
    Adis2 <- setdiff(which(A[, cindex[2]] == 1), which(A[, cindex[1]] == 1))

    # ---- plot the selection results ---- #
    if ("cap" %in% constraint) {
        if (Rules[[cindex[1]]] != "MLE") {
            contour(xgrid, log(ygrid), cls[, , cindex[[1]]],
                lwd = 2, col = 4,
                levels = round(thresh[cindex[1], 1], digits = 3),
                xlab = expression(theta[i]), ylab = expression(log(sigma[i]^2)), drawlabels = FALSE
            )
        } else {
            abline(v = thresh[cindex[1], 1], lwd = 2, col = 4) # MLE rule
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
        legend("topright", text,
            col = cols, pch = 1, cex = 0.95, bty = "n"
        )
        mtext(paste("Capacity ", Rules[cindex[1]], " vs ", Rules[cindex[2]], sep = ""))
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
                xlab = expression(theta[i]), ylab = expression(log(sigma[i]^2)), add = TRUE, col = 2, drawlabels = FALSE
            )
        } else {
            abline(v = thresh[cindex[2], 1], lwd = 2, col = 2)
        }
        points(Z$S[Aagree], log(Z$W[Aagree]), col = "grey", cex = 0.5)
        points(Z$S[Adis1], log(Z$W[Adis1]), col = 4, cex = 0.5)
        points(Z$S[Adis2], log(Z$W[Adis2]), col = 2, cex = 0.5)
        text <- c(
            "Agree", paste(Rules[cindex[1]], " extra", sep = ""),
            paste(Rules[cindex[2]], " extra", sep = "")
        )
        legend("topright", text, col = cols, pch = 1, cex = 0.95, bty = "n")
        mtext(paste("Capacity and FDR ", Rules[cindex[1]], " vs ", Rules[cindex[2]], sep = ""))
        title("(b)")
    }
}



select_plot_1d <- function(Z, s, alpha, gamma, tail, rule_index, sub = FALSE, filename, format = "pdf") {
    Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
    dt2 <- Z$pdt[, .(hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI, STJR)]
    A <- s$A[, rule_index]
    B <- s$B[, rule_index]
    selection <- cbind(dt2, A, B)
    if (format == "pdf") {
        pdf(filename, height = 4.5, width = 8)
    } else {
        png(filename, height = 450, width = 800)
    }
    par(mfrow = c(1, 2))
    if (sub) {
        pub <- selection[STJR == 1]
        pri <- selection[STJR == 2]
    } else {
        pub <- selection[STJR == 1 | STJR == 0]
        pri <- selection[STJR == 2 | STJR == 3]
    }
    n_pub <- nrow(pub)
    n_pri <- nrow(pri)
    B_pub <- pub[B == 1]
    B_pri <- pri[B == 1]
    plot(NULL, xlim = c(u_min, u_max), ylim = c(log(v_min), log(v_max)), xlab = expression(theta), ylab = expression(sigma^2))
    points(B_pri$hat_mu, log(B_pri$Var_res1), col = 4, cex = 0.5)
    points(B_pub$hat_mu, log(B_pub$Var_res1), col = 2, cex = 0.5)

    text <- c(paste0("Public: ", nrow(B_pub), "/", n_pub, "=", round(nrow(B_pub) / n_pub, 2)), paste0("Private: ", nrow(B_pri), "/", n_pri, "=", round(nrow(B_pub) / n_pub, 2)))
    legend("topright", text, col = c(2, 4), pch = 1, cex = 0.95, bty = "n")
    mtext(paste("alpha = ", alpha, "selected: ", nrow(B_pub) + nrow(B_pri)))
    title(paste("Rule: ", Rules[rule_index]))
    A_pub <- pub[A == 1]
    A_pri <- pri[A == 1]
    plot(NULL, xlim = c(u_min, u_max), ylim = c(log(v_min), log(v_max)), xlab = expression(theta), ylab = expression(sigma^2))
    points(A_pri$hat_mu, log(A_pri$Var_res1), col = 4, cex = 0.5)
    points(A_pub$hat_mu, log(A_pub$Var_res1), col = 2, cex = 0.5)

    text <- c(paste0("Public: ", nrow(A_pub), "/", n_pub, "=", round(nrow(A_pub) / n_pub, 2)), paste0("Private: ", nrow(A_pri), "/", n_pri, "=", round(nrow(A_pri) / n_pri, 2)))
    legend("topright", text, col = c(2, 4), pch = 1, cex = 0.95, bty = "n")
    mtext(paste("alpha = ", alpha, "gamma = ", gamma, "selected: ", nrow(A_pub) + nrow(A_pri)))
    title(paste("Rule: ", Rules[rule_index]))
    dev.off()
}
