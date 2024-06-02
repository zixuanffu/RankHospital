# evaluate FDR for different rules under just capacity constraints.
# assuming 1-dimensional G.
# using first 3 years of data.

pacman::p_load(REBayes, Hmisc)
source("Code/SelectX.R")
load("Results/2013-2022/ZsLsR.Rda")
z <- Z
n <- length(z$S)
pms <- predict(z$fs, z$S, newsigma = sqrt(z$s))
est <- optim(c(0, 1), lik, x = z$S, s = sqrt(z$s))$par
estmean <- est[1]
estvar <- est[2]
R <- estmean + (z$S - estmean) * estvar / (estvar + (z$s)) # James-Stein


# right
alphas <- c(4, 10, 15, 20, 25) / 100
FDRright <- matrix(NA, 4, length(alphas))
for (j in 1:length(alphas)) {
    alpha <- alphas[j]
    cnulls <- qKW(z$fs, 1 - alpha)
    tps <- Lfdr.GLmix_temp(z$S, z$fs, sqrt(z$s), cnull = cnulls)
    FDRright[4, j] <- ThreshFDR(quantile(R, 1 - alpha), R, tps) # James-Stein
    FDRright[3, j] <- ThreshFDR(quantile(z$S, 1 - alpha), z$S, tps) # MLE (Gaussian MLE, or FE)

    FDRright[2, j] <- ThreshFDR(quantile(pms, 1 - alpha), pms, tps) # Posterior mean
    FDRright[1, j] <- ThreshFDR(quantile(tps, 1 - alpha), tps, tps) # Posterior tp
}

# # left
# alphas = c(4,10,15,20,25)/100
# FDRleft = matrix(NA, 7, length(alphas))
# for (j in 1:length(alphas)){
# 	alpha = alphas[j]
# cnulls = qKW(z$fs, alpha)
# tps = Lfdr.GLmix_temp(z$S, z$fs, sqrt(1/z$W), cnull = cnulls, tail = "L")
# FDRleft[1,j] = ThreshFDR(-quantile(z$S,alpha), -z$S,tps)  # MLE (Gaussian MLE, or FE)
# FDRleft[2,j] = ThreshFDR(-quantile(z$D/z$E,alpha), -z$D/z$E,tps)  #P-MLE (Poisson MLE)
# FDRleft[3,j] = ThreshFDR(quantile((1-sqrt(z$D/z$E)) * sqrt(4*z$E), 1-alpha), (1-sqrt(z$D/z$E))*sqrt(4*z$E), tps)  #Pvalue
# FDRleft[4,j] = ThreshFDR(-quantile(EM$v, alpha), -EM$v, tps)  #Efron & Morris
# FDRleft[5,j] = ThreshFDR(-quantile(R, alpha), -R, tps) # James-Stein
# FDRleft[6,j] = ThreshFDR(-quantile(pms,alpha), -pms, tps)  #Posterior mean
# FDRleft[7,j] = ThreshFDR(quantile(tps, 1-alpha), tps,tps)   #Posterior tp
# }

# tab construction
require(Hmisc)
meths <- c("TP", "PM", "MLE", "James-Stein")
alphas <- c("$\\alpha=4\\%$", "$\\alpha=10\\%$", "$\\alpha=15\\%$", "$\\alpha=20\\%$", "$\\alpha=25\\%$")
# D <- rbind(round(FDRright, digits = 3), round(FDRleft, digits = 3))
D <- round(FDRright, digits = 3)
dimnames(D) <- list(rep(meths, 1), rep(alphas, 1))
cap <- "FDR Estimates: 2004-2006"
latex(D,
    file = "Results/2013-2022/FDR.tex", rowlabel = "",
    rgroup = c("Right Selection"),
    caption.loc = "bottom", caption = cap, label = "tab: naive_31_1d"
)
