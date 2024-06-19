rm(list = ls())
pacman::p_load(rDEA, data.table)
dt_md <- readRDS("Data/Out/dt_md_pool.rds")
output <- c(
    "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "VEN_TOT", "SEJ_HTP_TOT", "ENTSSR", "SEJ_HAD"
)
input <- c("ETP_INF", "EFF_MD", "ETP_AID")
Y <- as.data.frame(dt_md[AN == 2016, ..output])
X <- as.data.frame(dt_md[AN == 2016, ..input])

di_naive <- dea(XREF = X, YREF = Y, X = X, Y = Y, model = "input", RTS = "variable")
View(cbind(di_naive$thetaOpt, dt_md[AN == 2016, .(AN, FI, STJR)]))


# Example
## load data on Japanese hospitals (Besstremyannaya 2013, 2011)
data("hospitals", package = "rDEA")
## inputs and outputs for analysis
Y <- hospitals[c("inpatients", "outpatients")]
X <- hospitals[c("labor", "capital")]
W <- hospitals[c("labor_price", "capital_price")]
## Naive input-oriented DEA score for the first 20 firms under variable returns-to-scale
firms <- 1:20
di_naive <- dea(XREF = X, YREF = Y, X = X[firms, ], Y = Y[firms, ], model = "input", RTS = "variable")
di_naive$thetaOpt
## Naive DEA score in cost-minimization model for the first 20 firms under variable returns-to-scale
ci_naive <- dea(
    XREF = X, YREF = Y, X = X[firms, ], Y = Y[firms, ], W = W[firms, ],
    model = "costmin", RTS = "variable"
)
ci_naive$XOpt
ci_naive$gammaOpt
