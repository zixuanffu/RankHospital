add_log <- function(var_list) {
    #' @title Add log to a list of variables
    len <- length(var_list)
    log_left <- rep("log(", length(len))
    log_right <- rep(")", length(len))
    log_var_list <- paste0(log_left, var_list, log_right)
    return(log_var_list)
}
extract.plm <- function(
    model, include.rsquared = TRUE, include.adjrs = FALSE,
    include.nobs = TRUE, ...) {
    s <- summary(model, ...)
    coefficient.names <- rownames(s$coefficients)
    coefficients <- s$coefficients[, 1]
    standard.errors <- s$coefficients[, 2]
    significance <- s$coefficients[, 4]
    rs <- s$r.squared[1]
    adjrs <- s$r.squared[2]
    n <- length(s$residuals)
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.rsquared == TRUE) {
        gof <- c(gof, rs)
        gof.names <- c(gof.names, "R$^2$")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.adjrs == TRUE) {
        gof <- c(gof, adjrs)
        gof.names <- c(gof.names, "Adj. R$^2$")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.nobs == TRUE) {
        gof <- c(gof, n)
        gof.names <- c(gof.names, "Num. obs.")
        gof.decimal <- c(gof.decimal, FALSE)
    }
    tr <- createTexreg(
        coef.names = coefficient.names, coef = coefficients,
        se = standard.errors, pvalues = significance, gof.names = gof.names,
        gof = gof, gof.decimal = gof.decimal
    )
    return(tr)
}

setMethod("extract",
    signature = className("plm", "plm"),
    definition = extract.plm
)

extract.pggls <- function(
    model, include.rsquared = TRUE, include.adjrs = TRUE,
    include.nobs = TRUE, ...) {
    s <- summary(model, ...)
    coefficient.names <- rownames(s$CoefTable)
    coefficients <- s$CoefTable[, 1]
    standard.errors <- s$CoefTable[, 2]
    significance <- s$CoefTable[, 4]
    rs <- s$rsqr
    n <- length(s$resid)
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.rsquared == TRUE) {
        gof <- c(gof, rs)
        gof.names <- c(gof.names, "R$^2$")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.nobs == TRUE) {
        gof <- c(gof, n)
        gof.names <- c(gof.names, "Num. obs.")
        gof.decimal <- c(gof.decimal, FALSE)
    }
    tr <- createTexreg(
        coef.names = coefficient.names, coef = coefficients,
        se = standard.errors, pvalues = significance, gof.names = gof.names,
        gof = gof, gof.decimal = gof.decimal
    )
    return(tr)
}

setMethod("extract",
    signature = className("pggls", "plm"),
    definition = extract.pggls
)
