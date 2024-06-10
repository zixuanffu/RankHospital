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

# reg_X <- function(data, varl, varr, control = "CASEMIX", cluster = "FI", method = "ols") {
#     #' @title Regression using fixest package
#     #' @description This function is used to estimate the regression model using the fixest package.
#     #' @param data A data.table object.
#     #' @param varl A character vector of the left-hand side variables.
#     #' @param varr A character vector of the right-hand side variables.
#     #' @param control A character string of the control variables.
#     #' @param cluster A character string of the cluster variable.
#     #' @param method A character string of the method to be used. Either "ols" or "pois".
#     #' @return The regression result

#     formula <- xpd(lhs = add_log(varl), rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
#     if (method == "ols") {
#         formula <- xpd(lhs = add_log(varl), rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
#         res <- feols(formula, data)
#     }

#     if (method == "pois") {
#         formula <- xpd(lhs = varl, rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
#         res <- fepois(formula, data)
#     }

#     # saveRDS(res, paste0("Results/", year_start, "-", year_end, "/reg_", varl, "_", method, "_", cluster, ".rds"))
#     etable(res, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = paste0("Tables/2016-2022/reg_", varl, "_", method, ".tex"), signif.code = "letters", replace = TRUE)

#     return(res)
# }


# Error in validObject(.Object) :
#   invalid class "texreg" object: 1: invalid object for slot "coef.names" in class "texreg": got class "NULL", should be or extend class "character"
# invalid class "texreg" object: 2: invalid object for slot "coef" in class "texreg": got class "NULL", should be or extend class "numeric"
# invalid class "texreg" object: 3: invalid object for slot "se" in class "texreg": got class "NULL", should be or extend class "numeric"
# invalid class "texreg" object: 4: invalid object for slot "pvalues" in class "texreg": got class "NULL", should be or extend class "numeric"
