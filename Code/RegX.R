pacman::p_load(data.table, fixest, ggplot2, docstring)


add_log <- function(var_list) {
    #' @title Add log to a list of variables
    len <- length(var_list)
    log_left <- rep("log(", length(len))
    log_right <- rep(")", length(len))
    log_var_list <- paste0(log_left, var_list, log_right)
    return(log_var_list)
}


reg_X <- function(data, varl, varr, control = "CASEMIX", cluster = "FI", method = "ols") {
    #' @title Regression using fixest package
    #' @description This function is used to estimate the regression model using the fixest package.
    #' @param data A data.table object.
    #' @param varl A character vector of the left-hand side variables.
    #' @param varr A character vector of the right-hand side variables.
    #' @param control A character string of the control variables.
    #' @param cluster A character string of the cluster variable.
    #' @param method A character string of the method to be used. Either "ols" or "pois".
    #' @return The regression result

    if (method == "ols") {
        formula <- xpd(lhs = add_log(varl), rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
        res <- feols(formula, data)
    }

    if (method == "pois") {
        formula <- xpd(lhs = varl, rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
        res <- fepois(formula, data)
    }

    # saveRDS(res, paste0("Results/", year_start, "-", year_end, "/reg_", varl, "_", method, "_", cluster, ".rds"))
    etable(res, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = paste0("Tables/2016-2022/reg_", varl, "_", method, ".tex"), signif.code = "letters", replace = TRUE)

    return(res)
}


plot_FE <- function(reg_res, cluster, dt_status, year_start, year_end, plot_format = "pdf") {
    #' @title Plot the fixed effect classified by the legal status
    #' @description This function is used to plot the fixed effect classified by the legal status.
    #' @param reg_res The regression result.
    #' @param cluster A character string of the cluster variable.
    #' @param dt_status A data.table object of the legal status.
    #' @param year_start The starting year
    #' @param year_end The ending year
    #' @param plot_format The plot format. Default is "pdf".
    #' @return A list of plots, one for the fixed effect and one for the exponentiated fixed effect.

    FE <- fixef(reg_res)
    dt_FE <- data.table(cluster = names(FE[[cluster]]), FixedEffect = unlist(FE[[cluster]]))
    dt_FE[, Rank := rank(FixedEffect)]
    if (cluster == "FI") {
        setkey(dt_status, FI)
        dt_FE_status <- dt_FE[dt_status, on = .(cluster = FI), nomatch = 0]
    } else {
        setkey(dt_status, FI_EJ)
        dt_FE_status <- dt_FE[dt_status, on = .(cluster = FI_EJ), nomatch = 0]
    }
    reg_name <- deparse(substitute(reg_res))
    p <- ggplot(dt_FE_status, aes(x = Rank, y = FixedEffect)) +
        geom_point(aes(color = STJR_LABEL), size = 1) +
        theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))

    ggsave(paste0("Figures/", year_start, "-", year_end, "/", reg_name, "_FE_on_", cluster, ".", plot_format), p, width = 6, height = 4, dpi = 300)
    pe <- ggplot(dt_FE_status, aes(x = Rank, y = exp(FixedEffect))) +
        geom_point(aes(color = STJR_LABEL), size = 1) +
        theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
    ggsave(paste0("Figures/", year_start, "-", year_end, "/", reg_name, "_FE_on_", cluster, "_e.", plot_format), pe, width = 6, height = 4, dpi = 300)

    return(list(p, pe))
}
