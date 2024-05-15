library(fixest)

varr1 <- c("SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED")


add_log <- function(var_list) {
    len <- length(var_list)
    log_left <- rep("log(", length(len))
    log_right <- rep(")", length(len))
    log_var_list <- paste0(log_left, var_list, log_right)
    return(log_var_list)
}

reg_X <- function(data, varl, varr, control = "CASEMIX", cluster = "FI", method = "ols") {
    if (method == "ols") {
        formula <- xpd(lhs = add_log(varl), rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
        res <- feols(formula, data)
    }

    if (method == "pois") {
        formula <- xpd(lhs = varl, rhs = add_log(varr), add = paste(control, cluster, sep = "|"))
        res <- fepois(formula, data)
    }
    etable(res, sdBelow = TRUE, digits = 3, fitstat = ~ n + sq.cor + pr2, digits.stats = 3, tex = TRUE, file = paste0("Tables/2016-2022/reg_", varl, "_", method, ".tex"), signif.code = "letters", replace = TRUE)
    return(res)
}
reg_X(pdt, "ETP_INF", varr1, method = "pois")

plot_FE <- function(reg_res, cluster, dt_status, plot_format = "pdf") {
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

    ggsave(paste0("Figures/2016-2022/", reg_name, "_FE_on_", cluster, ".", plot_format), p, width = 6, height = 4, dpi = 300)
    pe <- ggplot(dt_FE_status, aes(x = Rank, y = exp(FixedEffect))) +
        geom_point(aes(color = STJR_LABEL), size = 1) +
        theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
    ggsave(paste0("Figures/2016-2022/", reg_name, "_FE_on_", cluster, "_e.", plot_format), pe, width = 6, height = 4, dpi = 300)
}

# test the functions
status <- readRDS("Data/Out/status_stable_2016_2022.rds")
# infirmier
reg_inf_ols_FI <- reg_X(pdt, "ETP_INF", varr1_less)
plot_FE(reg_inf_ols_FI, "FI", status)
# medical doctors
reg_md_olf_FI <- reg_X(pdt, "EFF_MD", varr1_less)
plot_FE(reg_md_olf_FI, "FI", status)
