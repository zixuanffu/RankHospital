rm(list = ls())
pacman::p_load(data.table, ggplot2, xtable)
# Section 1: All stays/labor
doctor <- c("EFFSAL_TOT", "EFFLIB_TOT")
infirmier <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE", "ETP_DIRINF")
aide_soignant <- c("ETP_AID")
non_med <- c("ETP_CAD", "ETP_DIR", "ETP_AUTADM")
labor <- c(doctor, infirmier, aide_soignant, non_med)

input <- readRDS("Data/Out/labor_input_2016_2022.rds")
input[is.na(input)] <- 0
output <- readRDS("Data/Out/output_2016_2022.rds")
output[is.na(output)] <- 0

for (i in labor) {
    input[, (i) := as.numeric(get(i))]
}
input[is.na(input)] <- 0
input[, EFF_MD := EFFSAL_TOT + EFFLIB_TOT]
input[, ETP_INF := ETP_INFAVECSPE + ETP_INFSANSSPE + ETP_DIRINF]

output[, SEJ_TOT := PASSU_GEN + PASSU_PED + SEJ_HTP_TOT + VEN_HDJ_TOT + VEN_HDN_TOT + SEJHC_SSR + ENT + JOUHP_SSR + SEJ_HAD + SEANCES_MED + CONSULT_EXT + SEJHP_MCO + SEJHC_MCO]
output[, SEJ_MCO := SEJHP_MCO + SEJHC_MCO]

setkey(input, AN, FI, FI_EJ)
setkey(output, AN, FI, FI_EJ)

inout <- input[output, on = c("AN", "FI", "FI_EJ"), nomatch = 0]

status <- readRDS("Data/Out/status_2016_2022.rds")
inout_status <- inout[status, on = c("AN", "FI", "FI_EJ"), nomatch = 0]
inout_status <- inout_status[AN >= 2016 & AN <= 2019]

dt_SJMD <- inout_status[EFF_MD > 0 & SEJ_MCO > 0, .(AN, FI, FI_EJ, STJR, STJR_LABEL, SJMD_MCO = SEJ_MCO / EFF_MD, SJMD_TOT = SEJ_TOT / EFF_MD)]
dt_SJINF <- inout_status[ETP_INF > 0 & SEJ_MCO > 0, .(AN, FI, FI_EJ, STJR, STJR_LABEL, SJINF_MCO = SEJ_MCO / ETP_INF, SJINF_TOT = SEJ_TOT / ETP_INF)]
dt_SJAS <- inout_status[ETP_AID > 0 & SEJ_MCO > 0, .(AN, FI, FI_EJ, STJR, STJR_LABEL, SJAS_MCO = SEJ_MCO / ETP_AID, SJAS_TOT = SEJ_TOT / ETP_AID)]


results_tab <- function(data, var, caption) {
    results <- data[, .(Q1 = quantile(get(var), 0.25, na.rm = TRUE), Q2 = quantile(get(var), 0.5, na.rm = TRUE), Q3 = quantile(get(var), 0.75, na.rm = TRUE), Mean = mean(get(var), na.rm = TRUE), Nobs = .N), by = .(STJR_LABEL)]
    results_pool <- data[, .(Q1 = quantile(get(var), 0.25, na.rm = TRUE), Q2 = quantile(get(var), 0.5, na.rm = TRUE), Q3 = quantile(get(var), 0.75, na.rm = TRUE), Mean = mean(get(var), na.rm = TRUE), Nobs = .N)]
    results_pool[, STJR_LABEL := "All"]
    results <- rbind(results, results_pool)
    results <- transpose(results, make.names = "STJR_LABEL", , keep.names = "STJR_LABEL")
    latex_table <- xtable(results, caption = caption)
    print(latex_table, type = "latex", file = paste0("Tables/2016-2022/", var, ".tex"))
}

results_tab(dt_SJMD, "SJMD_MCO", "SJMD MCO")
results_tab(dt_SJMD, "SJMD_TOT", "SJMD TOT")
results_tab(dt_SJINF, "SJINF_MCO", "SJINF MCO")
results_tab(dt_SJINF, "SJINF_TOT", "SJINF TOT")
results_tab(dt_SJAS, "SJAS_MCO", "SJAS MCO")
results_tab(dt_SJAS, "SJAS_TOT", "SJAS TOT")


# Section 2: The share of outpatient STAC stay out of all STAC stays.

output <- readRDS("Data/Out/output_2016_2022.rds")
# replicate (2016-2022)
# cast the columns to numeric type
cols <- colnames(output)[!colnames(output) %in% c("FI", "FI_EJ")]
for (i in cols) {
    output[, (i) := as.numeric(get(i))]
}
# filter out those obs with zero STAC stays
output[is.na(output)] <- 0
output[, STAC := SEJHC_MCO + SEJHP_MCO]
output_stac <- output[STAC > 0, .(AN, FI, FI_EJ, SEJHC_MCO, SEJHP_MCO, STAC)]
output_stac[, share := SEJHP_MCO / STAC]
# plot the share of outpatient STAC stay out of all STAC stays
## define a function for generating histogras
# options(repr.plot.width =16, repr.plot.height =12)
share_hist <- function(
    data, x = x, binwidth = 0.025, fill = "#061987", color = "#e9ecef", alpha = 0.99,
    xlab = "Share of outpatient stays", ylab = "Density", title = "Share of outpatient STAC stays out of all STAC stays") {
    p <- ggplot(data = data, aes(x = get(x))) +
        geom_histogram(aes(y = ..density..), binwidth = binwidth, fill = fill, color = color, alpha = alpha) +
        geom_density(alpha = 0.33, fill = "#061987", color = "#e9ecef") +
        labs(x = xlab, y = ylab, title = title) +
        theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
    return(p)
}
p_share_pool <- share_hist(output_stac, "share")
ggsave("Figures/2016-2022/STAC_share_pool.pdf", plot = p_share_pool, width = 8, height = 6, units = "in", dpi = 300)

# plot by the status of the hospitals
status <- readRDS("Data/Out/status_2016_2022.rds")
status <- status[, .(AN, RS, FI, FI_EJ, STJR, STJR_LABEL, CAT, CATR)]
setkey(status, AN, FI, FI_EJ)
setkey(output, AN, FI, FI_EJ)
output_stac_status <- output_stac[status, on = c("AN", "FI", "FI_EJ"), nomatch = 0]

for (i in 0:3) {
    label <- output_stac_status[STJR == i][1, STJR_LABEL]
    p <- share_hist(output_stac_status[STJR == i], "share", title = paste0("Share of outpatient STAC stays out of all STAC stays in ", label, " hospitals"))
    ggsave(paste0("Figures/2016-2022/STAC_share_", label, ".pdf"), plot = p, width = 8, height = 6, units = "in", dpi = 300)
}

# we can plot everything on the same graph to compare
p <- ggplot(data = output_stac_status, aes(x = share, fill = STJR_LABEL)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.025, alpha = 0.66, color = "#e9ecef") +
    geom_density(alpha = 0.22) +
    labs(
        x = "Share of outpatient stays", y = "Density",
        title = "Share of outpatient STAC stays out of all STAC stays", fill = "Status"
    ) +
    scale_color_brewer(palette = "Set2") +
    theme(
        text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), legend.justification = c(1, 1), legend.background = element_rect(fill = "transparent")
    )
ggsave("Figures/2016-2022/STAC_share_all.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)


p <- share_hist(output_stac_status, "share") +
    facet_wrap(~STJR_LABEL, nrow = 2, ncol = 2)
ggsave("Figures/2016-2022/STAC_share_all_facet.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

# Section 3: Average length of stay

output_len <- output[SEJHC_MCO > 0, .(AN, FI, FI_EJ, SEJHC_MCO, JOU_MCO)]
output_len[, ALOS := JOU_MCO / SEJHC_MCO]
output_len_status <- output_len[status, on = c("AN", "FI", "FI_EJ"), nomatch = 0]

p <- ggplot(data = output_len_status, aes(x = STJR_LABEL, y = ALOS)) +
    geom_boxplot(fill = "#061987", color = "#061987", alpha = 0.66) +
    ylim(0, 20) +
    coord_flip() +
    labs(x = "Status", y = "Average Length of Stay", title = "Average Length of Stay by Hospital legal status") +
    theme(text = element_text(family = "Times"), plot.title = element_text(hjust = 0.5))
ggsave("Figures/2016-2022/ALOS.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)


ALOS_sum <- output_len_status[, .(Q1 = quantile(ALOS, 0.25, na.rm = TRUE), Q2 = quantile(ALOS, 0.5, na.rm = TRUE), Q3 = quantile(ALOS, 0.75, na.rm = TRUE), Mean = mean(ALOS, na.rm = TRUE), Std_dev = sd(ALOS, na.rm = TRUE), Nobs = .N), by = .(STJR_LABEL)]
ALOS_sum_pool <- output_len_status[, .(Q1 = quantile(ALOS, 0.25, na.rm = TRUE), Q2 = quantile(ALOS, 0.5, na.rm = TRUE), Q3 = quantile(ALOS, 0.75, na.rm = TRUE), Mean = mean(ALOS, na.rm = TRUE), Std_dev = sd(ALOS, na.rm = TRUE), Nobs = .N)]
ALOS_sum_pool[, STJR_LABEL := "All"]
ALOS_sum <- rbind(ALOS_sum, ALOS_sum_pool)
ALOS_sum <- transpose(ALOS_sum, make.names = "STJR_LABEL", , keep.names = "STJR_LABEL")

latex_table_ALOS <- xtable(ALOS_sum, caption = "Average length of stay") # convert the data frame to a LaTeX table

# Print the LaTeX table
print(latex_table, type = "latex", file = "Tables/2016-2022/ALOS.tex")

# Section 4: Selection of Patients

cap <- readRDS("Data/Out/capacity_2016_2022.rds")
cap <- cap[!is.na(LIT_MCO)]
cap$AN <- as.numeric(cap$AN)
setkey(cap, AN, FI)
setkey(status, AN, FI)

casemix <- readRDS("Data/Out/control_2016_2022.rds")
status <- readRDS("Data/Out/status_2016_2022.rds")
casemix[, CASEMIX := as.numeric(gsub(",", ".", CASEMIX))]
casemix_stat_23 <- status[STJR == 2 | STJR == 3][casemix, on = .(AN, FI), nomatch = 0]
casemix_stat_01 <- status[STJR == 0 | STJR == 1][casemix, on = .(AN, FI_EJ = FI), nomatch = 0]
casemix_stat <- rbind(casemix_stat_01, casemix_stat_23)
colnames(casemix_stat)


casemix_cap_status <- casemix_stat[cap, on = .(AN, FI), nomatch = 0]
casemix_cap_status$size <- cut(casemix_cap_status$LIT_MCO, breaks = c(-Inf, 28, 76, 174, Inf), labels = c(1, 2, 3, 4))

casemix_cap_status <- casemix_cap_status[CASEMIX > 0]
casemix_mean <- casemix_cap_status[, .(
    Mean_Casemix = mean(CASEMIX, na.rm = TRUE)
), by = .(STJR_LABEL, size)]
casemix_mean <- dcast(casemix_mean, size ~ STJR_LABEL, value.var = "Mean_Casemix")

latex_table_casemix <- xtable(casemix_mean, caption = "Average casemix index by hospital legal status and size of hospital") # convert the data frame to a LaTeX table
print(latex_table_casemix, type = "latex", file = "Tables/2016-2022/casemix_bed.tex")

# Section 5: The within and between variation of input and output
library(data.table)
input <- readRDS("Data/Out/labor_input_2016_2022.rds")
input[is.na(input)] <- 0
input <- input[AN != 2020]
output <- readRDS("Data/Out/output_2016_2022.rds")
output[is.na(output)] <- 0
output <- output[AN != 2020]

input[, EFF_MD := EFFSAL_TOT + EFFLIB_TOT]
input[, ETP_INF := ETP_INFAVECSPE + ETP_INFSANSSPE + ETP_DIRINF]
input[is.na(input)] <- 0

within_between <- function(data, var) {
    a <- data[, .(mean_A = mean(get(var)), var_A = var(get(var)), .N), by = (FI)]
    Mean <- mean(data[, get(var)], na.rm = TRUE)
    b <- a[, .(Bvar_A = sum((mean_A - Mean)^2 * N, na.rm = TRUE) / sum(N))]
    w <- a[, .(Wvar_A = sum(N * var_A, na.rm = TRUE) / sum(N))]
    d <- data[, .(Tvar_A = var(get(var), na.rm = TRUE))]
    e <- cbind(b, w, d)
    e[, type := var]
    return(e)
}
dt <- data.table()
for (i in c("EFF_MD", "ETP_INF", "ETP_AID")) {
    dt <- rbind(dt, within_between(input, i))
}
dt[, sum := Bvar_A + Wvar_A]
View(dt)

tex <- xtable(e, caption = "Within and between variation of input")
print(tex, type = "latex", file = "Tables/2016-2022/within_between_input.tex")

output[, SEJ_TOT := PASSU_GEN + PASSU_PED + SEJ_HTP_TOT + VEN_HDJ_TOT + VEN_HDN_TOT + SEJHC_SSR + ENT + JOUHP_SSR + SEJ_HAD + SEANCES_MED + CONSULT_EXT + SEJHP_MCO + SEJHC_MCO]
output[, SEJ_MCO := SEJHP_MCO + SEJHC_MCO]
