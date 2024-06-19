# ---- Plot in Section 2 ---- #
rm(list = ls())
pacman::p_load(data.table, ggplot2, xtable)


# ---- Set the color for each legal status ---- #
color_value <- c("red", "orange", "blue", "darkgreen")
legal <- c("Teaching", "Normal Public", "Private For Profit", "Private Non Profit")
# ---- 1. The number of hospitals each year, by legal status ---- #
dt_all <- readRDS("Data/Out/dt_all.rds")
dt_all <- unique(dt_all, by = c("AN", "FI")) # no duplicates
dt_all$STJR <- factor(dt_all$STJR,
    levels = c(0, 1, 2, 3),
    labels = c("Teaching", "Normal Public", "Private For Profit", "Private Non Profit")
)

count_hpt <- dt_all[, .N, by = .(AN, STJR)]
count_hpt_wide <- dcast(count_hpt, AN ~ STJR, value.var = "N")
count_hpt_wide[, Total := rowSums(.SD), .SDcols = legal]
print(xtable(count_hpt_wide), type = "latex", file = "Tables/Descriptive/hospital_count.tex", floating = FALSE, latex.environments = NULL, booktabs = TRUE)
mean_count_hpt <- count_hpt[, round(mean(N)), by = STJR]


p <- ggplot(mean_count_hpt, aes(x = STJR, y = round(V1, digits = 0))) +
    geom_col() +
    geom_text(aes(label = V1), vjust = -0.5) +
    labs(x = "Legal status", y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 20, hjust = 0.5), text = element_text(size = 20, family = "Times"))
if (!dir.exists("Figures/Descriptive")) dir.create("Figures/Descriptive")
ggsave("Figures/Descriptive/hospital_count.pdf", p, width = 8, height = 4.5)

p <- ggplot(count_hpt, aes(x = as.integer(AN), y = N, color = STJR)) +
    geom_line() +
    scale_color_manual(values = color_value) +
    labs(x = "Year", y = "Count") +
    scale_x_continuous(breaks = 2013:2022) +
    theme_minimal() +
    theme(plot.title = element_text(size = 20, hjust = 0.5), text = element_text(size = 20, family = "Times"))
print(p)
ggsave("Figures/Descriptive/hospital_count_trend.pdf", p, width = 8, height = 4.5)

# ---- 2. The share of each output, by legal status---- #
dt_all <- readRDS("Data/Out/dt_all.rds")
dt_all[, SEJ_MCO := SEJHC_MCO + SEJHP_MCO]
dt_all[, SEJ_PSY := VEN_TOT + SEJ_HTP_TOT]
output <- c(
    "SEJHC_MCO", "SEJHP_MCO", "SEANCES_MED", "CONSULT_EXT", "PASSU", "ENTSSR", "SEJ_HAD",
    "SEJ_PSY"
)
dt_sum <- dt_all[, lapply(.SD, function(x) sum(x)), by = STJR, .SDcols = output]
dt_share <- dt_sum[, lapply(.SD, function(x) x / rowSums(.SD)), by = STJR, .SDcols = output]

new_names <- c("Legal Status", "STAC inpatient", "STAC oupatient", "Sessions", "Outpatient Consultations", "Emergency", "Follow-up care and Long-term care", "Home hospitalization", "Psychiatry stays")
setnames(dt_share, colnames(dt_share), new_names)
dt_share <- transpose(dt_share, keep.names = "Output", make.names = "Legal Status")
dt_share[, (legal) := lapply(.SD, function(x) paste0(round(x * 100, 2), "%")), .SDcols = legal]

dir.create("Tables/Descriptive/")
print(xtable(dt_share), type = "latex", file = "Tables/Descriptive/output_share.tex", floating = FALSE, latex.environments = NULL, booktabs = TRUE)

# ---- 3. The 3 quantiles of the number of stays/personnel (2013-2022) ---- #


plot_trend <- function(var, var_name) {
    dt <- copy(dt_all)
    dt <- dt[get(var) > 0]
    dt_q <- dt[, .(
        q25 = log(quantile(get(var), 0.25)),
        q50 = log(quantile(get(var), 0.50)),
        q75 = log(quantile(get(var), 0.75)),
        mean = log(mean(get(var)))
    ), by = AN]
    colors <- c("25th" = "blue", "50th" = "black", "75th" = "red", "Mean" = "grey")
    p <- ggplot(dt_q, aes(x = AN)) +
        geom_line(aes(y = q25, color = "25th")) +
        geom_line(aes(y = q50, color = "50th")) +
        geom_line(aes(y = q75, color = "75th")) +
        geom_line(aes(y = mean, color = "Mean")) +
        labs(
            y = paste0("Number of ", var_name, " (in log)"), x = "Year",
            title = "Trend from 2013 to 2022", color = "Quantile"
        ) +
        scale_x_continuous(breaks = 2013:2022) +
        scale_color_manual(values = colors) +
        theme(plot.title = element_text(size = 20, hjust = 0.5), text = element_text(size = 20, family = "Times"))

    return(p)
}

# Plot quantiles
dir.create("Figures/Descriptive", showWarnings = TRUE)
pdf("Figures/Descriptive/inpatient.pdf", width = 10, height = 6)
p <- plot_trend("SEJHC_MCO", "inpatient stays")
p
dev.off()

pdf("Figures/Descriptive/outpatient.pdf", width = 10, height = 6)
p <- plot_trend("SEJHP_MCO", "outpatient stays")
p
dev.off()

pdf("Figures/Descriptive/nurses.pdf", width = 10, height = 6)
p <- plot_trend("ETP_INF", "nurses")
p
dev.off()

pdf("Figures/Descriptive/doctors.pdf", width = 10, height = 6)
p <- plot_trend("EFF_MD", "doctors")
p
dev.off()

pdf("Figures/Descriptive/psy.pdf", width = 10, height = 6)
p <- plot_trend("SEJ_PSY", "psychiatric sessions")
p
dev.off()
