rm(list = ls())
pacman:p_load(data.table, ggplot2)
dt_all <- readRDS("Data/Out/dt_stable.rds")

# Inpatient stay: plot the quantiles of the nu,ber of inpatient stays over the ANs

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
        scale_color_manual(values = colors) +
        theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Times"))

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
