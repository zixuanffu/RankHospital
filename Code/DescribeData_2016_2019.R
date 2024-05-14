rm(list = ls())
library(data.table)
library(ggplot2)
library(xtable)
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
    print(latex_table, type = "latex", file = paste0("Tables/2016-2019/", var, ".tex"))
}

results_tab(dt_SJMD, "SJMD_MCO", "SJMD MCO")
results_tab(dt_SJMD, "SJMD_TOT", "SJMD TOT")
results_tab(dt_SJINF, "SJINF_MCO", "SJINF MCO")
results_tab(dt_SJINF, "SJINF_TOT", "SJINF TOT")
results_tab(dt_SJAS, "SJAS_MCO", "SJAS MCO")
results_tab(dt_SJAS, "SJAS_TOT", "SJAS TOT")


# Section 2: The share of outpatient STAC stay out of all STAC stays.

output <- readRDS("Data/Out/output_2016_2022.rds")
# replicate (2016-2019)
output <- output[AN <= 2019 & AN >= 2016]
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
ggsave("Figures/2016-2019/STAC_share_pool.pdf", plot = p_share_pool, width = 8, height = 6, units = "in", dpi = 300)

# plot by the status of the hospitals
status <- readRDS("Data/Out/status_2016_2022.rds")
status <- status[AN <= 2019 & AN >= 2016]
status <- status[, .(AN, RS, FI, FI_EJ, STJR, STJR_LABEL, CAT, CATR)]
setkey(status, AN, FI, FI_EJ)
setkey(output, AN, FI, FI_EJ)
output_stac_status <- output_stac[status, on = c("AN", "FI", "FI_EJ"), nomatch = 0]

for (i in 0:3) {
    label <- output_stac_status[STJR == i][1, STJR_LABEL]
    p <- share_hist(output_stac_status[STJR == i], "share", title = paste0("Share of outpatient STAC stays out of all STAC stays in ", label, " hospitals"))
    ggsave(paste0("Figures/2016-2019/STAC_share_", label, ".pdf"), plot = p, width = 8, height = 6, units = "in", dpi = 300)
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
ggsave("Figures/2016-2019/STAC_share_all.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)


p <- share_hist(output_stac_status, "share") +
    facet_wrap(~STJR_LABEL, nrow = 2, ncol = 2)
ggsave("Figures/2016-2019/STAC_share_all_facet.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)

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
ggsave("Figures/2016-2019/ALOS.pdf", plot = p, width = 8, height = 6, units = "in", dpi = 300)


ALOS_sum <- output_len_status[, .(Q1 = quantile(ALOS, 0.25, na.rm = TRUE), Q2 = quantile(ALOS, 0.5, na.rm = TRUE), Q3 = quantile(ALOS, 0.75, na.rm = TRUE), Mean = mean(ALOS, na.rm = TRUE), Std_dev = sd(ALOS, na.rm = TRUE), Nobs = .N), by = .(STJR_LABEL)]
ALOS_sum_pool <- output_len_status[, .(Q1 = quantile(ALOS, 0.25, na.rm = TRUE), Q2 = quantile(ALOS, 0.5, na.rm = TRUE), Q3 = quantile(ALOS, 0.75, na.rm = TRUE), Mean = mean(ALOS, na.rm = TRUE), Std_dev = sd(ALOS, na.rm = TRUE), Nobs = .N)]
ALOS_sum_pool[, STJR_LABEL := "All"]
ALOS_sum <- rbind(ALOS_sum, ALOS_sum_pool)
ALOS_sum <- transpose(ALOS_sum, make.names = "STJR_LABEL", , keep.names = "STJR_LABEL")

latex_table_ALOS <- xtable(ALOS_sum, caption = "Average length of stay") # convert the data frame to a LaTeX table

# Print the LaTeX table
print(latex_table, type = "latex", file = "Tables/2016-2019/ALOS.tex")

# Section 4: Selection of Patients

cap <- readRDS("Data/Out/capacity_2016_2022.rds")
cap <- cap[AN <= 2019 & AN >= 2016]
cap <- cap[!is.na(LIT_MCO)]
cap$AN <- as.numeric(cap$AN)
setkey(cap, AN, FI)
setkey(status, AN, FI)

casemix <- readRDS("Data/Out/control_2016_2022.rds")
casemix <- casemix[AN <= 2019 & AN >= 2016]
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
print(latex_table_casemix, type = "latex", file = "Tables/2016-2019/casemix_bed.tex")







# draft
##  focus on doctor
# labor input
doctor <- c("EFFLIB_MEDGEN", "EFFLIB_SPEMED", "EFFLIB_SPECHI", "ETPSAL_MEDGEN", "ETPSAL_SPEMED", "ETPSAL_SPECHI", "EFFSAL_MEDGEN", "EFFSAL_SPEMED", "EFFSAL_SPECHI")
a <- readRDS("Data/Out/labor_input_2016_2022_admin.rds")
a <- a[AN <= 2019 & AN >= 2016]
for (i in doctor) {
    a[, (i) := as.numeric(get(i))]
}

# a <- a[!(is.na(EFFLIB_SPEMED) & is.na(EFFLIB_SPECHI) & is.na(EFFLIB_MEDGEN)
# & is.na(ETPSAL_SPEMED) & is.na(ETPSAL_SPECHI) & is.na(ETPSAL_MEDGEN))]
a[is.na(a)] <- 0
a[, MD := rowSums(.SD), .SDcols = c("EFFLIB_MEDGEN", "EFFLIB_SPEMED", "EFFLIB_SPECHI", "EFFSAL_MEDGEN", "EFFSAL_SPEMED", "EFFSAL_SPECHI")]
# a[,MD_N:=rowSums(.SD),.SDcols=c( "EFFLIB_SPEMED", "EFFLIB_SPECHI", "ETPSAL_SPEMED", "ETPSAL_SPECHI")]
a <- a[MD > 0]
a[, MDSAL := ETPSAL_MEDGEN + ETPSAL_SPEMED + ETPSAL_SPECHI]
a[, MDLIB := EFFLIB_MEDGEN + EFFLIB_SPEMED + EFFLIB_SPECHI]
a[, MDEFF := EFFSAL_MEDGEN + EFFSAL_SPEMED + EFFSAL_SPECHI]

setkey(a, AN, FI)

b <- readRDS("Data/Out/output_2016_2022.rds")
b <- b[AN <= 2019 & AN >= 2016]
colnames(b)
# b <- b[!(is.na(SEJHC_MCO) & is.na(SEJHP_MCO))]
b[is.na(b)] <- 0
b[, SEJ := SEJHC_MCO + SEJHP_MCO]
# +SEJHC_SSR+SEJ_HAD+SEJACC+SEJ_NEO+SEJ_REA+SEJ_REANIM] # all kinds of stays? this is a question...
b <- b[SEJ > 0]
setkey(b, AN, FI)


c <- readRDS("Data/Out/status_2016_2022.rds")
c <- c[AN <= 2019 & AN >= 2016]
setkey(c, AN, FI)

b$AN <- as.numeric(b$AN)
a$AN <- as.numeric(a$AN)
ab <- a[b, on = c("AN", "FI"), nomatch = 0]
ab$AN <- as.numeric(ab$AN)
abc <- ab[c, on = c("AN", "FI"), nomatch = 0]

abc <- abc[MD > 0 & SEJ > 0]
abc[, SJMD := SEJ / MD]
results_stat <- abc[, .(Q1 = quantile(SJMD, 0.25, na.rm = TRUE), Q2 = quantile(SJMD, 0.5, na.rm = TRUE), Q3 = quantile(SJMD, 0.75, na.rm = TRUE), Q4 = quantile(SJMD, 1, na.rm = TRUE), Mean = mean(SJMD, na.rm = TRUE)), by = .(STJR_LABEL)]
results_stat <- transpose(results_stat, make.names = "STJR_LABEL", , keep.names = "STJR_LABEL")
mean_stat <- mean(abc$SJMD, na.rm = TRUE)

# not too much difference.
results_admin <- abc[, .(Q1 = quantile(SJMD, 0.25, na.rm = TRUE), Q2 = quantile(SJMD, 0.5, na.rm = TRUE), Q3 = quantile(SJMD, 0.75, na.rm = TRUE), Q4 = quantile(SJMD, 1, na.rm = TRUE), Mean = mean(SJMD, na.rm = TRUE)), by = .(STJR_LABEL)]
results_admin <- transpose(results_admin, make.names = "STJR_LABEL", , keep.names = "STJR_LABEL")


MD_STAT <- abc[, .(SAL = mean(MDSAL[MDSAL != 0], na.rm = TRUE), LIB = mean(MDLIB[MDLIB != 0], na.rm = TRUE), EFF = mean(MDEFF[MDEFF != 0], na.rm = TRUE)), by = .(STJR_LABEL)]
MD_STAT <- abc[, .(SAL = mean(MDSAL, na.rm = TRUE), LIB = mean(MDLIB, na.rm = TRUE), EFF = mean(MDEFF, na.rm = TRUE)), by = .(STJR_LABEL)]


## focus on nurses
rm(list = ls())
a <- readRDS("Data/Out/labor_input_2016_2022.rds")
a <- a[AN <= 2019 & AN >= 2016]
inf <- c("ETP_INFAVECSPE", "ETP_INFSANSSPE")
for (i in inf) {
    a[, (i) := as.numeric(get(i))]
}
a[is.na(a)] <- 0
a[, INF := rowSums(.SD), .SDcols = inf]

b <- readRDS("Data/Out/output_2016_2022.rds")
b <- b[AN <= 2019 & AN >= 2016]
colnames(b)
b <- b[!(is.na(SEJHC_MCO) & is.na(SEJHP_MCO))]
dim(b)
b[is.na(b)] <- 0
b[, SEJ := SEJHC_MCO + SEJHP_MCO + SEJHC_SSR + ENT + SEJ_HAD
    + SEJACC + SEJ_NEO + SEJ_REA + SEJ_REANIM
    + SEJ_HAD + VEN_HDJ_TOT + VEN_HDJ_TOT + PASSU_GEN]
b[, SEJ := SEJHC_MCO + SEJHP_MCO + SEJHC_SSR + ENT + SEJ_HAD
    + PASSU_GEN]

# +SEJHC_SSR+SEJ_HAD+SEJACC+SEJ_NEO+SEJ_REA+SEJ_REANIM] # all kinds of stays? this is a question...
b <- b[SEJ > 0]
dim(b)
setkey(b, AN, FI)


c <- readRDS("Data/Out/status_2016_2022.rds")
c <- c[AN <= 2019 & AN >= 2016]
setkey(c, AN, FI)

b$AN <- as.numeric(b$AN)
a$AN <- as.numeric(a$AN)
ab <- a[b, on = c("AN", "FI"), nomatch = 0]
dim(ab)
ab <- ab[INF > 0 & SEJ > 0]
dim(ab)
ab$AN <- as.numeric(ab$AN)

abc <- ab[c, on = c("AN", "FI"), nomatch = 0]

abc <- abc[INF > 0 & SEJ > 0]
dim(abc)
abc[, SJIF := SEJ / INF]
results_stat <- abc[, .(Q1 = quantile(SJIF, 0.25, na.rm = TRUE), Q2 = quantile(SJIF, 0.5, na.rm = TRUE), Q3 = quantile(SJIF, 0.75, na.rm = TRUE), Mean = mean(SJIF, na.rm = TRUE)), by = .(STJR_LABEL)]
results_stat <- transpose(results_stat, make.names = "STJR_LABEL", , keep.names = "STJR_LABEL")
View(results_stat)
View(abc)
colnames(b)


# plot the year to year change in labor input and output
library(data.table)
library(ggplot2)
a <- readRDS("Data/Out/labor_input_EFF_2016_2022.rds")
b <- readRDS("Data/Out/output_2016_2022.rds")
c <- readRDS("Data/Out/status_2016_2022.rds")
# a<-a[AN<=2019 & AN>=2016]
a[is.na(a)] <- 0
a[, MD := EFFLIB_MEDGEN + EFFLIB_SPEMED + EFFLIB_SPECHI + EFFSAL_MEDGEN + EFFSAL_SPEMED + EFFSAL_SPECHI]
a[, IF := EFF_INFAVECSPE + EFF_INFSANSSPE]
# b<-b[AN<=2019 & AN>=2016]
b[is.na(b)] <- 0
b[, STAC := SEJHC_MCO + SEJHP_MCO]
# c<-c[AN<=2019 & AN>=2016]

a$AN <- as.numeric(a$AN)
b$AN <- as.numeric(b$AN)
setkey(a, AN, FI)
setkey(b, AN, FI)
setkey(c, AN, FI)
ab <- a[b, on = c("AN", "FI"), nomatch = 0]
abc <- ab[c, on = c("AN", "FI"), nomatch = 0]

abc <- abc[MD > 0 & IF > 0 & STAC > 0]
results <- abc[, .(
    Mean_MD = mean(MD, na.rm = TRUE), SD_MD = sd(MD, na.rm = TRUE),
    Mean_IF = mean(IF, na.rm = TRUE), SD_IF = sd(IF, na.rm = TRUE),
    Mean_STAC = mean(STAC, na.rm = TRUE), SD_STAC = sd(STAC, na.rm = TRUE)
),
by = .(FI)
]

# plot the distribution of mean_md, mean_if, mean_stac in boxplot
boxplot(results$Mean_MD, results$Mean_IF, results$Mean_STAC, names = c("Mean_MD", "Mean_IF", "Mean_STAC"))

# plot the distribution of mean_md, mean_if, mean_stac in histogram
hist(results$Mean_MD, main = "Mean_MD Distribution")
hist(results$Mean_IF, main = "Mean_IF Distribution")
hist(results$Mean_STAC, main = "Mean_STAC Distribution")

# plot the distribution of sd_md, sd_if, sd_stac in boxplot
boxplot(results$SD_MD, results$SD_IF, names = c("SD_MD", "SD_IF"))

# plot the distribution of sd_md, sd_if, sd_stac in histogram
results[SD_MD < 0.01, .N]
hist(results$SD_MD, breaks = 100, main = "SD_MD Distribution")
hist(results$SD_IF, main = "SD_IF Distribution")
hist(results$SD_STAC, main = "SD_STAC Distribution")
