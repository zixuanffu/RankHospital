# Look at FI separately
a <- readRDS("Data/Out/labor_input_2016_2022.rds")
dim(a)
a <- a[AN <= 2019 & AN >= 2016]
a[, .N, by = AN]

a[is.null(a)] <- 0
a[, names(a) := lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
labor <- colnames(a)[!colnames(a) %in% c("FI", "FI_EJ", "AN")]
for (i in labor) {
    a[, (i) := as.numeric(get(i))]
}
a[is.na(a)] <- 0
a[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = labor]
a <- a[rowsum != 0]
# doctor <- c("ETPSAL_MEDGEN", "ETPSAL_SPEMED", "ETPSAL_SPECHI")
# STAC <- "SEJHC_MCO"
a[, MD := ETPSAL_MEDGEN + ETPSAL_SPEMED + ETPSAL_SPECHI]
# a <- a[MD > 0]
a[, IF := ETP_INFAVECSPE + ETP_INFSANSSPE]
a_md <- copy(a)

# Assuming dt is your data.table and 'year' and 'FI' are the columns
unique_years <- unique(a$AN)

# Get the unique 'FI' for each year
list_of_FI_sets <- lapply(unique_years, function(y) unique(a[AN == y, FI]))

# Find the intersection
intersection_of_FI <- Reduce(intersect, list_of_FI_sets)

a_md <- a_md[FI %in% intersection_of_FI]

b <- readRDS("Data/Out/output_2016_2022.rds")
b <- b[AN <= 2019 & AN >= 2016]
cols <- colnames(b)[!colnames(b) %in% c("FI", "FI_EJ", "AN")]
for (i in cols) {
    b[, (i) := as.numeric(get(i))]
}
b[is.na(b)] <- 0
b[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
b <- b[rowsum != 0]
b[SEJHC_MCO + SEJHP_MCO >= 0, .N]
b_stac <- b[SEJHC_MCO + SEJHP_MCO >= 0]

status <- readRDS("Data/Out/status_2016_2022.rds")
status <- status[AN >= 2016 & AN <= 2019]
status <- status[, .(AN, RS, FI, FI_EJ, STJR, STJR_LABEL, CAT, CATR, NAT)]
# status <- status[NAT == 0]
status <- unique(status)

### Describe outpatient stay share
a_md$AN <- as.numeric(a_md$AN)
b_stac$AN <- as.numeric(b_stac$AN)
ab <- a_md[b_stac, on = .(AN, FI), nomatch = 0]
ab[, share := SEJHP_MCO / (SEJHC_MCO + SEJHP_MCO)]

### Describe the length of stay
ab[, LEN := JOU_MCO / SEJHC_MCO]

library(ggplot2)
p_all <- ggplot(ab, aes(x = share)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.025, fill = "#061987", color = "#e9ecef", alpha = 0.9) +
    theme_minimal() +
    labs(x = "Share", y = "Frequency", title = "Histogram of Share")
ggsave("Figure/STAC_share_all.png", plot = p_all, width = 16, height = 12)

abc_status[, STJR_LABEL := factor(STJR_LABEL, levels = c("CHU", "PNL", "PLU", "PUB"))]
p <- ggplot(ab_status, aes(x = share)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.025, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
    geom_density(alpha = .2, fill = "#69b3a2") +
    facet_wrap(~STJR_LABEL, nrow = 2, ncol = 2) +
    labs(x = "Share", y = "Density", title = "Density Histogram of Share Grouped by STJR_LABEL")
p
# Save the plot to a file and specify its size
ggsave("Figure/STAC_share.png", plot = p, width = 16, height = 12)


# ab <- ab[, SJMD := (SEJHC_MCO + SEJHP_MCO) / MD]

ab <- ab[, `:=`(SJIF = (SEJHC_MCO + SEJHP_MCO) / IF, SJMD = (SEJHC_MCO + SEJHP_MCO) / MD)]
ab[is.na(ab)] <- 0
ab[is.infinite(ab)] <- 0
ab[, c("SJIF", "SJMD") := lapply(.SD, function(x) {
    x[!is.finite(x)] <- 0
    x
}), .SDcols = c("SJIF", "SJMD")]
ab_status <- ab[status, on = .(AN, FI), nomatch = 0]
ab_status <- ab_status[SJMD > 0 & SJMD < 2000]
# ab_status <-ab_status[CAT != 128 & CAT != 365,]
ab_status[, .N, by = .(STJR_LABEL, STJR)]
ab_status <- ab_status[, .(AN, RS, STJR, STJR_LABEL, FI, FI_EJ, CAT, CATR, MD, IF, SEJHC_MCO, SEJHP_MCO, JOU_MCO, SJIF, SJMD, LEN)]








# Only look at FI_EJ
a <- readRDS("Data/Out/labor_input_2016_2022.rds")
dim(a)
a[, .N, by = AN]
colnames(a)
unique(a$AN)
a[, FI := NULL]
a[is.na(a)] <- 0
labor <- colnames(a)[!colnames(a) %in% c("FI_EJ", "AN")]
for (i in labor) {
    a[, (i) := as.numeric(get(i))]
}
a_summed <- a[, lapply(.SD, sum, na.rm = TRUE), by = .(AN, FI_EJ)]
dim(a_summed[AN <= 2019 & AN >= 2016])
a_summed <- a_summed[AN <= 2019 & AN >= 2016]
a_summed[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = labor]
a_summed <- a_summed[rowsum != 0]

b <- readRDS("Data/Out/output_2016_2022.rds")
b <- b[AN <= 2019 & AN >= 2016]
b[, FI := NULL]
cols <- colnames(b)[!colnames(b) %in% c("FI_EJ", "AN")]
for (i in cols) {
    b[, (i) := as.numeric(get(i))]
}
b_summed <- b[, lapply(.SD, sum, na.rm = TRUE), by = .(AN, FI_EJ)]
b_summed[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
b_summed <- b_summed[rowsum != 0]

c <- readRDS("Data/Out/capacity_2016_2022.rds")
c <- c[AN <= 2019 & AN >= 2016]
c[, FI := NULL]
cols <- colnames(c)[!colnames(c) %in% c("FI_EJ", "AN")]
for (i in cols) {
    c[, (i) := as.numeric(get(i))]
}
c_summed <- c[, lapply(.SD, sum, na.rm = TRUE), by = .(AN, FI_EJ)]
c_summed[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
# c_summed <- c_summed[rowsum != 0]

# merge the data
ab <- a_summed[b_summed, on = .(AN, FI_EJ), nomatch = 0]
abc <- ab[c_summed, on = .(AN, FI_EJ), nomatch = 0]

doctor <- c("ETPSAL_MEDGEN", "ETPSAL_SPEMED", "ETPSAL_SPECHI")
STAC <- "SEJHC_MCO"
abc[, `:=`(MD = ETPSAL_MEDGEN + ETPSAL_SPEMED + ETPSAL_SPECHI, STAC = SEJHC_MCO)]
abc[, IF := ETP_INFAVECSPE + ETP_INFSANSSPE]
abc <- abc[MD >= 1 & STAC > 0]
abc <- abc[, .(AN, FI_EJ, MD, IF, STAC, LIT_MCO)]
abc[, SJMD := STAC / MD]
abc[, SJIF := STAC / IF]
abc$AN <- as.numeric(abc$AN)

status <- readRDS("Data/Out/status_2016_2022.rds")
status <- status[, .(AN, FI_EJ, STJR, STJR_LABEL, CAT, CATR)]
status <- unique(status)
status2019 <- status[AN == 2019 & STJR == 2]
status2019[, .N, by = .(CAT, CATR)]
setkey(abc, AN, FI_EJ)
setkey(status, AN, FI_EJ)
abc_status <- abc[status, on = .(AN, FI_EJ), nomatch = 0]
abc_status[, .N, by = .(STJR_LABEL, STJR)]


# Describe stay per MD OR IF
# Calculate the 4 quantiles and the mean of 'SJMD' grouped by 'STJR'
results <- ab_status[, .(
    Q1 = quantile(SJMD, 0.25, na.rm = TRUE),
    Q2 = quantile(SJMD, 0.5, na.rm = TRUE),
    Q3 = quantile(SJMD, 0.75, na.rm = TRUE),
    Q4 = quantile(SJMD, 1, na.rm = TRUE),
    Mean = mean(SJMD, na.rm = TRUE)
),
by = .(STJR_LABEL)
]

ab_status <- ab_status[LEN > 0]
results_len <- ab_status[, .(
    Q1 = quantile(LEN, 0.25, na.rm = TRUE),
    Q2 = quantile(LEN, 0.5, na.rm = TRUE),
    Q3 = quantile(LEN, 0.75, na.rm = TRUE),
    Q4 = quantile(LEN, 1, na.rm = TRUE),
    Mean = mean(LEN, na.rm = TRUE)
),
by = .(STJR_LABEL)
]

boxplot(LEN ~ STJR_LABEL, data = ab_status, main = "Length of Stay by STJR_LABEL", xlab = "STJR_LABEL", ylab = "Length of Stay", horizontal = TRUE, ylim = c(0, 20))

library(ggplot2)
ggplot(data = ab_status, aes(x = STJR_LABEL, y = LEN)) +
    geom_boxplot(fill = "#69b3a2") +
    coord_flip() +
    labs(title = "Length of Stay by STJR_LABEL", x = "STJR_LABEL", y = "Length of Stay") +
    ylim(0, 20)

# look at the strange places...this is too weird i have to say.
sygen_2019 <- readRDS("Data/In/SYGEN/SYGEN_2019.rds")

r <- sygen_2019[FI_EJ == 670000199]
cols <- colnames(r)[grepl("ETP", colnames(r))]
cols <- c(cols, "FI_EJ")
View(r[, ..cols])
r <- r[, ..cols]
View(melt(r))

etp <- colnames(sygen_2019)[grepl("ETP", colnames(sygen_2019))]
cols <- c("AN", "FI", "FI_EJ", etp)
sygen_2019 <- sygen_2019[, ..cols]
for (i in etp) {
    sygen_2019[, (i) := as.numeric(get(i))]
}

sygen_2019[is.na(sygen_2019)] <- 0
sygen_2019[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = etp]
sygen_2019 <- sygen_2019[rowsum > 1]
library(readxl)
md <- read_excel("Data/Others/input.xlsx", col_names = "MD")$MD
md <- c(
    "ETPSAL_SPEMED", "ETPSAL_DT_MEDGEN", "ETPSAL_DT_URG", "ETPSAL_DT_ANESTREA",
    "ETPSAL_SPECHI", "ETPSAL_DT_GYNOBS", "ETPSAL_PSY", "ETPSAL_ODON",
    "ETPSAL_PHARMA", "ETPSAL_AUT", "ETPSAL_TOT"
)

md <- c(
    "ETPSAL_SPEMED",
    "ETPSAL_SPECHI", "ETPSAL_PSY", "ETPSAL_ODON",
    "ETPSAL_PHARMA", "ETPSAL_AUT", "ETPSAL_TOT"
)

md_2019 <- sygen_2019[, ..md]
md_2019[, rowsum := rowSums(.SD, na.rm = TRUE), .SDcols = md[md != "ETPSAL_TOT"]]
md_2019 <- md_2019[rowsum > 1]



### Describe the avearage length of stay


### Casemix
casemix <- readRDS("Data/Out/control_2017_2022.rds")
casemix <- casemix[AN <= 2019 & AN >= 2017]
colnames(casemix)
setkey(casemix, AN, FI_EJ)
setnames(casemix, "FI", "FI_EJ")
cap <- readRDS("Data/Out/capacity_2016_2022.rds")
cap <- cap[AN <= 2019 & AN >= 2017]
colnames(cap)
setkey(cap, AN, FI)
casemix$AN <- as.numeric(casemix$AN)
cap$AN <- as.numeric(cap$AN)
setkey(status, AN, FI_EJ)
colnames(status)

casemix_status <- casemix[status[, .(AN, FI, FI_EJ, RS, STJR, STJR_LABEL)], on = .(AN, FI_EJ), nomatch = 0]
# casemix[,lapply(.SD,function(x) gsub(",", ".", x)),.SDcols = c("CASEMIX")]

casemix_cap <- casemix[cap, on = .(AN, FI), nomatch = 0]

casemix_cap_status <- casemix_cap[status[, .(AN, FI, RS, STJR, STJR_LABEL)], on = .(AN, FI_EJ), nomatch = 0]
casemix_cap_status <- casemix_cap_status[CASEMIX > 0]
casemix_cap_status$size <- cut(casemix_cap_status$LIT_MCO, breaks = c(-Inf, 28, 76, 174, Inf), labels = c(1, 2, 3, 4))


casemix_mean <- casemix_cap_status[, .(
    Mean_Casemix = mean(CASEMIX, na.rm = TRUE)
), by = .(STJR_LABEL, size)]

dcast(casemix_mean, STJR_LABEL ~ size, value.var = "Mean_Casemix")


#
casemix <- readRDS("Data/Out/control_2017_2022.rds")
casemix <- casemix[AN <= 2019 & AN >= 2017]
casemix[, CASEMIX := as.numeric(gsub(",", ".", CASEMIX))]
casemix_stat_23 <- status[STJR == 2 | STJR == 3][casemix, on = .(AN, FI), nomatch = 0]
casemix_stat_01 <- status[STJR == 0 | STJR == 1][casemix, on = .(AN, FI_EJ = FI), nomatch = 0]
casemix_stat <- rbind(casemix_stat_01, casemix_stat_23)
casemix_stat <- unique(casemix_stat, by = c("AN", "i.RS"))

# setnames(casemix, "FI", "FI_EJ")
# setkey(casemix, AN, FI_EJ)
# setkey(status, AN, FI_EJ)
# casemix_status<-casemix[status[,.(AN,FI,FI_EJ,RS,STJR,STJR_LABEL)], on = .(AN, FI_EJ), nomatch = 0]
casemix_cap_status <- casemix_stat[cap, on = .(AN, FI), nomatch = 0]
# casemix_cap_status<-casemix_cap_status[CASEMIX>0]
casemix_cap_status$size <- cut(casemix_cap_status$LIT_MCO, breaks = c(-Inf, 28, 76, 174, Inf), labels = c(1, 2, 3, 4))
# casemix_cap_status[STJR==1]

casemix_cap_status <- casemix_cap_status[CASEMIX > 0]
casemix_mean <- casemix_cap_status[, .(
    Mean_Casemix = mean(CASEMIX)
), by = .(STJR, size)]
dcast(casemix_mean, STJR ~ size, value.var = "Mean_Casemix")
View(status[STJR == 2, .(FI, FI_EJ, RS, STJR, STJR_LABEL)])
View(status[STJR == 3, .(FI, FI_EJ, RS, STJR, STJR_LABEL)])
View(casemix)
# good good successfully reproduced very similar result!!!


# sejours
library(data.table)
sej <- readRDS("Data/Out/output_2016_2022.rds")
sej <- sej[AN <= 2019 & AN >= 2016]
sej <- sej[!(is.na(SEJHC_MCO) & is.na(SEJHP_MCO))]
sej[, SEJ := SEJHC_MCO + SEJHP_MCO]

md <- readRDS("Data/Out/labor_input_2016_2022.rds")
md <- md[AN <= 2019 & AN >= 2016]
for (i in c("ETPSAL_MEDGEN", "ETPSAL_SPEMED", "ETPSAL_SPECHI")) {
    md[, (i) := as.numeric(as.numeric(trimws(get(i))))]
}
md <- md[!is.na(ETPSAL_TOT)]
md <- md[!(is.na(ETPSAL_SPEMED) & is.na(ETPSAL_SPECHI) & is.na(ETPSAL_MEDGEN))]
md[is.na(md)] <- 0
md[, MD := ETPSAL_SPEMED + ETPSAL_MEDGEN + ETPSAL_MEDGEN]
md <- md[MD > 0]

test <- md[3]$ETPSAL_SPECHI

setkey(sej, AN, FI)
setkey(md, AN, FI)
sej_md <- sej[md, on = .(AN, FI), nomatch = 0]
test <- sej_md[, .(SEJ, MD)]
status <- readRDS("Data/Out/status_2016_2022.rds")
status <- status[AN >= 2016 & AN <= 2019]
setkey(status, AN, FI)
sej_md$AN <- as.numeric(sej_md$AN)
sej_md_status <- sej_md[status, on = .(AN, FI, FI_EJ), nomatch = 0]
sej_md_status[, SJMD := SEJ / MD]
results <- sej_md_status[, .(Q1 = quantile(SJMD, 0.25, na.rm = TRUE), Q2 = quantile(SJMD, 0.5, na.rm = TRUE), Q3 = quantile(SJMD, 0.75, na.rm = TRUE), Q4 = quantile(SJMD, 1, na.rm = TRUE), Mean = mean(SJMD, na.rm = TRUE)), by = .(STJR_LABEL)]
sej_md_status[, .N, by = .(STJR_LABEL)]
PLU <- sej_md_status[STJR_LABEL == "PLU", .(SJMD)]




sygen_2019 <- readRDS("Data/In/SYGEN/SYGEN_2016.rds")
etp <- colnames(sygen_2019)[grepl("ETP", colnames(sygen_2019))]
sygen_2019 <- sygen_2019[, ..etp]
sygen_2019 <- sygen_2019[!is.na(ETPSAL_TOT)]
for (i in etp) {
    sygen_2019[, (i) := as.numeric(as.numeric(trimws(get(i))))]
}
sygen_2019[is.na(sygen_2019)] <- 0
sygen_2019[ETPSAL_SPEMED - ETPSAL_MEDGEN < 0]


sygen_2019 <- readRDS("Data/In/SYGEN/SYGEN_2019.rds")
efflib <- colnames(sygen_2019)[grepl("EFFLIB", colnames(sygen_2019))]
colnames(sygen_2019)
efflib
etp <- colnames(sygen_2019)[grepl("EFFSAL", colnames(sygen_2019))]
col <- c(etp, efflib)
sygen_2019 <- sygen_2019[, ..col]
sygen_2019 <- sygen_2019[, .(EFFSAL_TOT, EFFLIB_TOT, ETPSAL_TOT)]
sygen_2019 <- sygen_2019[!(is.na(EFFLIB_TOT) & is.na(ETPSAL_TOT) & is.na(EFFSAL_TOT))]
sygen_2019[is.na(sygen_2019)] <- 0

library(texreg)
# install.packages("texreg")
etable()