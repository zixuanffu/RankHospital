rm(list = ls())
library(data.table)

# get input
input <- readRDS("Data/Out/labor_input_2016_2022.rds")
colnames(input)
input[, `:=`(AN = as.numeric(AN))]
setkey(input, AN, FI, FI_EJ)
# input_ETP<-readRDS("Data/Out/labor_input_2016_2022.rds")

# get capacity
capacity <- readRDS("Data/Out/capacity_2016_2022.rds")
colnames(capacity)
capacity[, `:=`(AN = as.numeric(AN))]
setkey(capacity, AN, FI, FI_EJ)
# get output
output <- readRDS("Data/Out/output_2016_2022.rds")
colnames(output)
output[, `:=`(AN = as.numeric(AN))]
setkey(output, AN, FI, FI_EJ)
# get dummy
dummy <- readRDS("Data/Out/dummy_2016_2022.rds")
colnames(dummy)
dummy[, `:=`(AN = as.numeric(AN))]
setkey(dummy, AN, FI, FI_EJ)

## control and status together
control_status <- readRDS("Data/Out/control_stat_2016_2022.rds")

# let's merge the dataset.
dt <- input[output, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[capacity, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[dummy, on = .(AN, FI, FI_EJ), nomatch = 0]
dt <- dt[control_status, on = .(AN, FI, FI_EJ), nomatch = 0]

colnames(dt)

dt[is.na(dt)] <- 0
dt[, EFF_MD := EFFSAL_TOT + EFFLIB_TOT]
dt[, ETP_INF := ETP_INFAVECSPE + ETP_INFSANSSPE + ETP_DIRINF]
dt[, ETP_NONMED := ETP_CAD + ETP_DIR + ETP_AUTADM]

saveRDS(dt, "Data/Out/combineddata_2016_2022.rds")
