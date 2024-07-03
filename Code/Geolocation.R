rm(list = ls())
pacman::p_load(sf, ggplot2, data.table, REBayes)
source("Code/SelectX_GLVmix.R")

dt <- read.csv("Data/In/Raw/finess_b.csv", header = FALSE, sep = ";")
setDT(dt)
dt1 <- dt[V1 == "structureet"]
dt2 <- dt[V1 == "geolocalisation"]
rm(dt)

dt1 <- dt1[, .(V2, V3, V4, V5, V20, V22, V26)]
dt2 <- dt2[, .(V2, V3, V4)]
setnames(dt1, new = c("FI", "FI_EJ", "NAME", "NAME_L", "TYPE1", "TYPE2", "TYPE3"))
setnames(dt2, new = c("FI", "coorx", "coory"))
dt <- merge(dt1, dt2, on = "FI")
saveRDS(dt, "Data/Out/geo.rds")

status <- readRDS("Data/Out/status_stable_2013_2022.rds")
colnames(status)

dt <- readRDS("Data/Out/geo.rds")
pdt <- readRDS("Data/Out/pdt_used_gmm_fd.rds")
status_not <- unique(status[!(FI %in% unique(dt$FI))]$FI)
pdt_not <- unique(pdt[!(FI %in% unique(dt$FI))]$FI)
dt <- dt[FI %in% unique(status$FI), ]

# ---- selection outcome ---- #
Rules <- c("TPKW", "TPKWs", "PMKW", "PMKWs", "MLE", "JS")
rule_index <- 2
Z <- fit2d(pdt)
dt2 <- Z$pdt[, .(hat_mu = first(hat_mu), Var_res1 = first(Var_res1), Var_res2 = first(Var_res2), Nobs = first(Nobs)), by = .(FI, STJR)]

alpha <- 0.2
gamma <- 0.2
tail <- "L"
s <- select2d(Z, alpha = alpha, gamma = gamma, tail = tail)
A <- s$A[, rule_index]
B <- s$B[, rule_index]

selection <- cbind(dt2, A, B)
selection <- merge(selection, dt, by = "FI")
colnames(selection)

#---- plot the selected on the map ----#

# Load France map
france_map <- st_read("Data/Out/map_fr.shp")
france_map_shx <- st_read("Data/Out/map_fr.shx")
str(france_map)
st_crs(france_map) <- 4326

# Set the CRS to Lambert 93
france_map <- st_transform(france_map, 2154)
france_map <- st_crop(france_map, xmin = -378305.81, xmax = 6005281.2, ymin = 1320649.57, ymax = 7235612.72)
# -378305.81 6005281.2
# 1320649.57 7235612.72

# Create a data frame for the point
point <- selection[A == 1, .(x = coorx, y = coory, STJR, FI)]
point <- st_as_sf(point, coords = c("x", "y"), crs = 2154)
str(point)
str(france_map)

france_box <- st_as_sfc(st_bbox(france_map))
str(france_box)

logi_point <- st_intersects(point, france_box, sparse = FALSE)
point_in_france <- point[logi_point, ]
str(point_in_france)
level_order <- c(1, 2, 3)
point_in_france$STJR <- factor(point_in_france$STJR, levels = level_order, label = c("Ordinary Public", "Private FP", "Private NP"))
setnames(point_in_france, old = "STJR", new = "Legal status")
View(point_in_france)
# Plot the map and the point
p <- ggplot() +
    geom_sf(data = france_map) +
    geom_point(data = point_in_france, aes(color = `Legal status`, geometry = geometry), size = 2, stat = "sf_coordinates")

ggsave("Figures/2013-2022/Maps/Map.pdf", plot = p, width = 8, height = 6)
