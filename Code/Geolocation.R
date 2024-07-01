pacman::p_load(sf, ggplot2, data.table, REBayes)
source("Code/SelectX_GLVmix.R")
# library(dplyr, warn.conflicts = FALSE)
# pacman::p_load(tidygeocoder, ggmap)
dt <- read.csv("Data/In/Raw/finess_b.csv", header = FALSE, sep = ";")
setDT(dt)
colnames(dt)
dt <- dt[V1 == "geolocalisation"]
dt <- dt[, .(V2, V3, V4)]
View(dt)
setnames(dt, c("V2", "V3", "V4"), c("FI", "coorx", "coory"))

status <- readRDS("Data/Out/status_stable_2013_2022.rds")
colnames(status)
dt <- dt[FI %in% status$FI]
saveRDS(dt, "Data/Out/FI_GEO.rds")

pdt <- readRDS("Data/Out/pdt_used_gmm_fd.rds")
dt <- dt[FI %in% unique(pdt$FI)]

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
View(selection)
colnames(selection)

# Load France map
france_map <- st_read("Data/Out/georef-france-departement-millesime.shx")
# france_map <- st_transform("Data/Out/regions_2015_metropole_region.shp")
str(france_map)
# View(france_map)
st_crs(france_map) <- 4326

# Set the CRS to Lambert 93
france_map <- st_transform(france_map, 2154)

# Create a data frame for the point
point <- selection[A == 1, .(x = coorx, y = coory, STJR, FI)]
point <- st_as_sf(point, coords = c("x", "y"), crs = 2154)
points_in_france <- st_join(point, france_map, join = st_within)

dim(points_in_france)
colnames(points_in_france)
# Plot the map and the point
p <- ggplot() +
    geom_sf(data = france_map) +
    geom_sf(data = points_in_france, aes(color = STJR), size = 0.0001)
ggsave("Figures/Map.pdf", plot = p)
