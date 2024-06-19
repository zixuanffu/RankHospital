pacman::p_load(data.table, archive, readxl)

# ---- Collect the urls of the data files
# read from Data/In/708_bases-statistiques-sae.csv

links <- fread("Data/In/Raw/708_bases-statistiques-sae.csv")
colnames(links)
colnames(links) <- c("year", "type", "url")
unique(links$type)

# links <- links[type %in% c("Formats SAS", "Formats SAS-CSV"), ]
links <- links[type == "Formats SAS" | type == "Formats SAS-CSV", ] # before 2016 there's no csv format

links <- links[order(year), ]

# from 2017 onwards, the url are broken
for (i in 2008:2012) {
    links[year == i]$url <- paste0("https://data.drees.solidarites-sante.gouv.fr/api/v2/catalog/datasets/708_bases-statistiques-sae/attachments/sae_", i, "_base_statistique_format_sas_7z")
}
for (i in 2013:2022) {
    links[year == i]$url <- paste0("https://data.drees.solidarites-sante.gouv.fr/api/v2/catalog/datasets/708_bases-statistiques-sae/attachments/sae_", i, "_bases_statistiques_formats_sas_csv_7z")
}

# ---- Download the data files from the urls stored in the links data frame
for (i in 2008:2016) {
    url <- links[year == i]$url
    filename <- paste0("Data/In/Raw/sae_stat_", i, ".7z")
    print(filename)
    tryCatch(download.file(url, filename, mode = "wb"),
        error = function(e) {
            print(paste("Error downloading", url))
        }
    )
}

# ---- SAE: Select the sheets of interests and save them as Rds ---- #
# SYGEN.csv contains a synthetic overview of the hospital
for (i in 2016:2022) {
    zipfilename <- paste0("Data/In/Raw/sae_stat_", i, ".7z")
    zipfiletable <- setDT(archive(zipfilename))
    row_number <- (grep("SYGEN", zipfiletable$path))
    x <- setDT(read.csv2(archive_read(zipfilename, row_number, mode = "r")))
    x[, `:=`(AN = as.numeric(AN), FI = as.character(FI), FI_EJ = as.character(FI_EJ))]
    # Save to Rdata
    saveRDS(x, file = paste0("Data/In/SYGEN/SYGEN_", i, ".rds"))
}

# BRULES, PSY, BLOCS.csv contains information to construct the dummies --- #
if (!dir.exists("Data/In/BRULES")) {
    dir.create("Data/In/BRULES", showWarnings = TRUE)
}
if (!dir.exists("Data/In/PSY")) {
    dir.create("Data/In/PSY", showWarnings = TRUE)
}
if (!dir.exists("Data/In/BLOCS")) {
    dir.create("Data/In/BLOCS", showWarnings = TRUE)
}

for (i in 2016:2022) {
    zipfilename <- paste0("Data/In/Raw/sae_stat_", i, ".7z")
    zipfiletable <- setDT(archive(zipfilename))
    row_number_b <- (grep(paste0("BRULES_", i), zipfiletable$path))
    row_number_p <- (grep(paste0("PSY_", i), zipfiletable$path))
    br <- setDT(read.csv2(archive_read(zipfilename, row_number_b, mode = "r")))
    p <- setDT(read.csv2(archive_read(zipfilename, row_number_p, mode = "r")))
    # Save to Rdata
    saveRDS(br, file = paste0("Data/In/BRULES/BRULES_", i, ".rds"))
    saveRDS(p, file = paste0("Data/In/PSY/PSY_", i, ".rds"))
}

for (i in 2016:2022) {
    zipfilename <- paste0("Data/In/Raw/sae_stat_", i, ".7z")
    zipfiletable <- setDT(archive(zipfilename))
    row_number_b <- (grep(paste0("BLOCS_", i), zipfiletable$path))
    bl <- setDT(read.csv2(archive_read(zipfilename, row_number_b, mode = "r")))
    # Save to Rdata
    saveRDS(bl, file = paste0("Data/In/BLOCS/BLOCS_", i, ".rds"))
}

# ID.csv contains the hospital ID
if (!dir.exists("Data/In/ID")) {
    dir.create("Data/In/ID", showWarnings = TRUE)
}

for (i in 2016:2022) {
    zipfilename <- paste0("Data/In/Raw/sae_stat_", i, ".7z")
    zipfiletable <- setDT(archive(zipfilename))
    row_number <- (grep(paste0("ID_", i), zipfiletable$path))[1]
    id <- setDT(read.csv2(archive_read(zipfilename, row_number, mode = "r")))
    # Save to Rdata
    saveRDS(id, file = paste0("Data/In/ID/ID_", i, ".rds"))
}

# ---- HOSPIDIAG: Extract the xlsx from the zip file and save the sheet of interests as Rds ---- #
# The Hospidiag dataset
hospidiag <- "Data/In/Raw/hospidiag_opendata.zip"
a <- archive(hospidiag)
year <- 2017:2022
for (i in seq_along(year)) { # seq_along is used to get the index of the vector
    archive_extract(hospidiag, "Data/In/Hospidiag", i)
    f <- read_xlsx(paste0("Data/In/Hospidiag/", a$path[i]), col_names = TRUE, sheet = paste0("hd", year[i]))
    x <- setDT(f)
    x[, AN := year[i]]
    saveRDS(x, file = paste0("Data/In/Hospidiag/hd_", year[i], ".rds"))
}

# ---- Extend to 2013-2015 ---- #
# ---- HOSPIDIAG ---- #
for (i in 2013:2015) {
    f <- read_xlsx(paste0("Data/In/Hospidiag/hospidiag_opendata_", i, ".xlsx"), col_names = TRUE, sheet = paste0("hd", i))
    x <- setDT(f)
    x[, AN := i]
    saveRDS(x, file = paste0("Data/In/Hospidiag/hd_", i, ".rds"))
}

# ---- SAE ---- #
SheetExtract <- function(sheet_names, start, end) {
    for (i in start:end) {
        zipfilename <- paste0("Data/In/Raw/sae_stat_", i, ".7z")
        zipfiletable <- setDT(archive(zipfilename))
        for (sheet_name in sheet_names) {
            row_number <- (grep(paste0(sheet_name, "_"), zipfiletable$path))[1]
            x <- setDT(read.csv2(archive_read(zipfilename, row_number, mode = "r")))
            colnames(x) <- toupper(colnames(x))
            x[, `:=`(AN = as.numeric(AN), FI = as.character(FI))]
            dir_name <- paste0("Data/In/", sheet_name, "/")
            if (!dir.exists(dir_name)) {
                dir.create(dir_name, showWarnings = TRUE)
            }
            saveRDS(x, file = paste0(dir_name, sheet_name, "_", i, ".rds"))
        }
    }
}
# MCO contains output
# Q20, Q21, Q23, Q24 contains labor input
# ID contains the hospital ID
sheet <- c("MCO", "Q20", "Q21", "Q23", "Q24", "ID")
SheetExtract(sheet, 2013, 2015)

SheetExtract(c("URGENCES2"), 2013, 2015)
SheetExtract(c("PSY"), 2013, 2015)
SheetExtract(c("SSR", "USLD", "HAD"), 2013, 2015)
SheetExtract("HAD", 2013, 2015)
