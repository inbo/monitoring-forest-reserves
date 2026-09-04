library(here)

# libraries & invoergegevens
source(here::here("scripts/Setup.R"))

forestreserve <- "Kolmont"
forestreserve_short <- forestreserve
# forestreserve_short <- "Wijnendale"
# plot_type <- "CA"
# plot_type <- "CP"
forestreserve_folder <- paste0(path_to_reserves, "4_Kolmont")
# jaar_analyse <- "2021"


###########
path_to_datarequest <- paste0(forestreserve_folder, "/verwerking_2025_3eSET/output_forrescalc/") # c-schijf
path_to_teamdrive_gebied <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/20_Per_gebied/2021_", forestreserve_short, "/")

# plots_to_exclude <- c(701, 707, 709, 727, 728, 738, 739, 740, 759, 760)

path_to_datacontrol <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/20_Per_gebied/2021_", forestreserve_short, "/datacontrole/")

# TIJDELIJK `path_to_fieldmap_db` naar situatie jan 2025 (voor aanpassing nalv andere benadering hakhout)
path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20250123/")
dbFieldmap <- "FieldMapData_MDB_BR_X8_inbo2020.accdb"
path_to_fieldmap_db <- paste0(path_to_fieldmap, dbFieldmap)

