library(here)

# libraries & invoergegevens
source(here::here("scripts/Setup.R"))

forestreserve <- "Bos Terrijst"
forestreserve_short <- "Terrijst"
# forestreserve_short <- "Wijnendale"
# plot_type <- "CA"
# plot_type <- "CP"
forestreserve_folder <- paste0(path_to_reserves, "06_Terrijst")
# jaar_analyse <- "2021"


###########
path_to_datarequest <- paste0(forestreserve_folder, "/verwerking_2026_1e2e3eSET/output_forrescalc/") # c-schijf
path_to_teamdrive_gebied <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/20_Per_gebied/2026_", forestreserve_short, "/")

# plots_to_exclude <- c(701, 707, 709, 727, 728, 738, 739, 740, 759, 760)

path_to_datacontrol <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/per_gebied/2026_", forestreserve_short, "/01_datacontrole/")
path_to_heightcurves <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/per_gebied/2026_", forestreserve_short, "/02_dh_curves/")

# TIJDELIJK `path_to_fieldmap_db` naar nieuwe situatie mrt 2026 (met aanpassing nalv andere benadering hakhout)
path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_22_inbo2026_20260313/")
dbFieldmap <- "FieldMapData_MDB_BR_22_inbo2026.accdb"
path_to_fieldmap_db <- paste0(path_to_fieldmap, dbFieldmap)
