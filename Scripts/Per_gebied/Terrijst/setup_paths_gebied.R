library(here)

# libraries & invoergegevens
source(here::here("scripts/Setup.R"))

forestreserve <- "Bos Terrijst"
forestreserve_short <- "Terrijst"
# forestreserve_short <- "Wijnendale"
# plot_type <- "CA"
# plot_type <- "CP"
forestreserve_folder_pc <- paste0(path_to_reserves, "06_Terrijst")  # daar staan de fieldmap-projecten
forestreserve_folder_teamdrive <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/20_Per_gebied/2026_", forestreserve_short)
# jaar_analyse <- "2021"


# plots_to_exclude <- c(701, 707, 709, 727, 728, 738, 739, 740, 759, 760)

path_to_datacontrol <- paste0(forestreserve_folder_teamdrive, "/01_datacontrole/")
path_to_heightcurves <- paste0(forestreserve_folder_teamdrive, "/02_dh_curves/")

path_to_plotdata_teamdrive <- paste0(forestreserve_folder_teamdrive, "/03_output_R_plotlevel")

# EVENTUEEL NIET MEER GEBRUIKEN??
path_to_plotdata_pc <- paste0(forestreserve_folder_pc, "/verwerking_2026_1e2e3eSET/output_forrescalc/") # c-schijf


# TIJDELIJK `path_to_fieldmap_db` naar nieuwe situatie mrt 2026 (met aanpassing nalv andere benadering hakhout)
path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_22_inbo2026_20260313/")
dbFieldmap <- "FieldMapData_MDB_BR_22_inbo2026.accdb"
path_to_fieldmap_db <- paste0(path_to_fieldmap, dbFieldmap)
