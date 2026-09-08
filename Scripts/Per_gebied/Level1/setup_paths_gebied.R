library(here)

# libraries & invoergegevens
source(here::here("scripts/Setup.R"))

forestreserve <- "Level1"
forestreserve_short <- "Level1"
# forestreserve_short <- "Wijnendale"
# plot_type <- "CA"
# plot_type <- "CP"
forestreserve_folder_pc <- paste0(path_to_reserves, "09_Level1")  # daar staan de fieldmap-projecten
forestreserve_folder_teamdrive <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/20_Per_gebied/2026_", forestreserve_short)
# jaar_analyse <- "2021"


# plots_to_exclude <- c(701, 707, 709, 727, 728, 738, 739, 740, 759, 760)

path_to_datacontrol <- paste0(forestreserve_folder_teamdrive, "/01_datacontrole/")
path_to_heightcurves <- paste0(forestreserve_folder_teamdrive, "/02_dh_curves/")

path_to_plotdata_teamdrive <- paste0(forestreserve_folder_teamdrive, "/03_output_R_plotlevel/")
path_to_stats_teamdrive <- paste0(forestreserve_folder_teamdrive, "/04_statistieken_BR/")


# EVENTUEEL NIET MEER GEBRUIKEN??
path_to_plotdata_pc <- paste0(forestreserve_folder_pc, "/verwerking_2025/output_forrescalc/") # c-schijf


# TIJDELIJK `path_to_fieldmap_db` naar nieuwe situatie mrt 2026 (met aanpassing nalv andere benadering hakhout)
path_to_fieldmap <- paste0(forestreserve_folder_pc, "/FMprojecten_2025/LEVEL1_inbo2025/")
dbFieldmap <- "FieldMapData_LEVEL1_inbo2025.accdb"
path_to_fieldmap_db <- paste0(path_to_fieldmap, dbFieldmap)

# ivm aanmaak/verkenning dhcurves
path_to_fieldmap_dhcurves <- path_to_fieldmap_db

path_to_datarequest <- paste0(forestreserve_folder_pc, "/verwerking_2025/2AanmaakDHcurves/")
# VirtualTreesHeightCurves

path_to_html <-  path_to_heightcurves
