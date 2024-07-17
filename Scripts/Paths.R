

### ALGEMEEN ----

path_to_dataverwerking <- "C:/03_BR/1_DataverwerkingBR/"
path_to_forrescalc <- "C:/03_BR/2_Forrescalc/R/"
path_to_git_forresdat <- "C:/03_BR/2_Forresdat/"
path_to_forresheights <- "C:/03_BR/2_Forresheights/data/"   # csv, gitrepo

path_to_teamdrive <- "G:/Gedeelde drives/Team_Boseco_BR/"
path_to_dataverwerking_teamdrive <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking")

# zou ev. ook AWS kunnen worden, dan wel vpn opzetten als niet in VAC
# vb-code van faunabeheer (sander devisscher)
    # library(DBI)
    # myconn <- dbConnect(odbc::odbc(),
    #                     .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//172.31.11.14/inbo/Prjdata/Projects/PRJ_Faunabeheer/Monitoring-Vraatschade/Fieldmap/FieldMapData_GameImpact_INBO.accdb")
    # 
    # Qry_Plots_ICEpunten_Name <- dbGetQuery(myconn, "SELECT * FROM Qry_Plots_ICEpunten_Name")

path_to_reserves <- "C:/03_BR_gebieden/"


# Fieldmap-db ------

path_to_fieldmap <- "C:/03_BR_db_monitoring/MDB_BR_X8_inbo2020_20240613/"
# path_to_fieldmap <- "C:/03_BR_db_monitoring/MDB_BR_X8_inbo2020_20240530/"
# path_to_fieldmap <- "C:/03_BR_db_monitoring/3_dB_Els_deel2_vs20231012/"
# path_to_fieldmap <- "C:/03_BR_db_monitoring/dB_Els_deel2_vs20220714/"


dbFieldmap <- "FieldMapData_MDB_BR_X8_inbo2020.accdb"
path_to_fieldmap_db <- paste0(path_to_fieldmap, dbFieldmap)
path_to_fieldmap_db_all <- paste0(path_to_fieldmap, dbFieldmap)


# Data -------
path_to_forresdat_data <- paste0(path_to_git_forresdat, "data/") # tsv
path_to_data <- paste0(path_to_dataverwerking, "Data/") 
path_to_RData <- paste0(path_to_dataverwerking, "RData/")


# Output ----
path_to_plotlevel_csv <- paste0(path_to_dataverwerking, "Output/_plot-level-data/") 
path_to_lulists_csv <- paste0(path_to_dataverwerking, "Output/_qXX_lookuplijsten/") 
path_to_treelevel_csv <- paste0(path_to_dataverwerking, "Output/_tree-level-data/")

path_to_analysis_set_db <- paste0(path_to_dataverwerking, "Output/BR_analysedb.accdb") # accessdb

path_to_dbResults <- paste0(path_to_dataverwerking, "Output/BR_resultaten.accdb")
dbResults <- path_to_dbResults


# Output - gdrive (PRJ_BR_AanvraagGegevens) ----
# path_to_output_gdrive <- "G:/.shortcut-targets-by-id/0B0xcP-eNvJ9dQ2w5ZFhSZEpCU0E/PRJ_BOSECO_ALGEMEEN/PRJ_BR_AanvraagGegevens/"   # oude drive

path_to_output_gdrive <- paste0(path_to_teamdrive, "PRJ_BR_AanvraagGegevens/")

path_to_plotlevel_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_plot-level-data/") 
path_to_lulists_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_qXX_lookuplijsten/") 
path_to_treelevel_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_tree-level-data/")


# Strata, Externe data, .... ------
dbExterneData <- paste0(path_to_data, "ExterneData/BR_ExterneData.accdb")

dbStrata <- "BR_Strata_2024-02-22.accdb"
# dbStrata <- "BR_Strata_2023-05-15.accdb"
# dbStrata <- "BR_Strata_2022-11-30.accdb"
# dbStrata <- "BR_Strata_2022-11-07.accdb"
# dbStrata <- "BR_Strata_2022-02-14.accdb"
# dbStrata <- "BR_Strata_2022-02-11.accdb"
path_to_strata <- paste0(path_to_data, "Strata/")
path_to_strata_db <- paste0(path_to_strata, dbStrata)


# Extra meetgegevens ----
path_to_meetgegevens <- paste0(path_to_data, "Meetgegevens/")


# Hoogtemodellen - xlsx ----
path_to_height_models <- paste0(path_to_data, "Hoogtemodellen/")
path_to_heightmodels_teamdrive <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/Hoogtemodellen/")
# op git: path_to_forresheights <- "C:/03_BR/2_Forresheights/data/" 


# Data-aanvragen ------
path_to_datarequests <- "C:/03_BR/3_Aanvraag_gegevens/"
path_to_datarequests_gdrive <- path_to_output_gdrive



# Shapefiles intensieve monitoring ---------
path_to_shp <- paste0(path_to_datarequests_gdrive, "00_METADATA-ALL_PLOTS/GIS-lagen_bosreservaten/")



# Oude Paths --------
# path_to_output <- paste0(path_to_dataverwerking, "Output/")
# path_to_analysis_set_csv <- paste0(path_to_output, "_plot-level-data/")  # csv
