

### ALGEMEEN ----

path_to_dataverwerking <- "C:/03_BR/1_DataverwerkingBR/"
path_to_forrescalc <- "C:/03_BR/2_Forrescalc_Forresdat/1_forrescalc/R/"
path_to_git_forresdat <- "C:/03_BR/2_Forrescalc_Forresdat/2_forresdat/"
path_to_fieldmap <- "C:/03_BR/3_dB_Els_deel2/"

# zou ev. ook AWS kunnen worden, dan wel vpn opzetten als niet in VAC
# vb-code van faunabeheer (sander devisscher)
    # library(DBI)
    # myconn <- dbConnect(odbc::odbc(),
    #                     .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//172.31.11.14/inbo/Prjdata/Projects/PRJ_Faunabeheer/Monitoring-Vraatschade/Fieldmap/FieldMapData_GameImpact_INBO.accdb")
    # 
    # Qry_Plots_ICEpunten_Name <- dbGetQuery(myconn, "SELECT * FROM Qry_Plots_ICEpunten_Name")

path_to_reserves <- "C:/03_BR_gebieden/"


# Fieldmap-db ------
dbFieldmap <- "FieldMapData_dB_Els_deel2.accdb"
path_to_fieldmap_db <- paste0(path_to_fieldmap, dbFieldmap)


# Data -------
path_to_forresdat_data <- paste0(path_to_git_forresdat, "data/") # tsv
path_to_data <- paste0(path_to_dataverwerking, "Data/") 
path_to_RData <- paste0(path_to_dataverwerking, "RData/")

# Output ----
path_to_plotlevel_csv <- paste0(path_to_dataverwerking, "Output/_plot-level-data/") 
path_to_treelevel_csv <- paste0(path_to_dataverwerking, "Output/_tree-level-data/")

path_to_analysis_set_db <- paste0(path_to_dataverwerking, "Output/BR_analysedb.accdb") # accessdb

path_to_dbResults <- paste0(path_to_dataverwerking, "Output/BR_resultaten.accdb")
dbResults <- path_to_dbResults


# Output - gdrive (PRJ_BR_AanvraagGegevens)
# path_to_output_gdrive <- "G:/.shortcut-targets-by-id/0B0xcP-eNvJ9dQ2w5ZFhSZEpCU0E/PRJ_BOSECO_ALGEMEEN/PRJ_BR_AanvraagGegevens/"   # oude drive

path_to_output_gdrive <- "G:/Gedeelde drives/Team_Boseco/00_projecten/PRJ_BR_AanvraagGegevens/"

path_to_plotlevel_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_plot-level-data/") 
path_to_treelevel_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_tree-level-data/")


# Strata, Externe data, .... ------
dbExterneData <- paste0(path_to_data, "ExterneData/BR_ExterneData.accdb")

dbStrata <- "BR_Strata_2023-05-15.accdb"
# dbStrata <- "BR_Strata_2022-11-30.accdb"
# dbStrata <- "BR_Strata_2022-11-07.accdb"
# dbStrata <- "BR_Strata_2022-02-14.accdb"
# dbStrata <- "BR_Strata_2022-02-11.accdb"
path_to_strata <- paste0(paste0(path_to_data, "Strata/"))
path_to_strata_db <- paste0(paste0(path_to_strata, dbStrata))


# Extra meetgegevens ----
path_to_meetgegevens <- paste0(paste0(path_to_data, "Meetgegevens/"))


# OVERIGE
path_to_height_models <- paste0(paste0(path_to_data, "Hoogtemodellen/"))

path_to_datarequests <- paste0(paste0(path_to_dataverwerking, "Aanvraag_gegevens/"))
path_to_datarequests_gdrive <- path_to_output_gdrive


# Oude Paths --------
# path_to_output <- paste0(path_to_dataverwerking, "Output/")
# path_to_analysis_set_csv <- paste0(path_to_output, "_plot-level-data/")  # csv
