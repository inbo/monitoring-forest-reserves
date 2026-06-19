
## OPGEPAST: de basis-paden zijn opgenomen in .Renviron
#       ENkel de afgeleide paths zijn hier opgenomen
#######################################################

### ALGEMEEN ----

path_to_dataverwerking <- Sys.getenv("path_to_dataverwerking") #C:/03_BR/1_DataverwerkingBR/"
path_to_forrescalc <- Sys.getenv("path_to_forrescalc") #"C:/03_BR/2_Forrescalc/R/"
path_to_git_forresdat <- Sys.getenv("path_to_git_forresdat") #"C:/03_BR/2_Forresdat/"
path_to_metadata <- Sys.getenv("path_to_metadata") #"G:/Gedeelde drives/Team_Boseco_BR/PRJ_BR_AanvraagGegevens/00_METADATA-ALL_PLOTS/_metadata/"
path_to_forresheights <- Sys.getenv("path_to_forresheights") #"C:/03_BR/2_Forresheights/data/"   # csv, gitrepo

path_to_teamdrive <- Sys.getenv("path_to_teamdrive") #"G:/Gedeelde drives/Team_Boseco_BR/"
path_to_dataverwerking_teamdrive <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking")

# zou ev. ook AWS kunnen worden, dan wel vpn opzetten als niet in VAC
# vb-code van faunabeheer (sander devisscher)
    # library(DBI)
    # myconn <- dbConnect(odbc::odbc(),
    #                     .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//172.31.11.14/inbo/Prjdata/Projects/PRJ_Faunabeheer/Monitoring-Vraatschade/Fieldmap/FieldMapData_GameImpact_INBO.accdb")
    # 
    # Qry_Plots_ICEpunten_Name <- dbGetQuery(myconn, "SELECT * FROM Qry_Plots_ICEpunten_Name")

path_to_reserves <- Sys.getenv("path_to_reserves") #"C:/03_BR_gebieden/"


# Fieldmap-db ------

path_to_databases <- Sys.getenv("path_to_databases") #"C:/03_BR_db_monitoring/"

path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_23_INBO2026_20260618/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_23_INBO2026_20260505/")
          # mail Peter: aanpassingen:
          #   LET OP, de nieuwe versie bevat 23 in de naam, staat voor FMversie, werken momenteel nog in 22 maar 23 staat klaar voor installatie binnenkort.
          # 
          # Aanpassingen (39)
          # Ook in de treelayers kon de 15 NVT-stoof meegenomen worden in de CondLUlists
          # 
          # SHOOTlayers (19)
          # 5x CoppiceID toegevoegd, Number 10
          # Shoots
          # Shoots_2eSet
          # Shoots_3eSet
          # Shoots_Kluis2020
          # Shoots_1986
          # 
          # 2x attribute AliveDeadShoots toegevoegd
          # Shoots_Kluis2020
          # Shoots_1986
          # 
          # 3x4 CondLUlist aangepast (decayshoots + iufroklassen) =>MASTER naar AliveDeadShoots
          # Shoots
          # Shoots_Kluis2020
          # Shoots_1986
          # 
          # TreeLayers (20)
          # 5x4 aanpassingen in Treelayers (decay + iufroklassen) => 15 NVT-stoof
          # decay : 15 NVT-stoof (master) = 17 NVT-stoof (decay)
          # iufro (3x): 15 NVT-stoof (master) = 50 NVT-stoof (qIUFRO...)
          # 
          # Trees
          # Trees_2eSet
          # Trees_3eSet
          # Trees_Kluis2020
          # Trees_1986
          # 
          # Met de aanpassingen in de treelayers zijn de CUlists ook consitenter geworden en de flexibiliteit behouden, script die aanpast naar 15 bij toevoegen spildiameter is nog altijd elegante oplossing voor opmeting nieuwe bomen/stoven



# TIJDELIJK nog onderstaande moederdb gebruiken bij gebruik van forrescalc (21/5/2026)
              # path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_22_inbo2026_20260313/")

# ARCHIEF:
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_22_inbo2026_20251014/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20250311/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20250123/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20241205/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20241127/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20241119/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20241030/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20240930/")
# path_to_fieldmap <- paste0(path_to_databases, "MDB_BR_X8_inbo2020_20240613/")
# path_to_fieldmap <- "C:/03_BR_db_monitoring/MDB_BR_X8_inbo2020_20240530/"
# path_to_fieldmap <- "C:/03_BR_db_monitoring/3_dB_Els_deel2_vs20231012/"
# path_to_fieldmap <- "C:/03_BR_db_monitoring/dB_Els_deel2_vs20220714/"


dbFieldmap <- "FieldMapData_MDB_BR_23_INBO2026.accdb"    # nieuwe structuur shoots

# TIJDELIJK bij gebruik van aforrescalc (voor aanpassen nalv nieuwe structuur shoots)
        # dbFieldmap <- "FieldMapData_MDB_BR_22_inbo2026.accdb"
# ARCHIEF:
# dbFieldmap <- "FieldMapData_MDB_BR_X8_inbo2020.accdb"


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

path_to_analysis_set_db <- paste0(path_to_dataverwerking, "Output/BR_analysedb_20260326.accdb") # accessdb
# path_to_analysis_set_db <- paste0(path_to_dataverwerking, "Output/BR_analysedb_20251215.accdb") # accessdb
# path_to_analysis_set_db <- paste0(path_to_dataverwerking, "Output/BR_analysedb_20250310.accdb") # accessdb
# path_to_analysis_set_db <- paste0(path_to_dataverwerking, "Output/BR_analysedb_20240924.accdb") # accessdb


path_to_dbResults <- paste0(path_to_dataverwerking, "Output/BR_resultaten.accdb")
dbResults <- path_to_dbResults

path_to_dataverwerking_teamdrive <- Sys.getenv("path_to_dataverwerking_teamdrive") # "G:/Gedeelde drives/Team_Boseco_BR/PRJ_BR_Gegevensverwerking/"


# Output - gdrive (PRJ_BR_AanvraagGegevens) ----
# path_to_output_gdrive <- "G:/.shortcut-targets-by-id/0B0xcP-eNvJ9dQ2w5ZFhSZEpCU0E/PRJ_BOSECO_ALGEMEEN/PRJ_BR_AanvraagGegevens/"   # oude drive

path_to_output_gdrive <- paste0(path_to_teamdrive, "PRJ_BR_AanvraagGegevens/")

path_to_plotlevel_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_plot-level-data/") 
path_to_lulists_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_qXX_lookuplijsten/") 
path_to_treelevel_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_tree-level-data/")
path_to_statistics_gdrive <- paste0(path_to_output_gdrive, "00_METADATA-ALL_PLOTS/_statistics/") 


# Strata, Externe data, .... ------
dbExterneData <- paste0(path_to_data, "ExterneData/BR_ExterneData.accdb")

dbStrata <- "BR_Strata_2026-03-30.accdb"
# dbStrata <- "BR_Strata_2026-01-30.accdb"
# dbStrata <- "BR_Strata_2025-03-24.accdb"
# dbStrata <- "BR_Strata_2025-02-26.accdb"
# dbStrata <- "BR_Strata_2024-12-18.accdb"
# dbStrata <- "BR_Strata_2024-09-11.accdb"
# dbStrata <- "BR_Strata_2024-02-22.accdb"
# dbStrata <- "BR_Strata_2023-05-15.accdb"
# dbStrata <- "BR_Strata_2022-11-30.accdb"
# dbStrata <- "BR_Strata_2022-11-07.accdb"
# dbStrata <- "BR_Strata_2022-02-14.accdb"
# dbStrata <- "BR_Strata_2022-02-11.accdb"
path_to_strata_db <- paste0(path_to_data, "Strata/", dbStrata) # moet op c-schijf staan

path_to_strata_gdrive <- paste0(path_to_output_gdrive
                               , "00_METADATA-ALL_PLOTS/strata/")
path_to_strata_input <- paste0(path_to_strata_gdrive
                               , "input/")

# Extra meetgegevens ----
path_to_meetgegevens <- paste0(path_to_data, "Meetgegevens/")


# Hoogtemodellen - xlsx ----
path_to_height_models <- paste0(path_to_data, "Hoogtemodellen/")
path_to_heightmodels_teamdrive <- paste0(path_to_teamdrive, "PRJ_BR_Gegevensverwerking/50_Hoogtemodellen/")
# op git: path_to_forresheights <- "C:/03_BR/2_Forresheights/data/" 


# Data-aanvragen ------
path_to_datarequests <- Sys.getenv("path_to_datarequests")
path_to_datarequests_gdrive <- path_to_output_gdrive



# Shapefiles intensieve monitoring ---------
path_to_shp <- paste0(path_to_datarequests_gdrive, "00_METADATA-ALL_PLOTS/GIS-lagen_bosreservaten/")



# Oude Paths --------
# path_to_output <- paste0(path_to_dataverwerking, "Output/")
# path_to_analysis_set_csv <- paste0(path_to_output, "_plot-level-data/")  # csv
