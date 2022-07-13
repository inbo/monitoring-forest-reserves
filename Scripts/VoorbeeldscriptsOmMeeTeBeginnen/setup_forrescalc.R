

library(forrescalc)
# https://github.com/inbo/forresdat


# Algemene documentatie
vignette("overview_forrescalc", package = "forrescalc")


### Paths ----
path_to_fieldmap <- "C:/3BR/5_MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
path_to_git_forresdat <- "C:/3BR/2_VisualisatieDataBR/1Packages/forresdat"

path_to_analysis_set <- "C:/3BR/1_DataVerwerkingBR/Output/BR_analysedb.accdb"
path_to_analysis_set_csv <- "C:/3BR/1_DataVerwerkingBR/Output/BR_analyseset_csv_files/"
path_to_results <- "C:/3BR/1_DataVerwerkingBR/Output/BR_resultaten.accdb"

path_to_forrescalc <- "C:/3BR/2_VisualisatieDataBR/1Packages/forrescalc/R/"
path_to_strata <- "C:/3BR/1_DataVerwerkingBR/Data/Strata/"


### Libraries ----
library(tidyr)
library (rlang)
library(dplyr)
library(RODBC)
library(lubridate)
library(openssl)
library(kableExtra)
library(stringr)
library(here)
library(tibble)
library(tidyselect)
library(readxl)
library(git2rdata)
# library(conflicted)  #ivm problemen unused argument bij "here", lukt ook door here::here te gebruiken


# bovenstaande libraries zitten allemaal vervat in package "forrescalc"
# !! toch best inladen, want soms nodig voor eigen code (buiten de functies van forrescalc)
library(forrescalc)
