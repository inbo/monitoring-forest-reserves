# This script shows how data can be loaded from the Fieldmap database,
# aggregated to the plot level and saved to a local version of forresdat.
# The path to Fieldmap given here is to the example database in forresdat
# (replace by path to own local database to use all data)
# and the path to a local version is taken from an environmental variable.
# This variable can be generated on the level of your RStudio project by
# adding a text file named .Renviron to the root of your project folder and
# add text 'path_to_git_forresdat="your/path"' as content.
# (To add more variables, use 1 line for each variable, and no separation
# symbols, just an enter to start a new line.)
# This variable can also be set on the level of a RStudio user by adding the
# file .Renviron in the folder given by Sys.getenv("R_USER")
# And it can be set on the level of the computer by adding an environmental
# variable to the system (to have it also available in software other than R).
# (After setting, restart RStudio to make the environmental variable available!)

library(tidyverse)
library(forrescalc)

path_to_fieldmap <- Sys.getenv("path_to_fieldmap")
path_to_git_forresdat <- Sys.getenv("path_to_git_forresdat")

# only when q-tables in Fieldmap have changed
# (and only mention the changed table)
temp <- tempfile(fileext = ".xlsx")  # naam maken
dl <- googledrive::drive_download(
  googledrive::as_id("12x2H9lp86R-AFPdN2JXB9nqwJ2_A6PF6"),  
  #verwijst naar metadata file "_metadata_lookuplists.xlsx"
  path = temp, overwrite = TRUE
)
# drive_download gaat downloaden van gdrive naar path "temp"
# dl wordt zelf niet gebruikt, enkel nodig om te zorgen dat download gebeurt

from_access_to_forresdat(
  database = path_to_fieldmap,
  tables = c("qAliveDead", "qSpecies", "qHeightClass_regeneration",
             "qnumber_regeneration_classes", "qdecaystage"),
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)
# metadata_path: hier metadata van lookuplijsten
# is om legende toe te kennen aan de lookuplijsten
# variabele die nodig is bij 


# download .xlsx with metadata of table plotinfo to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("1tUNtlwcSnlVXnri235gqnhIHaBNf1Z2W"), path = temp,
  overwrite = TRUE
)
# metadata van plotinfo

plotinfo <- load_plotinfo(database = path_to_fieldmap)
save_results_forresdat(
  results = list(plotinfo = plotinfo),
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# DENDROMETRY

data_dendro <-
  load_data_dendrometry(
    database = path_to_fieldmap
  )
data_deadwood <-
  load_data_deadwood(
    database = path_to_fieldmap
  )
data_shoots <-
  load_data_shoots(
    database = path_to_fieldmap
  )
height_model <- load_height_models()

dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_shoots,
                                height_model, plotinfo)

# download .xlsx with metadata of dendrometry tables to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("1M8vTjaPst9LXaDrN1vPVUrsgjaZtAbRi"), path = temp,
  overwrite = TRUE
)

save_results_forresdat(
  results = dendro,
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# REGENERATION

data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )

regeneration <- calculate_regeneration(data_regeneration)

# download .xlsx with metadata of regeneration tables to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("17M_TfOyjpqLzsFqQ_w1DXitzI7tnULR6"), path = temp,
  overwrite = TRUE
)

save_results_forresdat(
  results = regeneration,
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# VEGETATION

data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap
  )
data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap
  )

vegetation <- calculate_vegetation(data_vegetation, data_herblayer)

# download .xlsx with metadata of vegetation tables to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("1Ywwgb0WKGWv9OqJd_rXjZRYDJVpW_KOJ"), path = temp,
  overwrite = TRUE
)

save_results_forresdat(
  results = vegetation,
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)
