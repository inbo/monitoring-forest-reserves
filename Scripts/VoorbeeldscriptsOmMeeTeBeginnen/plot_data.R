# rm(list=ls())   # om workspace leeg te maken


#### Setup ----
library(here)

# libraries & invoergegevens
source(here::here("scripts/VoorbeeldscriptsOmMeeTeBeginnen/Setup_ForresCalc.R"))    # runt het setup-script met info over pad-namen en nuttige packages
# OF
source(here::here("scripts/Setup.R")) 


#### lookuplijsten inladen ----

# rechtstreeks uit fieldmap-db
con <- odbcConnectAccess2007(path_to_fieldmap_db)

qIufroheight <- sqlFetch(con, "qiufroheight", stringsAsFactors = FALSE)
qIndShootCop <- sqlFetch(con, "qIndShootCop", stringsAsFactors = FALSE)
qAliveDead <- sqlFetch(con, "qAliveDead", stringsAsFactors = FALSE)
qSpecies <- sqlFetch(con, "qspecies", stringsAsFactors = FALSE)
qDecaystage <- sqlFetch(con, "qdecaystage", stringsAsFactors = FALSE)

odbcClose(con)

# OF mbv read_forresdat
qSpecies <- read_forresdat("qSpecies", repo_path = "C:/3BR/2_VisualisatieDataBR/1Packages/forresdat", join_plotinfo = FALSE)
#OF
qSpecies <- read_forresdat("qSpecies", repo_path = path_to_git_forresdat, join_plotinfo = FALSE)


#### Reeds berekende plotdata inladen ----
dendro_by_plot_species <- read_forresdat("dendro_by_plot_species", repo_path = "C:/3BR/2_VisualisatieDataBR/1Packages/forresdat", join_plotinfo = TRUE)
# OF
dendro_by_plot_species <- read_forresdat("dendro_by_plot_species", repo_path = path_to_git_forresdat, join_plotinfo = TRUE)
# join_plotinfo = TRUE : is default en hoeft niet noodzakelijk vermeld te worden
dendro_by_plot <- read_forresdat("dendro_by_plot", repo_path = path_to_git_forresdat, join_plotinfo = TRUE)
# plotinfo <- read_forresdat("plotinfo", repo_path = path_to_git_forresdat)
plotinfo <- read_vc(file = "plotinfo", root = "C:/03_BR/2_Forrescalc_Forresdat/2_forresdat/data/")



# OF mbv read_vc als read_forresdat mankeert (read_forresdat = functie uit package van Els; read_vc is algemener)
# hier wordt de tsv-file die op c-schijf staat, rechtstreeks ingeladen
dendro_by_plot_species <- read_vc(file = "dendro_by_plot_species", root = "C:/3BR/2_VisualisatieDataBR/1Packages/forresdat/data")


#### Soortenlijst koppelen aan plotdata ----
str(dendro_by_plot_species)
str(qSpecies)

dendro_by_plot_species2 <- dendro_by_plot_species %>%
  left_join (qSpecies, by = c("species" = "ID")) %>%  # by = c("a" = "b") will match x$a to y$b
  select(-Value3, -OrderField, -Active) %>%
  rename(NameNl = Value1,
         SpeciesName = Value2)  # new_name = old_name

str(dendro_by_plot_species2)


#### statistieken berekenen -----
?create_statistics

# dataset aanmaken waarin we geïnteresseerd zijn
dataset <- dendro_by_plot_species2 %>%
  filter(plottype == 20 & forest_reserve == "Muizenbos")


# functie create_statistics
stat <- create_statistics(
  dataset = dataset,
  level = c("period", "forest_reserve"),
  variables = "volume_alive_m3_ha",
  include_year_range = FALSE
)

str(stat)
head(stat)

# !!! resultaat lijkt niet OK
# ?? wat was onze input??  volume_alive_m3_ha per boomsoort!!!

stat <- create_statistics(
  dataset = dataset,
  level = c("period", "forest_reserve", "NameNl"),
  variables = "volume_alive_m3_ha",
  include_year_range = FALSE
)


# Statistieken per bosreservaat, zonder soortgegevens ----

dendro_by_plot <- read_forresdat("dendro_by_plot", repo_path = "C:/3BR/2_VisualisatieDataBR/1Packages/forresdat", join_plotinfo = TRUE)
# join_plotinfo = TRUE : is default en hoeft niet noodzakelijk vermeld te worden

dataset <- dendro_by_plot %>%
  filter(plottype == 20)   # enkel cirkelplots

stat <- create_statistics(
  dataset = dataset,
  level = c("period", "forest_reserve"),
  variables = "volume_alive_m3_ha",
  include_year_range = FALSE
)

str(stat)
head(stat)

