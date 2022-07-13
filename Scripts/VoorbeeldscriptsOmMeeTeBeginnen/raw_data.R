# rm(list=ls())   # om workspace leeg te maken


#### Setup ----
library(here)

# libraries & invoergegevens
source(here::here("scripts/ScriptsToStart/Setup_ForresCalc.R"))    # runt het setup-script met info over pad-namen en nuttige packages

#### Lookuplijsten inladen ----

# rechtstreeks uit fieldmap-db
con <- odbcConnectAccess2007(path_to_fieldmap)

qIufroheight <- sqlFetch(con, "qiufroheight", stringsAsFactors = FALSE)
qIndShootCop <- sqlFetch(con, "qIndShootCop", stringsAsFactors = FALSE)
qAliveDead <- sqlFetch(con, "qAliveDead", stringsAsFactors = FALSE)
qSpecies <- sqlFetch(con, "qspecies", stringsAsFactors = FALSE)
qDecaystage <- sqlFetch(con, "qdecaystage", stringsAsFactors = FALSE)

odbcClose(con)

# OF mbv read_forresdat
qSpecies <- read_forresdat("qSpecies", repo_path = "C:/3BR/2_VisualisatieDataBR/1Packages/forresdat", join_plotinfo = FALSE)


#### Ruwe data inladen ----

# plotinfo: plottype en naam forest_reserve
plotinfo <- load_plotinfo(database = path_to_fieldmap)


# dendro
data_dendro <-
  load_data_dendrometry(
    database = path_to_fieldmap
  )

data_shoots <-
  load_data_shoots(
    database = path_to_fieldmap
  )
data_stems  <- compose_stem_data(data_dendro, data_shoots)


# deadwood
data_deadwood <-
  load_data_deadwood(
    database = path_to_fieldmap
  ) %>%
  filter(plot_id != 11000)  # in KV Kersselaerspleyn (plot 11000) no lying deadwood is meausured


# regeneration
data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )


# vegetation
data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap
  )
data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap
  )


### Status van alle bomen doorheen de tijd ----
# één record per boom (met info over meerdere jaren)

status_tree <- create_overview_status(data_dendro)
# status_tree bevat nog steeds meerdere records per boom: 1 record per boom en per jaartal
# verschil met data_dendro is de toevoeging van een unieke tree_id, die constant blijft doorheen de tijd
str(status_tree)

# als je 1 record per boom wil, moet je functie "make_table_wide" gebruiken
?make_table_wide
# make_table_wide(table_long, column_to_repeat, columns_for_comparison)

# OPGEPAST: deze functie groepeert op alle variabelen die niet genoemd worden in
# - column_to_repeat
# - columns_for_comparison
# => vooraf zorgen dat enkel de gewenste groeperende variabelen over blijven

table_long <- status_tree %>%
  filter(plot_id < 110) %>%
  select(plot_id, species, tree_id, period, dbh_mm, alive_dead)

status_tree_wide <- make_table_wide(table_long = table_long, column_to_repeat = "period",
                                    columns_for_comparison = c("dbh_mm", "alive_dead"))
str(status_tree_wide)


#### Soortenlijst koppelen aan ruwe data ----
str(status_tree_wide)
str(qSpecies)

status_tree_wide2 <- status_tree_wide %>%
  left_join (qSpecies, by = c("species" = "ID")) %>%  # by = c("a" = "b") will match x$a to y$b
  select(-Value3, -OrderField, -Active) %>%
  rename(NameNl = Value1,
         SpeciesName = Value2)  # new_name = old_name

str(status_tree_wide2)


#### Exporteren van aangemaakt data naar csv of access ----
write.csv2(status_tree_wide2, "Output/status_tree_Wide.csv")

# TIP: wegschrijven van één file naar een access-db
library(RODBC)

connectie <- odbcConnectAccess2007("C:/3BR/1_DataVerwerkingBR/Output/BR_resultaten.accdb")
sqlSave(connectie, status_tree_wide2)
odbcClose(connectie)



#### Exporteren van ruwe data naar csv en access ----
write.csv2(data_dendro, "Output/BR_ruwe_data/data_dendro.csv")
write.csv2(data_stems, "Output/BR_ruwe_data/data_stems.csv")
write.csv2(data_regeneration, "Output/BR_ruwe_data/data_regeneration.csv")
write.csv2(data_vegetation, "Output/BR_ruwe_data/data_vegetation.csv")
write.csv2(data_herblayer, "Output/BR_ruwe_data/data_herblayer.csv")


connectie <- odbcConnectAccess2007("C:/3BR/1_DataVerwerkingBR/Output/BR_resultaten.accdb")
sqlSave(connectie, data_stems)
odbcClose(connectie)



