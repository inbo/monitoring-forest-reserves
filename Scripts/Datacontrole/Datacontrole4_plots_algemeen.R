
# DOEL: via het berekenen van de standaard totalen eventuele fouten/ontbrekende waarden detecteren


# Bron: script Main_UpdataForresdat.R (en Main.R in package "Forrescalc")


rm(list=ls())


# Libraries & invoergegevens ----
source("Scripts/Setup_ForresCalc.R")


#### Plotinfo: plottype, naam forest_reserve en info over survey en data al dan niet processed -----
plotinfo <- load_plotinfo(database = path_to_fieldmap)

# tijdelijk uitgebreider (tot package geupdate is)
query_plot <-
  "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.DataProcessed_YN AS data_processed
    FROM Plots INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots;"

query_plot2 <-
  "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.DataProcessed_YN AS data_processed
    FROM Plots INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots;"

query_plot3 <-
  "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.DataProcessed_YN AS data_processed
    FROM Plots INNER JOIN PlotDetails_3eSet pd ON Plots.ID = pd.IDPlots;"

con <- odbcConnectAccess2007(path_to_fieldmap)
plotinfo <- sqlQuery(con, query_plot, stringsAsFactors = FALSE) %>%
  mutate(
    period = 1
  ) %>%
  bind_rows(
    sqlQuery(con, query_plot2, stringsAsFactors = FALSE) %>%
      mutate(
        period = 2
      )
  ) %>%
  bind_rows(
    sqlQuery(con, query_plot3, stringsAsFactors = FALSE) %>%
      mutate(
        period = 3
      )
  ) %>%
  distinct()
odbcClose(con)

summary(plotinfo)

plotinfo %>% filter(survey_trees == 10) %>% filter(is.na(data_processed)) %>% nrow()
# [1] 0


### Data_dendro berekenen ----
data_dendro <-
  load_data_dendrometry(
    database = path_to_fieldmap
  )
data_deadwood <-
  load_data_deadwood(
    database = path_to_fieldmap
  ) %>%
  filter(plot_id != 11000)  # in KV Kersselaerspleyn (plot 11000) no lying deadwood is meausured
data_shoots <-
  load_data_shoots(
    database = path_to_fieldmap
  )
data_stems  <- compose_stem_data(data_dendro, data_shoots)

dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_stems)


# WARNING: if plotid 11000 in data, replace (lying) deadwood results for plot_id = 11000 by "NA"
# (in plot KV Kersselaerspleyn (plot 11000) no lying deadwood is meausured)
dendro[["dendro_by_plot"]] <- dendro[["dendro_by_plot"]] %>%
  mutate(
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha),
    volume_deadwood_m3_ha  =
      ifelse(plot_id == 11000 & is.na(volume_log_m3_ha), NA, volume_log_m3_ha)
  )
dendro[["dendro_by_plot_species"]] <- dendro[["dendro_by_plot_species"]] %>%
  mutate(
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha),
    volume_deadwood_m3_ha  =
      ifelse(plot_id == 11000 & is.na(volume_log_m3_ha), NA, volume_log_m3_ha)
  )
dendro[["dendro_by_diam_plot"]] <- dendro[["dendro_by_diam_plot"]] %>%
  mutate(
    log_number_ha  =
      ifelse(plot_id == 11000 & log_number_ha == 0, NA, log_number_ha),
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha)
  )
dendro[["dendro_by_diam_plot_species"]] <- dendro[["dendro_by_diam_plot_species"]] %>%
  mutate(
    log_number_ha  =
      ifelse(plot_id == 11000 & log_number_ha == 0, NA, log_number_ha),
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha)
  )


# list opsplitsen ----

dendro_by_plot <- dendro[["dendro_by_plot"]]

dendro_by_plot_species <- dendro[["dendro_by_plot_species"]]

dendro_by_diam_plot<- dendro[["dendro_by_diam_plot"]]

dendro_by_diam_plot_species <- dendro[["dendro_by_diam_plot_species"]]


# plot 11000
plotinfo  %>% filter(plot_id == 11000)
# survey_deadw=20 => NA ipv '0'
t <- dendro[["dendro_by_plot"]] %>% filter(plot_id == 11000)



summary(dendro_by_plot)



# koppelen aan plotinfo
dendro_by_plot2 <- dendro_by_plot %>%
  left_join(plotinfo)

table(dendro_by_plot2$forest_reserve, dendro_by_plot2$data_processed)

foute_naam <- dendro_by_plot2 %>%
  filter(forest_reserve %in% c("Bos terrijst", "Everzijnbad"))

dendro_by_plot_ss <- dendro_by_plot2 %>% filter(data_processed == 10)
summary(dendro_by_plot_ss)



### Status_tree berekenen ----
# bevat een unieke tree_id per boom (die constant blijft doorheen de tijd),
# verschillend van de tree_measure_id uit data_dendro
# één record per boom en per periode
# om één record per boom te verkrijgen met ifo uit beide periodes,
# dient functie "make_table_wide" gebruikt te worden
dendro_status_tree <- create_overview_status(data_dendro = data_dendro)
# Deze tabel wordt NIET bewaard in forresdat (daar enkel geaggregeerde resultaten op plotniveau)

status_tree_long <- status_tree %>%
  select(plot_id, species, tree_id, period, dbh_mm, alive_dead)

status_tree_wide <- make_table_wide(table_long = status_tree_long,
                                    column_to_repeat = "period",
                                    columns_for_comparison = c("dbh_mm", "alive_dead"))



### Data_regeneration berekenen ----
data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )

regeneration <- calculate_regeneration(data_regeneration)



### Data_vegetation berekenen ----
data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap
  )
data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap
  )

vegetation <- calculate_vegetation(data_vegetation, data_herblayer)

