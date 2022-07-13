# DOEL: datacontrole trees - focus op positie trees (XY-coordinaat) ----
##############################################

library(forrescalc)


### Setup ----
path_to_fieldmap <- "C:/3BR/5_dB_Els_deel2/FieldMapData_dB_Els_deel2.accdb"
path_to_git_forresdat <- "C:/3BR/2_VisualisatieDataBR/2Packages/forresdat"

path_to_output <- "C:/3BR/1_DataVerwerkingBR/Validatie/deel2_1e_ronde"
path_to_output_db <- "C:/3BR/1_DataVerwerkingBR/Validatie/results_datacontrole.accdb"
# !! zelf de lege access databank aanmaken in de desbetreffende folder


library(tidyr)
library (rlang)
library(dplyr)
library(RODBC)
library(lubridate)
library(readr)



### Inladen trees -----

# normaal kan dit met de functie load_dendro
data_dendro <- load_data_dendrometry(database = path_to_fieldmap, plottype = "Circular plot")


# MAAR, op dit moment laadt deze de tresholds nog niet in, daarom  hierna met behulp van queries

query_dendro <-
    "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        (Trees.X_m - Plots.Xorig_m) AS x_local, (Trees.Y_m - Plots.Yorig_m) AS y_local,
        pd.ForestReserve AS forest_reserve,
        pd.Date_dendro_1eSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.DecayStage AS decaystage,
        Trees.Vol_tot_m3 AS vol_tot_m3,
        Trees.Vol_stem_m3 AS vol_stem_m3,
        Trees.Vol_crown_m3 AS vol_crown_m3,
        Trees.BasalArea_m2 AS basal_area_m2,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS tree_number,
        Trees.Individual AS individual
      FROM ((Plots INNER JOIN Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots)"

query_dendro2 <-
    "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        (Trees.X_m - Plots.Xorig_m) AS x_local, (Trees.Y_m - Plots.Yorig_m) AS y_local,
        pd.ForestReserve AS forest_reserve,
        pd.Date_dendro_2eSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.DecayStage AS decaystage,
        Trees.Vol_tot_m3 AS vol_tot_m3,
        Trees.Vol_stem_m3 AS vol_stem_m3,
        Trees.Vol_crown_m3 AS vol_crown_m3,
        Trees.BasalArea_m2 AS basal_area_m2,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS tree_number,
        Trees.Individual AS individual,
        Trees.OldID as old_id
      FROM ((Plots INNER JOIN Trees_2eSET Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots);"

query_dendro3 <-
  "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        (Trees.X_m - Plots.Xorig_m) AS x_local, (Trees.Y_m - Plots.Yorig_m) AS y_local,
        pd.ForestReserve AS forest_reserve,
        pd.Date_dendro_3eSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.DecayStage AS decaystage,
        Trees.Vol_tot_m3 AS vol_tot_m3,
        Trees.Vol_stem_m3 AS vol_stem_m3,
        Trees.Vol_crown_m3 AS vol_crown_m3,
        Trees.BasalArea_m2 AS basal_area_m2,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS tree_number,
        Trees.Individual AS individual,
        Trees.OldID as old_id
      FROM ((Plots INNER JOIN Trees_3eSET Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN PlotDetails_3eSet pd ON Plots.ID = pd.IDPlots);"

con <- odbcConnectAccess2007(path_to_fieldmap)
data_dendro <- sqlQuery(con, query_dendro, stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    period = 1
  ) %>%
  bind_rows(
    sqlQuery(con, query_dendro2, stringsAsFactors = FALSE) %>%
      mutate(
        period = 2
      )
  ) %>%
  bind_rows(
    sqlQuery(con, query_dendro3, stringsAsFactors = FALSE) %>%
      mutate(
        period = 3
      )
  ) %>%
  mutate(
    year = year(round_date(.data$date_dendro, "year")) - 1,
    subcircle =
      ifelse(
        .data$alive_dead == 11 & .data$dbh_mm >= dbh_min_a4,
        "A4",
        ifelse(
          .data$alive_dead == 12 & .data$dbh_mm >= dbh_min_a4_dead,
          "A4",
          "A3"
        )
      ),
    subcirclearea_ha =
      ifelse(
        .data$subcircle == "A4",
        (pi * .data$r_A4 ^ 2)/10000,
        (pi * .data$r_A3 ^ 2)/10000
      ),
    plotarea_ha =
      ifelse(
        .data$plottype == 20,
        .data$subcirclearea_ha,
        NA
      ),
    plotarea_ha =
      ifelse(
        .data$plottype == 30,
        (.data$length_core_area_m * .data$width_core_area_m)/10000,
        .data$plotarea_ha
      ),
    plotarea_ha =
      ifelse(
        .data$plottype == 30 & is.na(.data$plotarea_ha),
        .data$core_area_ha,
        .data$plotarea_ha
      ),
    plotarea_ha =
      ifelse(
        is.na(.data$plotarea_ha),
        .data$totalplotarea_ha,
        .data$plotarea_ha
      ),
    dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm)
  )
odbcClose(con)

table(data_dendro$period, data_dendro$forest_reserve)

# Detecteren trees outside plot -----
names(data_dendro)
outside_plot <- data_dendro %>%
  filter(plottype == 20) %>%
  filter(!is.na(alive_dead)) %>%
  filter(!is.na(dbh_mm)) %>%
  filter(!is.na(dbh_min_a4)) %>%
  mutate(
    problem =
      ifelse(
        sqrt(x_local ^ 2 + y_local ^ 2) > r_A4,
        "tree not in A4",
        NA)
      ) %>%
  mutate(
      problem =
      ifelse(
        alive_dead == 11 & dbh_mm < dbh_min_a4 &
          (sqrt(x_local ^ 2 + y_local ^ 2) > r_A3),
        "tree not in A3",
        problem)
      ) %>%
  mutate(
    problem =
      ifelse(
        alive_dead == 12 & dbh_mm < dbh_min_a4_dead &
          sqrt(x_local ^ 2 + y_local ^ 2) > r_A3,
        "tree not in A3",
        problem
      )
  )

table(outside_plot$forest_reserve, outside_plot$problem)
table(outside_plot$period, outside_plot$problem)


# Make table wide ----
# 3 periodes samen
# create_overview_status reeds aangepast aan 3 periodes


# tabel compacter maken
outside_plot_set123 <- outside_plot %>%
  # filter(period == 1 | period == 2) %>%
  create_overview_status() %>%
  select(forest_reserve, plot_id, tree_id, period, x_local, y_local, problem, dbh_mm, alive_dead, species, r_A3, r_A4, dbh_min_a4, dbh_min_a4_dead)

# make table wide
outside_plot_set123_wide <- make_table_wide(table_long = outside_plot_set123,
                                            column_to_repeat = c("period"),
                                            columns_for_comparison = c("x_local", "y_local", "problem", "dbh_mm", "alive_dead", "r_A3", "r_A4", "dbh_min_a4", "dbh_min_a4_dead")
)

# Select trees outside plot ----
outside_plot_set123_wide <- outside_plot_set123_wide %>%
  filter(!is.na(problem_1) | !is.na(problem_2) | !is.na(problem_3))


# Waar oldID verwijderen? ----

# Als bv. boom uit periode 1 verwijderd wordt, wegens nog te smal voor A4, moet de oldID verwijderd worden in periode 2
# Idem voor periode 3 versus periode 2

# waar old_id verwijderen?
# daar waar set1 moet verwijderd worden en set2 niet
# en waar set2 een problem heeft, maar set3 niet


delete_oldID_set23<- outside_plot_set123_wide %>%
  filter(!is.na(problem_1) | !is.na(problem_2) | !is.na(problem_3)) %>%
  filter((!is.na(problem_1) & !is.na(dbh_mm_2))
         | (!is.na(problem_2) & !is.na(dbh_mm_3))
         )



# Export te verwijderen oldID's (1 record per boom)-----

# wegschrijven naar een csv (xls) tabel
write_csv2(outside_plot_set123_wide, paste0(path_to_output, "/", "outside_plot.csv"))
write_csv2(delete_oldID_set23, paste0(path_to_output, "/", "delete_oldID_set23.csv"))

# OF naar access-db
# !! eerst de lege databnk aanmaken in de desbetreffende folder
con <- odbcConnectAccess2007(path_to_output_db)  # connectie openen

sqlDrop(con, "outside_plot")
sqlSave(con, outside_plot_set123_wide, "outside_plot")

sqlDrop(con, "delete_oldID_set23")
sqlSave(con, delete_oldID_set23, "delete_oldID_set23")

odbcClose(con)  # connectie sluiten


