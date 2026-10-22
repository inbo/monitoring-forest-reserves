source(here::here("scripts/Setup.R"))

library(DBI)
library(RSQLite)

database <- "C:/03_BR/2_Forrescalc/inst/example/testdb/mdb_bosres_test.sqlite"


con <- dbConnect(RSQLite::SQLite(), database)
con <- odbcConnectAccess2007(path_to_fieldmap_db)

# sqlite lukt niet, checken met echte db (fieldmap) - lukt wel 
# selectie voor periode 2

# check_data_trees <- function(database, forest_reserve = "all") {

  query_trees <-
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      pd.rA3 AS r_a3, pd.rA4 AS r_a4,
      pd.TresHoldDBH_Trees_A4_alive AS treshold_alive,
      pd.TresHoldDBH_Trees_A4_dead AS treshold_dead,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.IUFROHght AS iufro_hght,
      Trees.IUFROVital AS iufro_vital,
      Trees.IUFROSocia AS iufro_socia,
      Trees.DecayStage AS decay_stage,
      Trees.Remark AS remark,
      Trees.CommonRemark AS commonremark,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID
    FROM ((Plots INNER JOIN Trees_2eSet Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_2eSet pd ON Plots.ID = pd.IDPlots;"
  
  query_shoots <-
    "SELECT shoots.IDPlots AS plot_id,
      shoots.XTrees_2eSet AS x_trees,
      shoots.YTrees_2eSet AS y_trees,
      shoots.IDTrees_2eSet AS id_trees,
      shoots.ID AS shoot_id,
      shoots.DBH_mm AS dbh_mm,
      shoots.Height_m AS height_m,
      shoots.IntactSnag
    FROM Shoots_2eSet shoots;"
  
  
  data_trees <- sqlQuery(con, query_trees) %>% 
    mutate(period = 2)
  data_shoots <- sqlQuery(con, query_shoots) %>% 
    mutate(period = 2)
  
  incorrect_trees <- data_trees %>%
    # not in A3 or A4
    mutate(
      location =
        ifelse(
          .data$plottype == "CP" &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > .data$r_a4,
          "tree not in A4",
          NA
        ),
      location =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 11 &
            .data$dbh_mm < .data$treshold_alive &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > .data$r_a3 &
            is.na(.data$location),
          "tree not in A3",
          .data$location
        ),
      location =
        ifelse(
          .data$plottype == "CP" & .data$alive_dead == 12 &
            .data$dbh_mm < .data$treshold_dead &
            sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > .data$r_a3 &
            is.na(.data$location),
          "tree not in A3",
          .data$location
        )
    ) %>%
    # shoots not correctly linked with trees
    left_join(
      data_trees %>%
        filter(.data$ind_sht_cop == 12) %>%
        anti_join(
          data_shoots,
          by = c("plot_id", "X_m" = "x_trees", "Y_m" = "y_trees",
                 "tree_measure_id" = "id_trees", "period")
        ) %>%
        select(
          "plot_id", "X_m", "Y_m", "tree_measure_id", "period"
        ) %>%
        mutate(
          link_to_layer_shoots = "missing"
        ),
      by = c("plot_id", "X_m", "Y_m", "tree_measure_id", "period")
    ) %>%
    mutate(
      # ratio D/H - geen snags
      ratio_dbh_height = round(.data$dbh_mm * pi / (.data$height_m * 10), 1),
      field_ratio_dbh_height =
        ifelse(
          .data$ratio_dbh_height < 1.35 & .data$intact_snag == 11,
          "tree too thin and high", NA),
      field_ratio_dbh_height =
        ifelse(
          .data$ratio_dbh_height > 16.5 & .data$intact_snag == 11,
          "tree too thick and low", .data$field_ratio_dbh_height
        ),
      field_dbh_mm = ifelse(is.na(.data$dbh_mm), "missing", NA),
      field_dbh_mm =
        ifelse(
          !is.na(.data$dbh_mm) & .data$dbh_mm > 2000,
          "too high", .data$field_dbh_mm
        ),
      field_height_m =
        ifelse(is.na(.data$height_m) & .data$intact_snag == 10, "missing", NA),
      field_height_m =
        ifelse(
          !is.na(.data$height_m) & .data$height_m > 50, "too high",
          .data$field_height_m
        ),
      field_height_m =
        ifelse(
          !is.na(.data$height_m) & .data$height_m < 1.3, "too low",
          .data$field_height_m
        ),
      field_species = ifelse(is.na(.data$species), "missing", NA),
      field_intact_snag = ifelse(is.na(.data$intact_snag), "missing", NA),
      field_intact_snag =
        ifelse(
          !.data$intact_snag %in% c(10, 11) & !is.na(.data$intact_snag),
          "not in lookuplist", .data$field_intact_snag
        ),
      field_alive_dead = ifelse(is.na(.data$alive_dead), "missing", NA),
      field_alive_dead =
        ifelse(
          !.data$alive_dead %in% c(11, 12, 15) & !is.na(.data$alive_dead),
          "not in lookuplist", .data$field_alive_dead
        ),
      field_ind_sht_cop = ifelse(is.na(.data$ind_sht_cop), "missing", NA),
      field_ind_sht_cop =
        ifelse(
          !.data$ind_sht_cop %in% c(10, 11, 12) & !is.na(.data$ind_sht_cop),
          "not in lookuplist", .data$field_ind_sht_cop
        ),
      field_decay_stage =
        ifelse(
          is.na(.data$decay_stage) & .data$alive_dead == 12 &
            (.data$ind_sht_cop %in% c(10, 11) | is.na(.data$ind_sht_cop)),
          "missing", NA
        ),
      field_decay_stage =
        ifelse(
          !is.na(.data$decay_stage) &
            !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16, 17),
          "not in lookuplist",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage %in% c(10, 11, 12, 13, 14, 15) &
            .data$alive_dead == 11 & !is.na(.data$decay_stage),
          "tree alive",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage == 16 &
            .data$alive_dead == 12 & is.na(.data$field_decay_stage),
          "tree not alive",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          is.na(.data$decay_stage) & .data$ind_sht_cop %in% c(10, 11) &
            .data$alive_dead == 12 & is.na(.data$field_decay_stage),
          "missing",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage == 17 & !is.na(.data$decay_stage) &
            .data$ind_sht_cop %in% c(10, 11),
          "tree no coppice",
          .data$field_decay_stage),
      field_iufro_hght =
        ifelse(is.na(.data$iufro_hght) & !.data$period %in% c(0, 1),
               "missing", NA),
      field_iufro_hght =
        ifelse(
          !is.na(.data$iufro_hght) &
            !.data$iufro_hght %in% c(10, 20, 30, 40, 50),
          "not in lookuplist", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_hght),
          "tree not alive",
          .data$field_iufro_hght),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_hght),
          "tree alive", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght == 50 & .data$ind_sht_cop %in% c(10, 11) &
            !is.na(.data$iufro_hght),
          "tree no coppice", .data$field_iufro_hght
        ),
      field_iufro_vital =
        ifelse(is.na(.data$iufro_vital) & !.data$period %in% c(0, 1),
               "missing", NA),
      field_iufro_vital =
        ifelse(
          !.data$iufro_vital %in% c(10, 20, 30, 40, 50) &
            !is.na(.data$iufro_vital),
          "not in lookuplist", .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_vital),
          "tree not alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_vital),
          "tree alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital == 50 & .data$ind_sht_cop %in% c(10, 11) &
            !is.na(.data$iufro_vital),
          "tree no coppice",
          .data$field_iufro_vital
        ),
      field_iufro_socia =
        ifelse(is.na(.data$iufro_socia) & !.data$period %in% c(0, 1),
               "missing", NA),
      field_iufro_socia =
        ifelse(
          !.data$iufro_socia %in% c(10, 20, 30, 40, 50) &
            !is.na(.data$iufro_socia),
          "not in lookuplist",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_socia),
          "tree not alive",
          .data$field_iufro_socia),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_socia),
          "tree alive",
          .data$field_iufro_socia
        ),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia == 50 & .data$ind_sht_cop %in% c(10, 11) &
            !is.na(.data$iufro_socia),
          "tree no coppice",
          .data$field_iufro_socia),
      tree_measure_id = as.character(.data$tree_measure_id),
      species = as.character(.data$species)
    ) %>%
    mutate(
      across(
        where(~ is.numeric(.x)) & !matches(c("plot_id", "period")),
        as.character
      )
    ) %>%
    pivot_longer(
      cols =
        c("location", "link_to_layer_shoots", starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL, remark = NULL
    ) %>%
    pivot_longer(
      cols =
        !c("plot_id", "tree_measure_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(
      .data$aberrant_field == .data$varname |
        .data$aberrant_field %in%
        c("location", "link_to_layer_shoots")
    ) %>%
    mutate(
      aberrant_value =
        ifelse(
          .data$aberrant_field %in%
            c("location", "link_to_layer_shoots"),
          NA,
          .data$aberrant_value
        )
    ) %>%
    select(-"varname") %>%
    distinct()
  
  
  return(incorrect_trees)
}
