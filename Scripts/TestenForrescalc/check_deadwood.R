source(here::here("scripts/Setup.R"))

library(DBI)
library(RSQLite)

database <- "C:/03_BR/2_Forrescalc/inst/example/testdb/mdb_bosres_test.sqlite"

con <- dbConnect(RSQLite::SQLite(), database)
# con <- odbcConnectAccess2007(path_to_fieldmap_db)


# check_data_deadwood <- function(database, forest_reserve = "all") {
#   selection <-
#     ifelse(
#       forest_reserve == "all", "",
#       paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
#     )
  
# sqlite lukt niet, checken met echte db (fieldmap) - lukt wel 
# selectie voor periode 2
  
  query_deadwood <-
    "SELECT Deadwood.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Deadwood.ID AS lying_deadw_id,
      Deadwood.IntactFragment AS intact_fragment,
      Deadwood.AliveDead AS alive_dead,
      Deadwood.DecayStage AS decay_stage
    FROM ((Plots
      INNER JOIN Deadwood Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_1eSet pd ON Plots.ID = pd.IDPlots;"
  
  query_deadwood_diameters <-
    "SELECT dwd.IDPlots As plot_id,
      dwd.IDDeadwood AS lying_deadw_id,
      dwd.Distance_m AS distance_m,
      dwd.Diameter_mm AS diameter_mm
    FROM Deadwood_Diameters dwd;"
  
  data_deadwood <- sqlQuery(con, query_deadwood) %>%
    mutate(period = 1)
  data_deadwood_diameters <- sqlQuery(con, query_deadwood_diameters) %>%
    mutate(period = 1)
  
  incorrect_deadwood <- data_deadwood %>%
    left_join(
      data_deadwood_diameters %>%
        group_by(.data$plot_id, .data$lying_deadw_id) %>%
        summarise(max_diameter_mm = max(.data$diameter_mm)) %>%
        ungroup() %>%
        filter(.data$max_diameter_mm < 100) %>%
        mutate(
          field_max_diameter_mm = "too low"
        ),
      by = c("plot_id", "lying_deadw_id")
    ) %>%
    mutate(
      field_intact_fragment =
        ifelse(is.na(.data$intact_fragment), "missing", NA),
      field_intact_fragment =
        ifelse(
          !is.na(.data$intact_fragment) &
            !.data$intact_fragment %in% c(10, 20, 30), "not in lookuplist",
          .data$field_intact_fragment
        ),
      field_intact_fragment =
        ifelse(
          !is.na(.data$intact_fragment) & .data$intact_fragment == 30 &
            .data$plottype %in% c("CP", "CA"),
          "invalid for plottype",
          .data$field_intact_fragment
        ),
      field_intact_fragment =
        ifelse(
          !is.na(.data$intact_fragment) & .data$intact_fragment == 10 &
            !.data$plottype %in% c("CA", "BE"),
          "invalid for plottype",
          .data$field_intact_fragment
        ),
      field_alive_dead = ifelse(.data$alive_dead == 11, "tree alive", NA),
      field_decay_stage = ifelse(is.na(.data$decay_stage), "missing", NA),
      field_decay_stage =
        ifelse(
          !.data$decay_stage %in% c(10, 11, 12, 13, 14, 15, 16) &
            !is.na(.data$decay_stage),
          "not in lookuplist",
          .data$field_decay_stage),
      field_decay_stage =
        ifelse(
          .data$decay_stage == 16 & .data$alive_dead == 12 &
            !is.na(.data$decay_stage) & !is.na(.data$alive_dead),
          "tree not alive",
          .data$field_decay_stage)
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL
    ) %>%
    pivot_longer(
      cols =
        !c("plot_id", "lying_deadw_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")
  
  return(incorrect_deadwood)
}
