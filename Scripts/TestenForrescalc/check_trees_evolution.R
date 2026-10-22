source(here::here("scripts/Setup.R"))

library(DBI)
library(RSQLite)

database <- "C:/03_BR/2_Forrescalc/inst/example/testdb/mdb_bosres_test.sqlite"


con <- dbConnect(RSQLite::SQLite(), database)
con <- odbcConnectAccess2007(path_to_fieldmap_db)

# sqlite lukt niet, checken met echte db (fieldmap) - lukt wel 
# selectie voor periode 2

# check_trees_evolution <- function(database, forest_reserve = "all") {
  
  query_trees <-
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Trees.X_m AS x_m, Trees.Y_m AS y_m,
      Trees.ID AS tree_measure_id,
      Trees.StatusRemeasurement AS status_remeasurement,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage AS decay_stage,
      Trees.Remark AS remark,
      Trees.CommonRemark AS commonremark,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID as old_id
    FROM ((Plots INNER JOIN Trees_2eSet Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_2eSet pd ON Plots.ID = pd.IDPlots;"
  
  data_trees <- sqlQuery(con, query_trees) %>% 
    mutate(period = 2)
  
  incorrect_trees <- data_trees %>%
    filter(!is.na(.data$old_id)) %>%
    group_by(.data$plot_id, .data$period, .data$old_id) %>%
    mutate(n_records = n()) %>%
    ungroup() %>%
    filter(.data$n_records > 1) %>%
    transmute(
      .data$plot_id, .data$period, .data$tree_measure_id, .data$old_id,
      aberrant_field = "old_id",
      anomaly = paste0(.data$n_records, " times the same old_id")
    )
  
  
  data_trees2 <- load_data_dendrometry(database = path_to_fieldmap_db, extra_variables = TRUE)
  names(data_trees2); names(data_trees)
  data_trees2 <- data_trees2 %>% 
    rename(x_m = x_local, y_m = y_local
           , IUFROHght = iufro_hght, IUFROSocia = iufro_socia
           , IUFROVital = iufro_vital
           , decay_stage = decaystage) %>% 
    mutate(status_remeasurement = NA) 
  
  # %>% 
  #   select(all_of(colnames(data_trees)))
  # debug(create_unique_tree_id)
  
  # withCallingHandlers({
    data_trees3 <- create_unique_tree_id(data_trees2)
  # }, warning = function(w) {
  #   if (
  #     startsWith(
  #       conditionMessage(w),
  #       "Some records did not get a tree_id (NA) because the old_id was unknown in the previous period" #nolint: line_length_linter
  #     )
  #   ) invokeRestart("muffleWarning")
  # })
    
    test <- data_trees3 %>% filter(is.na(tree_id)) 
    # komt omdat ik maar één periode ingeladen heb
    
    
  
  # same XY but not the same tree_id?
  data_trees <- data_trees3 %>%
    mutate(
      tree_id_cop = sub("(^.*)_[a|b]$", "\\1", .data$tree_id)
    )
  for (i in min(data_trees$period):(max(data_trees$period) - 1)) {
    incorrect_trees <- incorrect_trees %>%
      mutate(
        period = as.character(.data$period),
        tree_measure_id = as.character(.data$tree_measure_id),
        old_id = as.character(.data$old_id)
      ) %>%
      bind_rows(
        data_trees %>%
          filter(.data$period == i) %>%
          inner_join(
            data_trees %>%
              filter(
                .data$period == i + 1,
                is.na(.data$old_id)
              ),
            by = c("plot_id", "species")
          ) %>%
          filter(.data$tree_id_cop.x != .data$tree_id_cop.y |
                   is.na(.data$tree_id.x) | is.na(.data$tree_id.y)) %>%
          mutate(
            location_shift =
              sqrt((.data$x_m.y - .data$x_m.x) ^ 2 +
                     (.data$y_m.y - .data$y_m.x) ^ 2)
          ) %>%
          filter(.data$location_shift < 0.2 &
                   (.data$status_remeasurement.y != 250 |
                      is.na(.data$status_remeasurement.y))) %>%
          transmute(
            .data$plot_id,
            period = paste(.data$period.x, .data$period.y, sep = "_"),
            tree_measure_id =
              paste(.data$tree_measure_id.x, .data$tree_measure_id.y,
                    sep = "_"),
            old_id = paste(.data$old_id.x, .data$old_id.y, sep = "_"),
            aberrant_field = "old_id",
            anomaly = "on same place but not coupled"
          )
      )
  }
  
  # is.na(tree_id) -> old_id unknown in the previous period
  incorrect_trees <- incorrect_trees %>%
    bind_rows(
      data_trees %>%
        filter(is.na(.data$tree_id)) %>%
        select("plot_id", "period", "tree_measure_id", "old_id") %>%
        mutate(
          aberrant_field = "old_id",
          anomaly = "unknown id",
          period = as.character(.data$period),
          tree_measure_id = as.character(.data$tree_measure_id),
          old_id = as.character(.data$old_id)
        )
    )
  data_trees <- data_trees %>%
    filter(!is.na(.data$tree_id)) %>%
    anti_join(
      incorrect_trees %>%
        filter(grepl("times the same old_id", .data$anomaly)) %>%
        mutate(
          period = as.integer(.data$period),
          tree_measure_id = as.integer(.data$tree_measure_id)
        ),
      by = c("plot_id", "period", "tree_measure_id")
    ) %>%
    distinct()
  
  trees_diff <-
    compare_periods_per_plot(
      data_trees %>%
        select(
          "period", "plot_id", "tree_id",
          "species", "alive_dead", "decay_stage", "ind_sht_cop",
          "x_m", "y_m", "dbh_mm",
          "height_m"
        ),
      measure_vars =
        c("species", "alive_dead", "decay_stage", "ind_sht_cop",
          "x_m", "y_m", "dbh_mm",
          "height_m")
    ) %>%
    mutate(
      location_shift = sqrt(.data$x_m_diff ^ 2 + .data$y_m_diff ^ 2)
    ) %>%
    left_join(
      data_trees %>%
        arrange(desc(.data$period)) %>%
        group_by(.data$tree_id) %>%
        summarise(
          species = .data$species[1],
          alive_dead = .data$alive_dead[1],
          ind_sht_cop = .data$ind_sht_cop[1]
        ) %>%
        ungroup(),
      by = "tree_id"
    )
  
  incorrect_tree_diff <- trees_diff %>%
    mutate(
      period_end = as.numeric(str_split_i(.data$period_diff, "_", 2)),
      field_species = ifelse(.data$species_diff != 0, "shifter", NA),
      field_alive_dead = ifelse(.data$alive_dead_diff == -1 &
                                  # coppice can be both: dead & alive
                                  # (noted on shoot level)
                                  .data$ind_sht_cop == 10 &
                                  .data$ind_sht_cop_diff == 0,
                                "zombie", NA),
      field_decay_stage =
        ifelse(
          !(.data$decay_stage_diff >= 0 & .data$decay_stage_diff <= 5) &
            .data$alive_dead_diff == 0 |
            .data$alive_dead_diff == 1 & !(.data$decay_stage_diff < 0 &
                                             .data$decay_stage_diff >= -6),
          "wrong shift",
          NA
        ),
      field_location_shift =
        ifelse(
          (.data$location_shift > 2 & .data$period_end >= 3) |
            (.data$location_shift > 3 & .data$period_end < 3),
          "walker",
          NA)
    ) %>%
    left_join(
      trees_diff %>%
        filter(
          !.data$species %in% c(51, 3),
          .data$alive_dead == 11,
          .data$ind_sht_cop %in% c(10, 11)
        ) %>%
        reframe(
          dbh_mm_diff = (boxplot(.data$dbh_mm_diff, plot = FALSE))$out
        ) %>%
        distinct() %>%
        mutate(field_dbh_mm = "outlier_diameter_total"),
      by = "dbh_mm_diff"
    ) %>%
    left_join(
      trees_diff %>%
        filter(
          .data$alive_dead == 11,
          .data$ind_sht_cop %in% c(10, 11)
        ) %>%
        group_by(.data$species) %>%
        reframe(
          dbh_mm_diff = (boxplot(.data$dbh_mm_diff, plot = FALSE))$out
        ) %>%
        ungroup() %>%
        distinct() %>%
        mutate(field_dbh_mm = "outlier_diameter_species"),
      by = c("dbh_mm_diff", "species")
    ) %>%
    mutate(
      field_dbh_mm =
        ifelse(
          is.na(.data$field_dbh_mm.x),
          ifelse(is.na(.data$field_dbh_mm.y), NA, .data$field_dbh_mm.y),
          ifelse(
            is.na(.data$field_dbh_mm.y), .data$field_dbh_mm.x,
            "outlier_diameter"
          )
        ),
      field_dbh_mm.x = NULL, field_dbh_mm.y = NULL
    )
  
  if (has_name(trees_diff, "height_m_diff")) {
    incorrect_tree_diff <- incorrect_tree_diff %>%
      left_join(
        trees_diff %>%
          filter(
            !.data$species %in% c(51, 3),
            .data$alive_dead == 11,
            .data$ind_sht_cop %in% c(10, 11)
          ) %>%
          reframe(
            height_m_diff = (boxplot(.data$height_m_diff, plot = FALSE))$out
          ) %>%
          distinct() %>%
          mutate(field_height_m = "outlier_height_total"),
        by = "height_m_diff"
      ) %>%
      left_join(
        trees_diff %>%
          filter(
            .data$alive_dead == 11,
            .data$ind_sht_cop %in% c(10, 11)
          ) %>%
          group_by(.data$species) %>%
          reframe(
            height_m_diff = (boxplot(.data$height_m_diff, plot = FALSE))$out
          ) %>%
          ungroup() %>%
          distinct() %>%
          mutate(field_height_m = "outlier_height_species"),
        by = c("height_m_diff", "species")
      ) %>%
      mutate(
        field_height_m =
          ifelse(
            is.na(.data$field_height_m.x),
            ifelse(is.na(.data$field_height_m.y), NA, .data$field_height_m.y),
            ifelse(
              is.na(.data$field_height_m.y), .data$field_height_m.x,
              "outlier_height"
            )
          ),
        field_height_m.x = NULL, field_height_m.y = NULL
      )
  }
  
  incorrect_tree_diff <- incorrect_tree_diff %>%
    select(
      "plot_id", "tree_id", period = "period_diff", "species",
      starts_with("field_"), "location_shift"
    ) %>%
    pivot_longer(
      cols = starts_with("field_"),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    transmute(
      .data$plot_id, .data$tree_id, .data$period,
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      .data$anomaly, .data$location_shift
    ) %>%
    mutate(
      period_start = as.numeric(substring(.data$period, 1, 1)),
      period_end = as.numeric(substring(.data$period, 3, 3))
    ) %>%
    left_join(
      data_trees %>%
        select(
          "plot_id", "tree_measure_id", "period", "tree_id",
          "species", "alive_dead", "decay_stage", "dbh_mm", "height_m"
        ),
      by = c("plot_id", "tree_id", "period_end" = "period")
    ) %>%
    left_join(
      data_trees %>%
        select(
          "plot_id", "tree_measure_id", "period", "tree_id",
          "species", "alive_dead", "decay_stage", "dbh_mm", "height_m"
        ),
      by = c("plot_id", "tree_id", "period_start" = "period"),
      suffix = c("_end", "_start")
    ) %>%
    mutate(
      period_end = NULL, period_start = NULL,
      tree_measure_id =
        paste(.data$tree_measure_id_start, .data$tree_measure_id_end,
              sep = "_"),
      tree_measure_id_end = NULL, tree_measure_id_start = NULL,
      species = paste(.data$species_start, .data$species_end, sep = "_"),
      species_end = NULL, species_start = NULL,
      alive_dead =
        paste(.data$alive_dead_start, .data$alive_dead_end, sep = "_"),
      alive_dead_end = NULL, alive_dead_start = NULL,
      decay_stage =
        paste(.data$decay_stage_start, .data$decay_stage_end, sep = "_"),
      decay_stage_end = NULL, decay_stage_start = NULL,
      dbh_mm = paste(.data$dbh_mm_start, .data$dbh_mm_end, sep = "_"),
      dbh_mm_end = NULL, dbh_mm_start = NULL,
      height_m = paste(.data$height_m_start, .data$height_m_end, sep = "_"),
      height_m_end = NULL, height_m_start = NULL
    ) %>%
    filter(
      !(.data$aberrant_field == "decay_stage" & .data$anomaly == "wrong shift" &
          .data$decay_stage == "17_17"),
      !(.data$aberrant_field == "decay_stage" & .data$anomaly == "wrong shift" &
          .data$decay_stage == "16_17"),
      !(.data$aberrant_field == "decay_stage" & .data$anomaly == "wrong shift" &
          .data$decay_stage == "17_16")
    )
  
  incorrect_tree_diff <- incorrect_tree_diff %>%
    mutate(
      location_shift = as.character(round(.data$location_shift, 2))
    ) %>%
    pivot_longer(
      cols =
        !c("plot_id", "tree_measure_id", "tree_id", "period", "aberrant_field",
           "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(
      .data$aberrant_field == .data$varname
    ) %>%
    select(-"varname") %>%
    distinct()
  
  if (nrow(incorrect_tree_diff) > 0) {
    incorrect_trees <- incorrect_trees %>%
      transmute(
        .data$plot_id, .data$period, .data$tree_measure_id,
        .data$aberrant_field,
        .data$anomaly,
        aberrant_value = .data$old_id
      ) %>%
      bind_rows(
        incorrect_tree_diff
      )
  }
  
  table(incorrect_trees$anomaly)
  
#   return(incorrect_trees)
# }