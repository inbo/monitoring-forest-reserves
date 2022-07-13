

query_total_cover <-
  "SELECT tc.ID AS id,
              tc.Value1 AS cover_interval,
              tc.Value2 AS mean_cover
        FROM qtotalCover tc"

con <- odbcConnectAccess2007(path_to_fieldmap)
total_cover <- sqlQuery(con, query_total_cover, stringsAsFactors = FALSE) %>%
  mutate(
    min_cover = gsub("^(\\d+) - (\\d+) %", "\\1", .data$cover_interval),
    max_cover = gsub("^(\\d+) - (\\d+) %", "\\2", .data$cover_interval),
    min_cover = ifelse(.data$min_cover == "< 1%", 0, .data$min_cover),
    max_cover = ifelse(.data$max_cover == "< 1%", 1, .data$max_cover),
    min_cover = ifelse(.data$min_cover == "nvt", NA, .data$min_cover),
    max_cover = ifelse(.data$max_cover == "nvt", NA, .data$max_cover),
    mean_cover = str_replace(string = .data$mean_cover, pattern = ",", replacement = "."),
      #replace , by .
    mean_cover = as.numeric(.data$mean_cover) * 100,
    min_cover = as.numeric(.data$min_cover),
    max_cover = as.numeric(.data$max_cover)
  )
odbcClose(con)


#----------

data_vegetation <-
  query_database(database, query_vegetation, selection = selection) %>%
  mutate(
    year = year(.data$date_vegetation),
    year = ifelse(is.na(.data$year), .data$year_record, .data$year),
    plotarea_ha =
      ifelse(
        .data$plottype == 20,
        0.16 * 0.16,
        NA
      ),
    plotarea_ha =
      ifelse(
        .data$plottype == 30,
        (.data$length_core_area_m * .data$width_core_area_m) / 10000,
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
      )
  ) %>%
  left_join(total_cover, by = c("total_moss_cover_id" = "id")) %>%
  rename(
    moss_cover_interval = .data$cover_interval,
    moss_cover_min = .data$min_cover,
    moss_cover_max = .data$max_cover
  ) %>%
  left_join(total_cover, by = c("total_herb_cover_id" = "id")) %>%
  rename(
    herb_cover_interval = .data$cover_interval,
    herb_cover_min = .data$min_cover,
    herb_cover_max = .data$max_cover
  ) %>%
  left_join(total_cover, by = c("total_shrub_cover_id" = "id")) %>%
  rename(
    shrub_cover_interval = .data$cover_interval,
    shrub_cover_min = .data$min_cover,
    shrub_cover_max = .data$max_cover
  ) %>%
  left_join(total_cover, by = c("total_tree_cover_id" = "id")) %>%
  rename(
    tree_cover_interval = .data$cover_interval,
    tree_cover_min = .data$min_cover,
    tree_cover_max = .data$max_cover
  ) %>%
  left_join(total_cover, by = c("total_waterlayer_cover_id" = "id")) %>%
  rename(
    waterlayer_cover_interval = .data$cover_interval,
    waterlayer_cover_min = .data$min_cover,
    waterlayer_cover_max = .data$max_cover
  ) %>%
  left_join(total_cover, by = c("total_soildisturbance_game_id" = "id")) %>%
  rename(
    soildisturbance_game_cover_interval = .data$cover_interval,
    soildisturbance_game_cover_min = .data$min_cover,
    soildisturbance_game_cover_max = .data$max_cover
  )

