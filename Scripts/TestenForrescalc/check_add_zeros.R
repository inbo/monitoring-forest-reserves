

dendro_by_plot_species <-   read_forresdat_table(tablename = "dendro_by_plot_species") %>%
    select(
      -plottype, -starts_with(c("survey_", "game", "r_", "dbh_min"))
      , -contains(c("diam_min", "core_area", "year", "length"))
      , -data_processed, -date_dendro, -vol_log_above40cm_m3_ha
      # -starts_with("game_")
    )

names(dendro_by_plot_species)
  add_zeros(
    dataset = dendro_by_plot_species,
    comb_vars = c("plot_id", "period", "species"),
    grouping_vars = c("forest_reserve")
  )
  add_zeros(
    dataset = dendro_by_plot_species,
    comb_vars = c("plot_id", "period", "species"),
    grouping_vars = c("forest_reserve"),
    remove_na_records_in_comb_vars = "species"
  )
  add_zeros(
    dataset = dendro_by_plot_species,
    comb_vars = c("plot_id", "period", "species"),
    grouping_vars = c("forest_reserve"),
    defaults_to_na = "stems_per_tree"
  )
  