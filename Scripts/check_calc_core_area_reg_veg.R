by_core_area_species <- data_herblayer %>%
  group_by(.data$plot_id, .data$period) %>%
  mutate(
    n_subplots = n_distinct(.data$subplot_id)
  ) %>%
  ungroup() %>%
  group_by(
    .data$plot_id, .data$year, .data$period, .data$species, .data$n_subplots,
  ) %>%
  summarise(
    number_of_subplots_with_vegetation = n_distinct(.data$subplot_id),
    perc_of_subplots = .data$number_of_subplots_with_vegetation * 100 / unique(.data$n_subplots),
    number_of_subplots_browsed =
      ifelse(
        all(is.na(.data$browse_index_id)),
        NA,
        sum(!is.na(.data$browse_index_id) &
              .data$browse_index_id %in% c(110, 120))
      ),
    number_of_subplots_seriously_browsed =
      ifelse(
        all(is.na(.data$browse_index_id)),
        NA,
        sum(!is.na(.data$browse_index_id) & .data$browse_index_id == 120)
      ),
    perc_of_subplots_browsed =
      .data$number_of_subplots_browsed * 100 / .data$number_of_subplots_with_vegetation,
    perc_of_subplots_seriously_browsed =
      .data$number_of_subplots_seriously_browsed * 100 / .data$number_of_subplots_with_vegetation,
    mean_coverage_class_average_perc = mean(.data$coverage_class_average_perc)
  ) %>%
  ungroup()


# reg ------


by_plot_species_reg <- data_regeneration %>%
  group_by(.data$plot_id, .data$period) %>%
  mutate(
    n_subplots = n_distinct(.data$subplot_id),
    min_number_established_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
             .data$min_number_of_regeneration / .data$plotarea_ha, NA),
    max_number_established_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
             .data$max_number_of_regeneration / .data$plotarea_ha, NA),
    min_number_seedlings_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
             .data$min_number_of_regeneration / .data$plotarea_ha, NA),
    max_number_seedlings_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
             .data$max_number_of_regeneration / .data$plotarea_ha, NA),
    approx_nr_established_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A2",
             .data$approx_nr_regeneration / .data$plotarea_ha, NA),
    approx_nr_seedlings_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
             .data$approx_nr_regeneration / .data$plotarea_ha, NA)
  ) %>%
  ungroup() %>%
  group_by(
    .data$plot_id, .data$year, .data$period, .data$species, .data$n_subplots
  ) %>%
  summarise(
    nr_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
    perc_subplots_with_regeneration =
      .data$nr_of_subplots_with_regeneration * 100 / unique(.data$n_subplots)
      ) %>%
  ungroup() 
