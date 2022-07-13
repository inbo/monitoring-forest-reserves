
test <- data_regeneration %>% 
  filter(forest_reserve == "Sevendonck A") %>% 
  group_by(
    .data$forest_reserve, .data$plot_id, .data$year, .data$period
  ) %>%
  summarise(n_subplots = n_distinct(.data$subplot_id)) %>% 
  ungroup()
    
    
    
by_plot_species <- data_regeneration %>%
  left_join(
    data_regeneration %>% 
      group_by(.data$plot_id, .data$period
               ) %>%
      summarise(n_subplots = n_distinct(.data$subplot_id)
                ) %>% 
      ungroup() %>% 
      select(.data$plot_id, .data$period, .data$n_subplots)
    ) %>% 
  # mutate(
  #   n_subplots = n_distinct(.data$subplot_id)
  # ) %>%
  group_by(
    .data$forest_reserve, .data$plot_id, .data$year, .data$period, .data$height_class, .data$species
  ) %>%
  summarise(
    mean_number_of_trees_ha = sum(.data$mean_number_of_trees / .data$plotarea_ha),
    min_number_of_trees_ha = sum(.data$min_number_of_trees / .data$plotarea_ha),
    max_number_of_trees_ha = sum(.data$max_number_of_trees / .data$plotarea_ha),
    number_of_subplots_with_regeneration = n_distinct(.data$subplot_id),
    perc_subplots_with_regeneration =
      .data$number_of_subplots_with_regeneration * 100 / unique(.data$n_subplots),
    rubbing_damage_perc = sum(.data$rubbing_damage_number) * 100 / sum(.data$reg_number)
  ) %>%
  ungroup()


data_regeneration %>% filter(forest_reserve == "Sevendonck A") %>% 
  select(plot_id, subplot_id, species, period)


range(data_regeneration$subplot_id)
range(by_plot_species$n_subplots)
summary(by_plot_species %>% filter(plot_id == 141100))

t <- by_plot_species %>% filter(forest_reserve == "Sevendonck A") 
