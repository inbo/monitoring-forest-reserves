# compose stem data--------

data_dendro_relevant <- data_dendro %>%
  select(
    -.data$vol_tot_m3, -.data$vol_stem_m3, -.data$vol_crown_m3,
    -.data$basal_area_m2, -.data$tree_number,
    -.data$individual, -.data$basal_area_alive_m2_ha,
    -.data$basal_area_snag_m2_ha, -.data$volume_alive_m3_ha,
    -.data$volume_snag_m3_ha, -.data$volume_stem_alive_m3_ha,
    -.data$volume_stem_snag_m3_ha, -.data$dbh_class_5cm
  )

stem_data_trees <- data_dendro_relevant %>%
  filter(.data$ind_sht_cop != 12)

stem_data_shoots <- data_dendro_relevant %>%
  select(-.data$dbh_mm, -.data$height_m, -.data$decaystage) %>%
  filter(.data$ind_sht_cop == 12) %>%
  inner_join(data_shoots, by = c("plot_id", "tree_measure_id", "period"))


stem_data <- rbind(stem_data_trees, stem_data_shoots)

stem_data <- compose_stem_data(data_dendro, data_shoots)

nrow(stem_data)
table(stem_data$period)
# 10054 shoots + 12038 trees geen hh = 22092 (controle access)


# calculate diam_plot ----
by_diam_plot_test <- stem_data %>%
  mutate(uniek = as.integer("1")) %>%
  group_by(
    .data$plot_id, .data$forest_reserve, .data$year, .data$period,
    .data$dbh_class_5cm, .data$alive_dead
  ) %>%
  summarise(
    stem_number_ha = round(sum(.data$uniek / .data$plotarea_ha)),
    basal_area_m2_ha = sum(.data$basal_area_m2 / .data$plotarea_ha)
  )

# acces stem_number
((32*4) + (25*4))/0.1018
# komt overeen met IA: plot 1201 2238 stems/ha

dendro_by_diam_plot %>% filter(plot_id == 1201 & period == 1)

by_diam_plot_test %>% filter(plot_id == 1201 & period == 1)


