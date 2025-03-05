

calculate_dendro_plot <- function(data_dendro_calc, data_deadwood) {
  
  
  by_plot <- data_dendro_calc %>%
    mutate(
      species_alive = ifelse(.data$alive_dead == 11, .data$species, NA)
    ) %>%
    group_by(
      .data$plot_id, .data$year, .data$period, .data$plottype
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species_alive, na.rm = TRUE),
      number_of_trees_ha = sum(.data$number_of_trees_alive_ha),
      stem_number_ha =
        sum((.data$alive_dead == 11) * .data$nr_of_stems / .data$plotarea_ha),
      basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
      basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
      vol_alive_m3_ha = sum(.data$vol_alive_m3_ha),
      vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
      vol_bole_alive_m3_ha = sum(.data$vol_bole_alive_m3_ha),
      vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
    )%>%
    ungroup() %>%
    full_join(
      data_deadwood %>%
        group_by(.data$plot_id, .data$year, .data$period) %>%
        summarise(
          vol_log_m3_ha = sum(.data$calc_volume_m3 / .data$plotarea_ha)
        ) %>%
        ungroup(),
      by = c("plot_id", "year", "period")
    ) %>%
    mutate(
      # hier alle variabelen obv staande bomen op '0' zetten, wannner ze
      # NA zijn en survey_trees uit plotdetails = TRUE, naar analogie met vol_logs
      # dat zijn: number_of_tree_species, number_of_trees_ha, stem_number_ha,
      # basal_area_alive_m2_ha, basal_area_dead_m2_ha,
      # vol_alive_m3_ha,vol_dead_standing_m3_ha, vol_bole_alive_m3_ha
      # @ els: ik denk dat jij dat beter kan programmeren,
      # ik zou per variabele een ifelse maken
      vol_log_m3_ha =
        ifelse(
          is.na(.data$vol_log_m3_ha) & .data$plottype %in% c("CP", "CA") &
            !is.na(.data$vol_alive_m3_ha),
          # !! soms wel staande bomen opgemeten, maar geen deadwood (liggend dood)
          # dan zou NA, nA moeten blijven
          # DUS: !is.na(.data$vol_alive_m3_ha) zou moeten vervangen worden door
          # info uit plotdetails (Survey_Deadwood_YN == 10)
          0, .data$vol_log_m3_ha
        ),
      plottype = NULL,
      vol_deadw_m3_ha = .data$vol_dead_standing_m3_ha + .data$vol_log_m3_ha,
      stems_per_tree = .data$stem_number_ha / .data$number_of_trees_ha
    )

  
  data_stems <- compose_stem_data(data_dendro, data_shoots)
  data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
  data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
  by_plot <- calculate_dendro_plot(data_dendro_calc, data_deadwood)
  
  
  calculate_dendrometry <- function(data_dendro, data_deadwood, data_shoots,
                                    height_model) {
    data_stems <- compose_stem_data(data_dendro, data_shoots)
    data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
    data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
    by_plot <- calculate_dendro_plot(data_dendro_calc, data_deadwood)
    by_plot_species <-
      calculate_dendro_plot_species(data_dendro_calc, data_deadwood)
    by_decay_plot <- calculate_logs_decay_plot(data_deadwood)
    by_decay_plot_species <-
      calculate_logs_decay_plot_species(data_deadwood)
    by_diam_plot <- calculate_diam_plot(data_stems_calc, data_deadwood)
    by_diam_plot_species <-
      calculate_diam_plot_species(data_stems_calc, data_deadwood)
    
    return(
        list(
        dendro_by_plot = by_plot,
        dendro_by_plot_species = by_plot_species,
        dendro_by_diam_plot = by_diam_plot,
        dendro_by_diam_plot_species = by_diam_plot_species,
        logs_by_decay_plot = by_decay_plot,
        logs_by_decay_plot_species = by_decay_plot_species
      )
    )
  }