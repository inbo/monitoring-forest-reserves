# test load_data_shoots: extra var alive_dead_shoots
data_stems_calc <- stems_calc
names(data_dendro1)
names(stems_calc)

calc_variables_tree_level_AL <-
  function(data_dendro, data_stems_calc) {
    
    attributes <-
      compare_attributes(
        data_dendro, data_stems_calc, "data_dendro", "data_stems_calc"
      )
    data_dendro1 <- data_dendro %>%
      select(
        -"dbh_mm", -"nr_of_stems", -"calc_height_fm",
        -"intact_snag", -"decaystage", -"alive_dead"
      ) %>%
      left_join(
        data_stems_calc %>%
          group_by(.data$plot_id, .data$tree_measure_id, .data$period
                   , .data$alive_dead) %>%
          summarise(
            nr_of_stems = n(),
            decaystage =
              as.integer(
                round(
                  sum(.data$decaystage * .data$dbh_mm ^ 2 / 4) /
                    sum(.data$dbh_mm ^ 2 / 4)
                )
              ),
            intact_snag = max(.data$intact_snag),
            calc_height_m = sum(.data$calc_height_m * .data$dbh_mm ^ 2 / 4) /
              sum(.data$dbh_mm ^ 2 / 4),
            dbh_mm = round(sqrt(sum(.data$dbh_mm ^ 2) / n())),
            basal_area_m2 = sum(.data$basal_area_m2),
            vol_bole_m3 = sum(.data$vol_bole_m3),
            vol_crown_m3 = sum(.data$vol_crown_m3),
            vol_tot_m3 = sum(.data$vol_tot_m3),
            # RESULTS PER HECTARE
            basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
            basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
            vol_alive_m3_ha = sum(.data$vol_alive_m3_ha),
            vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
            vol_bole_alive_m3_ha = sum(.data$vol_bole_alive_m3_ha),
            vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
          ) %>%
          ungroup(),
        by = c("plot_id", "tree_measure_id", "period")
      ) %>%
      mutate(
        individual = (.data$ind_sht_cop == 10 | .data$ind_sht_cop == 12)
      ) %>%
      mutate(number_of_trees_alive_ha =
               ifelse(
                 .data$alive_dead == 11,
                 .data$individual / .data$plotarea_ha,
                 0
               ),
             number_of_trees_dead_ha =
               ifelse(
                 .data$alive_dead == 12,
                 .data$individual / .data$plotarea_ha,
                 0
               )
      ) %>%
      select(-"individual")
    
    attr(data_dendro1, "database") <- attributes[["attr_database"]]
    attr(data_dendro1, "forrescalc") <- attributes[["attr_forrescalc"]]
    attr(data_dendro1, "heightmodels") <- attr(data_stems_calc, "heightmodels")
    
    return(data_dendro1)
  }
