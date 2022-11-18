
sum_intervals <-
  function(var_min, var_max, transformation = NA, na_rm = FALSE) {
    
    if (na_rm) {
      var_min <- var_min[!is.na(var_min)]
      var_max <- var_max[!is.na(var_max)]
    }
    if (length(var_min) == 0) {
      return(data.frame(n_obs = NA, sum = NA, lci = NA, uci = NA))
    }
    
    if (!is.na(transformation) & transformation == "log") {
      value <- exp((log(var_min + 1e-10) + log(var_max)) / 2)
      variance <- ((log(var_max) - log(var_min + 1e-10)) / (2 * 1.96)) ^ 2
    } else {
      value <- (var_min + var_max) / 2
      variance <- ((var_max - var_min) / (2 * 1.96)) ^ 2
    }
    
    result <- data.frame(n_obs = length(value))
    result$sum <-
      ifelse(
        !is.na(transformation) & transformation == "log",
        log(sum(value)),
        sum(value)
      )
    variance <- sum(variance) / result$n_obs
    result$lci <- result$sum - 1.96 * sqrt(variance) / sqrt(result$n_obs)
    result$uci <- result$sum + 1.96 * sqrt(variance) / sqrt(result$n_obs)
    
    if (!is.na(transformation) & transformation == "log") {
      result$sum <- exp(result$sum)
      result$lci <- exp(result$lci)
      result$uci <- exp(result$uci)
    }
    
    if (is.na(result$uci) & result$sum == 0 & result$lci == 0) {
      result$uci <- 0
    }
    
    return(result)
  }




calculate_regeneration_plot <- function(data_regeneration) {
  by_plot <- data_regeneration %>%
    mutate(
      plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha),
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
    group_by(
      .data$plot_id, .data$year, .data$period, .data$subplot_id
    ) %>%
    summarise(
      number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
      established_interval =
        sum_intervals(
          var_min = .data$min_number_established_ha,
          var_max = .data$max_number_established_ha,
          transformation = "log", na_rm = TRUE
        ),
      seedlings_interval =
        sum_intervals(
          var_min = .data$min_number_seedlings_ha,
          var_max = .data$max_number_seedlings_ha,
          transformation = "log", na_rm = TRUE
        ),
      rubbing_damage_perc =
        sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
        sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),
      not_na_rubbing = sum(!is.na(.data$rubbing_damage_perc)),
      approx_nr_established_ha =
        sum(.data$approx_nr_established_ha, na.rm = TRUE),
      approx_nr_seedlings_ha = sum(.data$approx_nr_seedlings_ha, na.rm = TRUE),
      approx_nr_ha = sum(approx_nr_established_ha, approx_nr_seedlings_ha, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      mean_number_established_ha = .data$established_interval$sum,
      lci_number_established_ha = .data$established_interval$lci,
      uci_number_established_ha = .data$established_interval$uci,
      mean_number_seedlings_ha = .data$seedlings_interval$sum,
      lci_number_seedlings_ha = .data$seedlings_interval$lci,
      uci_number_seedlings_ha = .data$seedlings_interval$uci,
      rubbing_damage_perc =
        ifelse(
          .data$not_na_rubbing > 0 & .data$rubbing_damage_perc > 0,
          .data$rubbing_damage_perc,
          NA
        )
    ) %>%
    mutate(mean_number_established_ha =
             ifelse(is.na(.data$mean_number_established_ha)
                    & .data$mean_number_seedlings_ha > 0
                    , 0
                    , .data$mean_number_established_ha),
           lci_number_established_ha =
             ifelse(is.na(.data$lci_number_established_ha)
                    & .data$mean_number_seedlings_ha > 0
                    , 0
                    , .data$lci_number_established_ha),
           uci_number_established_ha =
             ifelse(is.na(.data$uci_number_established_ha)
                    & .data$mean_number_seedlings_ha > 0
                    , 0
                    , .data$uci_number_established_ha),
           mean_number_seedlings_ha =
             ifelse(is.na(.data$mean_number_seedlings_ha)
                    & .data$mean_number_established_ha > 0
                    , 0
                    , .data$mean_number_seedlings_ha),
           lci_number_seedlings_ha =
             ifelse(is.na(.data$lci_number_seedlings_ha)
                    & .data$mean_number_established_ha > 0
                    , 0
                    , .data$lci_number_seedlings_ha),
           uci_number_seedlings_ha =
             ifelse(is.na(.data$uci_number_seedlings_ha)
                    & .data$mean_number_established_ha > 0
                    , 0
                    , .data$uci_number_seedlings_ha),
           approx_nr_established_ha =
             ifelse(is.na(.data$approx_nr_established_ha)
                    & .data$approx_nr_seedlings_ha > 0
                    , 0
                    , .data$approx_nr_established_ha),
           approx_nr_seedlings_ha =
             ifelse(is.na(.data$approx_nr_seedlings_ha)
                    & .data$approx_nr_established_ha > 0
                    , 0
                    , .data$approx_nr_seedlings_ha)
    ) %>%
    select(
      -.data$established_interval, -.data$seedlings_interval,
      -.data$not_na_rubbing
    )
  
  return(by_plot)
}

