# functie------------
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
    
    return(result)
  }


# test ---------

by_plot <- data_regeneration %>%
  mutate(
    plotarea_ha = ifelse(.data$plottype == "CA", 0.01, .data$plotarea_ha),
    nr_established_ha =
      ifelse(.data$subcircle == "A2",
             .data$nr_of_regeneration / .data$plotarea_ha, NA),  
    min_number_established_ha =
      ifelse(.data$subcircle == "A2",
             .data$min_number_of_regeneration / .data$plotarea_ha, NA),
    max_number_established_ha =
      ifelse(.data$subcircle == "A2",
             .data$max_number_of_regeneration / .data$plotarea_ha, NA),
    nr_seedlings_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
             .data$nr_of_regeneration / .data$plotarea_ha, NA),
    min_number_seedlings_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
             .data$min_number_of_regeneration / .data$plotarea_ha, NA),
    max_number_seedlings_ha =
      ifelse(is.na(.data$subcircle) | .data$subcircle == "A1",
             .data$max_number_of_regeneration / .data$plotarea_ha, NA)
  ) 

by_plot2 <- by_plot %>%
  group_by(
    .data$plot_id, .data$year, .data$period, .data$subplot_id
  ) %>%
  summarise(
    number_of_tree_species = n_distinct(.data$species, na.rm = TRUE),
    nr_established_ha = sum(.data$nr_established_ha, na.rm = TRUE),
    not_na_established = sum(!is.na(.data$nr_established_ha)),
    established_interval =
      sum_intervals(
        var_min = .data$min_number_established_ha,
        var_max = .data$max_number_established_ha,
        transformation = "log", na_rm = TRUE   # nergens wordt = FALSE gebruikt !!!
      ),
    nr_seedlings_ha = sum(.data$nr_seedlings_ha, na.rm = TRUE),
    not_na_seedlings = sum(!is.na(.data$nr_seedlings_ha)),
    seedlings_interval =
      sum_intervals(
        var_min = .data$min_number_seedlings_ha,
        var_max = .data$max_number_seedlings_ha,
        transformation = "log", na_rm = TRUE
      ),
    rubbing_damage_perc =
      sum(.data$rubbing_damage_number, na.rm = TRUE) * 100 /
      sum(.data$nr_of_regeneration * (.data$subcircle == "A2"), na.rm = TRUE),  # subcircle A2 verwijst naar de hoogteklasses en kan dus ook bij CA gebruikt worden
    not_na_rubbing = sum(!is.na(.data$rubbing_damage_perc))
  ) %>%
  ungroup()

by_plot3 <- by_plot2 %>%
  mutate(
    nr_established_ha =
      ifelse(
        .data$not_na_established > 0 & .data$nr_established_ha > 0,
        .data$nr_established_ha,  #? nr_established kan toch ook 0 zijn??
        # ja maar dit vangt een foutje in vorige stap op: nr_of_reg = NA, maar nr_of_est wordt 0
        # OF TOCH NIET: enkel 0 als species ook 0 is, wordt niet bijgewerkt: 
        # ofwel allebei 0 (established én seedlings) ofwel geen van beide
        NA
      ),
    mean_number_established_ha = .data$established_interval$sum,
    lci_number_established_ha = .data$established_interval$lci,
    uci_number_established_ha = .data$established_interval$uci,
    nr_seedlings_ha =
      ifelse(
        .data$not_na_seedlings > 0 & .data$nr_seedlings_ha > 0,
        .data$nr_seedlings_ha,
        NA
      ),
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
  select(
    -.data$established_interval, -.data$not_na_established,
    -.data$seedlings_interval, -.data$not_na_seedlings, -.data$not_na_rubbing
  )

# testen ---------------

t <- by_plot3 %>% 
  filter(is.na(nr_seedlings_ha) & is.na(nr_established_ha))

t <- by_plot %>% 
  filter(nr_established_ha == 0 & !is.na(species))

t <- by_plot3 %>% 
  filter(is.na(mean_number_seedlings_ha) & is.na(mean_number_established_ha))
