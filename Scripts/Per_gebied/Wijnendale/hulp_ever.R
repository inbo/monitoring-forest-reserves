

t <- dendro_by_plot %>% filter(plot_id == 101 & period == 2)
view(t)


t <- dendro_by_plot_species %>% filter(plot_id == 101 & period == 2)
view(t)


stems0 <- data_stems %>% filter(plot_id %in% c(101, 401) & period == 2 &
                                       species == 16)
view(stems0)

stems <- data_stems_calc0 %>% filter(plot_id %in% c(101, 401) & period == 2 &
                                      species == 16)
view(stems)

height_model_ <- height_model %>% filter(plottype == "CP" & period == 2 &
                                       species == 16 & forest_reserve %in% c("Wijnendalebos", "Everzwijnbad"))
view(height_model_)


trees <- data_dendro_calc %>% filter(plot_id %in% c(101, 401) & period == 2 &
                                       species == 16)
view(trees)

#  stap voor stap functie
dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_shoots, height_model, plotinfo)

  data_stems <- compose_stem_data(data_dendro, data_shoots)
  data_stems_calc <- calc_variables_stem_level(data_stems, height_model)
  data_dendro_calc <- calc_variables_tree_level(data_dendro, data_stems_calc)
  data_deadwood <- calc_intact_deadwood(data_deadwood)
  by_plot <- calculate_dendro_plot(data_dendro_calc, data_deadwood, plotinfo)
  by_plot_species <-
    calculate_dendro_plot_species(data_dendro_calc, data_deadwood, plotinfo)
  by_decay_plot <-
    calculate_deadw_decay_plot(plotinfo, data_deadwood, data_dendro_calc)
  by_decay_plot_species <-
    calculate_deadw_decay_plot_species(plotinfo, data_deadwood,
                                       data_dendro_calc)
  by_diam_plot <- calculate_diam_plot(data_stems_calc, data_deadwood, plotinfo)
  by_diam_plot_species <-
    calculate_diam_plot_species(data_stems_calc, data_deadwood, plotinfo)
  
  return(
    list(
      dendro_by_plot = by_plot,
      dendro_by_plot_species = by_plot_species,
      dendro_by_diam_plot = by_diam_plot,
      dendro_by_diam_plot_species = by_diam_plot_species,
      deadw_by_decay_plot = by_decay_plot,
      deadw_by_decay_plot_species = by_decay_plot_species
    )
  )