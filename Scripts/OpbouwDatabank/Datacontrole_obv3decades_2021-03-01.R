
# LATEN LOPEN obv nieuwe db Peter
###################################


# Aangezien package nog niet bijgewerkt is naar drie decades data inladen mbv
# temp_load_data_123eSET.Rmd

source(here::here("Scripts/Validatie/temp_load_data_123eSET.Rmd"))
source("C:/3BR/1_DataVerwerkingBR/Scripts/Validatie/temp_load_data_123eSET.Rmd")
source("C:/3BR/1_DataVerwerkingBR/Scripts/OpbouwDatabank/tmp_calc_functiesNGV_2021-03-02.R")
# Error in source("C:/3BR/1_DataVerwerkingBR/Scripts/Validatie/temp_load_data_123eSET.Rmd") :
#   attempt to use zero-length variable name
# !! Rmd scripts niet makkelijk te sourcen => tijdelijk gewoon gaan runnen in file zelf!!

# gerund - OK


# bevat reeds:
# rm(list=ls())
# # libraries & invoergegevens
# source("Scripts/Setup_ForresCalc.R")




### Lookuplijsten : input from access fieldmap ----

# Enkel indie er iets gewijzigd is ... (recent nog weggecshreven (zie temp 21-02-22))


#### Plotinfo: plottype, naam forest_reserve en info over survey en data al dan niet processed-----
# plotinfo <- load_plotinfo(database = path_to_fieldmap)

save_results_git(
  results = list(plotinfo = plotinfo),
  repo_path = path_to_git_forresdat,
  strict= FALSE
)

save_results_access (
  results = list(plotinfo = plotinfo),
  database = path_to_analysis_set_db,
  remove_tables = TRUE
)

save_results_csv(
    results = list(plotinfo = plotinfo),
    output_dir = path_to_plotlevel_csv
  )


### Data_dendro berekenen ----
  # data_dendro <-
  #   load_data_dendrometry(
  #     database = path_to_fieldmap
  #   )
  #
  # data_deadwood <-
  #   load_data_deadwood(
  #     database = path_to_fieldmap
  #   )
  # data_deadwood %>% filter((plot_id == 11000)) %>% nrow()  # klopt, geen deadwood gemeten in Kersselaerspleyn KV
  #
  # data_shoots <-
  #   load_data_shoots(
  #     database = path_to_fieldmap
  #   )

data_stems  <- compose_stem_data(data_dendro, data_shoots)


# EERST calculate-functies bijwerken met automatisch N, G, V berekenen in R ------------
# zie "temp_calc_functiesNGV_2021-03-02.R"

dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_stems)
table(data_dendro$forest_reserve,data_dendro$dbh_min_a4)
# tijdelijk alle functies gekopieerd met extra variabelen : zie "tmp_functies_extra_variabelen_2021-03-01.R"
# !! blijkt overbodig: tresholds zitten in de load_data_functies
t <- dendro[["dendro_by_plot"]] %>% filter(plot_id == 11000)
# calculate_functies: daar bedoeling om mbv survey_trees/deadw en data_processed zero-values op te nemen, gevraagd aan Els (issue 64)
# in load_functies survey_trees en data_processed gebruiken om enkel relevenate data in te laden (mbv extra argument: TRUE or FALSe)
# ook aan Els gevraagd (issue 58)

# tijdelijk nog ambtachterlijk ....
# WARNING: if plotid 11000 in data, replace (lying) deadwood results for plot_id = 11000 by "NA"
# (in plot KV Kersselaerspleyn (plot 11000) no lying deadwood is meausured)
dendro[["dendro_by_plot"]] <- dendro[["dendro_by_plot"]] %>%
  mutate(
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha),
    volume_deadwood_m3_ha  =
      ifelse(plot_id == 11000 & is.na(volume_log_m3_ha), NA, volume_deadwood_m3_ha)
  )
dendro[["dendro_by_plot_species"]] <- dendro[["dendro_by_plot_species"]] %>%
  mutate(
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha),
    volume_deadwood_m3_ha  =
      ifelse(plot_id == 11000 & is.na(volume_log_m3_ha), NA, volume_deadwood_m3_ha)
  )
dendro[["dendro_by_diam_plot"]] <- dendro[["dendro_by_diam_plot"]] %>%
  mutate(
    log_number_ha  =
      ifelse(plot_id == 11000 & log_number_ha == 0, NA, log_number_ha),
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha)
  )
dendro[["dendro_by_diam_plot_species"]] <- dendro[["dendro_by_diam_plot_species"]] %>%
  mutate(
    log_number_ha  =
      ifelse(plot_id == 11000 & log_number_ha == 0, NA, log_number_ha),
    volume_log_m3_ha  =
      ifelse(plot_id == 11000 & volume_log_m3_ha == 0, NA, volume_log_m3_ha)
  )

# aanmaak & controle van de subfiles --------
dendro_by_plot <- dendro[["dendro_by_plot"]]
dendro_by_plot_species <- dendro[["dendro_by_plot_species"]]
dendro_by_diam_plot <- dendro[["dendro_by_diam_plot"]]
dendro_by_diam_plot_species <- dendro[["dendro_by_diam_plot_species"]]
logs_by_decay_plot <- dendro[["logs_by_decay_plot"]]
logs_by_decay_plot_species <- dendro[["logs_by_decay_plot_species"]]

dendro_by_plot_ <- dendro_by_plot %>%
  left_join(plotinfo)

table(dendro_by_plot_$forest_reserve, dendro_by_plot_$period)
table(dendro_by_plot_$forest_reserve, dendro_by_plot_$survey_number)

dendro_3de_decade <- dendro_by_plot_ %>%
  filter(period == 3)
table(dendro_3de_decade$forest_reserve, dendro_3de_decade$survey_number)

# voor Peter
dendro_3de_decade <- dendro_by_plot_ %>%
  filter(period == 3 & survey_number == 2)
write_csv2(dendro_3de_decade, "Output/Validatie/3de_decade_2de_survey.csv")



# incorporeren van survey_trees en data_processed --------
# dit zou best in de calculate_functies geïncorporeerd worden, eerder dan bij de load_data
# zodat de forresdat-files enkel processed data bevatten

# enkel mogelijk voor de meer algemene
# bij de diam_species_decay_outputs: mbv functie "add_zeros"

      # calculate_dendro_plot: bevat deadwod én trees
      # calc_reg en calc_veg: daar wordt dat reeds opgevangen doordat er een subid moet aangemaakt worden

      # op termijn: data_processed <> 10 wegfilteren bij load_data_funcies : zie isssue, gevraagd aan Els


# nu tijdelijk mbv plotinfo
# dendro[["dendro_by_plot"]]

dendro_by_plot_tmp <- dendro_by_plot %>%
  full_join(plotinfo %>% filter(survey_trees == 10 & data_processed == 10), by = c("plot_id", "period"))

# voor Peter
t <- dendro_by_plot_tmp %>%
  anti_join(dendro_by_plot, by = c("plot_id", "period"))
write_csv2(t, "Output/Validatie/plots_no_trees_survey_yes.csv")

t2 <- dendro_by_plot_tmp %>%
  anti_join(plotinfo, by = c("plot_id", "period"))

t3 <- dendro_by_plot_tmp %>% filter(is.na(year.y))
# ik koppel enkel de info van de info waar data_processsed = 10 => te beperkt!!
# filter pas na de koppeling erop zetten!!
# TOCH NIET, want dan overkill: alle nog niet pogemeten plots er ook bij

dendro_by_plot_tmp <- dendro_by_plot %>%
  full_join(plotinfo %>% filter(survey_trees == 10 & data_processed == 10), by = c("plot_id", "period")) %>%
  mutate(number_of_tree_species = ifelse(is.na(number_of_tree_species) & survey_trees == 10 & data_processed == 10, 0, number_of_tree_species),
         number_of_trees_ha = ifelse(is.na(number_of_trees_ha) & survey_trees == 10 & data_processed == 10, 0, number_of_trees_ha),
         stem_number_ha = ifelse(is.na(stem_number_ha) & survey_trees == 10 & data_processed == 10, 0, stem_number_ha),
         basal_area_alive_m2_ha = ifelse(is.na(basal_area_alive_m2_ha) & survey_trees == 10 & data_processed == 10, 0, basal_area_alive_m2_ha),
         basal_area_snag_m2_ha = ifelse(is.na(basal_area_snag_m2_ha) & survey_trees == 10 & data_processed == 10, 0, basal_area_snag_m2_ha),
         volume_alive_m3_ha = ifelse(is.na(volume_alive_m3_ha) & survey_trees == 10 & data_processed == 10, 0, volume_alive_m3_ha),
         volume_snag_m3_ha = ifelse(is.na(volume_snag_m3_ha) & survey_trees == 10 & data_processed == 10, 0, volume_snag_m3_ha),
         volume_stem_alive_m3_ha = ifelse(is.na(volume_stem_alive_m3_ha) & survey_trees == 10 & data_processed == 10, 0, volume_stem_alive_m3_ha),
         volume_stem_snag_m3_ha = ifelse(is.na(volume_stem_snag_m3_ha) & survey_trees == 10 & data_processed == 10, 0, volume_stem_snag_m3_ha),
         volume_log_m3_ha = ifelse(is.na(volume_log_m3_ha) & survey_deadw == 10 & data_processed == 10, 0, volume_log_m3_ha),
         volume_deadwood_m3_ha = ifelse(is.na(volume_deadwood_m3_ha) & survey_deadw == 10 & data_processed == 10, 0, volume_deadwood_m3_ha),
         stems_per_tree = ifelse(is.na(stems_per_tree) & survey_trees == 10 & data_processed == 10, 0, stems_per_tree),
         year = ifelse(is.na(year.x), year.y, year.x)
  ) %>%
  select(-year.x, -year.y) %>%
  filter(data_processed == 10)

names(dendro_by_plot_tmp)
summary(dendro_by_plot_tmp)

# KLOPPEN DE NA's???
t4 <- dendro_by_plot_tmp %>%
  filter(is.na(number_of_trees_ha) & !is.na(number_of_tree_species))
# OK: number_of_tree_species wordt in R berekend => ingevuld zodra er bomen zijn
# number_of_trees_ha  NA want nog niet processed

t5 <- dendro_by_plot_tmp %>%
  filter(!is.na(data_processed) & is.na(number_of_tree_species))
# OK! (meestal is data_processed = NA, wanneer survey_trees = 20; die ene is een waarbij survey_trees = 10)

t6 <- dendro_by_plot_tmp %>%
  filter(is.na(stem_number_ha) & !is.na(number_of_tree_species))

t7 <- dendro_by_plot_tmp %>%
  filter(is.na(volume_deadwood_m3_ha ) & !is.na(volume_log_m3_ha))
# OK
# 2 extra met volume logs dan met volume_deadw: dat is omdat vol_snags niet gekend, want niet processed (plots 424 en 426)

# save
save_results_git(
  results = dendro,
  repo_path = path_to_git_forresdat
)

save_results_access (
  results = dendro,
  database = path_to_analysis_set_db,
  remove_tables = TRUE
  )


# volgende functie heb ik zelf geschreven, zit niet in het package!!!
# enkel in nood te gebruiken als "save_results_git" niet werkt
save_results_csv (
    results = dendro,
    output_dir = path_to_plotlevel_csv
    )


### Status_tree berekenen ----
# bevat een unieke tree_id per boom (die constant blijft doorheen de tijd),
# verschillend van de tree_measure_id uit data_dendro
# één record per boom en per periode
# om één record per boom te verkrijgen met ifo uit beide periodes,
# dient functie "make_table_wide" gebruikt te worden
status_tree <- create_overview_status(data_dendro = data_dendro)
# Deze tabel wordt NIET bewaard in forresdat (daar enkel geaggregeerde resultaten op plotniveau)

status_tree_long <- status_tree %>%
  select(plot_id, species, tree_id, period, dbh_mm, alive_dead)

status_tree_wide <- make_table_wide(table_long = status_tree_long,
                                    column_to_repeat = "period",
                                    columns_for_comparison = c("dbh_mm", "alive_dead"))


### Data_regeneration berekenen ----
data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )

regeneration <- calculate_regeneration(data_regeneration)

save_results_git(
  results = regeneration,
  repo_path = path_to_git_forresdat
)

save_results_access(
    results = regeneration,
    database = path_to_analysis_set_db,
    remove_tables = TRUE
  )

save_results_csv (
  results = regeneration,
  output_dir = path_to_plotlevel_csv
)


### Data_vegetation berekenen ----
data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap
  )
data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap
  )

vegetation <- calculate_vegetation(data_vegetation, data_herblayer)

save_results_git(
  results = vegetation,
  repo_path = path_to_git_forresdat
)

save_results_access(
  results = vegetation,
  database = path_to_analysis_set_db,
  remove_tables = TRUE
)

save_results_csv (
  results = vegetation,
  output_dir = path_to_plotlevel_csv
)


# ook herblayer naar forresdat wegschrijven
str(data_herblayer)

herblayer <- data_herblayer %>%
  select(plot_id, subplot_id, period, year, species, coverage_class_average_perc, browse_index_id, plotarea_ha, date_vegetation, deviating_date) %>%
  mutate(date_vegetation = as.Date(date_vegetation), deviating_date = as.Date(deviating_date)) %>%
  arrange(deviating_date, date_vegetation)
# browse_index_id nog te koppelen aan qBrowseIndex
# arrange nodig zodat er een datum ingeyuld staat in eerste regel van de datumvelden, anders denkt access dat het een string of zo is (NA)
# en dan plots probleem als er een datum blijkt te staan in één van de volgende regels


# save_results_xxxx: gemaakt om list weg te schrijven
# hier maar één dataframe

herblayer_list <- list(herblayer_by_plot = herblayer)

# summary(herblayer)

save_results_git(
  results = herblayer_list,
  repo_path = path_to_git_forresdat
)


# save to access nog steeds niet opgelost, is alsof access date niet herkent
# => manueel gedaan (csv opgeslagen als xlsx en dan geïmporteerd; csv gaf zelfd eproblemen als access)
save_results_access(
  results = herblayer_list,
  database = path_to_analysis_set_db,
  remove_tables = TRUE
)
# probleem met datumvelden


save_results_csv (
  results = herblayer_list,
  output_dir = path_to_plotlevel_csv
)


# na update van forresdat, project "forresdat" openen en alles naar de cloud pushen

