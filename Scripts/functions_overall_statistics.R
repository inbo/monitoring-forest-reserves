
##########################################################################
#### Functies ikv dataverwerking op bosreservaatniveau (cirkelplots) ----
#########################################################################

# OPGEPAST ------

# Onderstaande functies enkel van toepassing op cirkelplots van bosreservaten.
# 
# Mocht ik ooit statistieken willen van andere onderzoekssites, moet ik checken of functies voldoen (vooral mbt NA's)


# dendro en reg-functies enkel van toepassing op forest plots


# FUNCTIES -------------------------

#' get dataframe with circular plots located in open area
#' 
#' This function creates a dataframe with all the circular plots 
#' (only processed data) located in an open area.
#' The dataframe is derived from the data saved in the forresdat-folder 
#' (`dendro_by_plot` and `regeneration_by_plot`)
#'
#' @param repo_path path_name of forresdat-data. 
#' Default is 'path_to_git_forresdat', as specified in 'Setup.R'.
#'
#' @return dataframe with all circular plots located in open area
#'
#' @examples
#' \dontrun{
#' open_area <- get_open_area()
#' }
#'
get_open_area <- function(repo_path = path_to_git_forresdat){
  dendro_by_plot <- read_forresdat("dendro_by_plot", repo_path)
  regeneration_by_plot <- read_forresdat("regeneration_by_plot", repo_path)
  open_area <- dendro_by_plot %>% 
    filter(number_of_tree_species == 0) %>% # 14 plots (13 plots ename & plot 475)
    filter(vol_deadw_m3_ha == 0) %>%  # dan valt plot 475 weg => is bosplot, maar op dat moment zonder bomen/verjonging
    left_join(regeneration_by_plot) %>% 
    filter((approx_nr_established_ha == 0 & approx_nr_seedlings_ha == 0) 
           | (is.na(approx_nr_established_ha) & is.na(approx_nr_seedlings_ha))
    ) %>% 
    select(forest_reserve, plot_id, period, number_of_tree_species)
  match.fun(mean) 
  
  open_area
}


#' get dataframe with circular plots located in forested area
#'
#' This function creates a dataframe with all the circular plots 
#' (only processed data) located in forested area.
#' The dataframe is derived from the data saved in the forresdat-folder  
#' (`dendro_by_plot` and `regeneration_by_plot`)
#' 
#' @inheritParams get_open_area
#' 
#' @return dataframe with all circular plots located in forested area
#'
#' @examples
#' \dontrun{
#' forest_plot <- get_forest_plot()
#' }
#'
get_forest_plot <- function(repo_path = path_to_git_forresdat){
  dendro_by_plot <- read_forresdat("dendro_by_plot", repo_path)

  open_area <- get_open_area()
  
  forest_plot <- dendro_by_plot %>% 
    filter(plottype == "CP") %>% 
    select(forest_reserve, plot_id, period, number_of_tree_species
           , survey_trees, survey_deadw) %>% 
    anti_join(open_area)
  
  forest_plot
}


#' differentiate between managed and unmanaged part of forest
#' 
#' This function is developed especially for Kluisbos, which has an unmanaged 
#' and managed part (containing 55 plots with 'selective thinning forestry' 
#' and 6 plots with 'Non-intervention').
#' The function renames the managed part of the 'forest_reserve': 
#' e.g. 'Kluisbos' becomes 'Kluisbos_managed' and 'Kluisbos_managed_non_intervention'.
#' The input dataset can only contain circular plots and has to include the 
#' variables `plot_id` & `forest_reserve`.
#' 
#' @param dataset name of the dataframe where 'forest_reserve' should be split up
#' in managed, managed_non_intervention and unmanaged
#'  
#' @return same dataset as the input, with forest_reserve 'Kluisbos' split up in 
#' 'Kluisbos', 'Kluisbos_managed' and 'Kluisbos_managed_non_intervention'
#' (resp. 67, 55 and 6 plots). 
#'
#' @examples
#' \dontrun{
#' dendro_by_plot <- read_forresdat(name_dataset, repo_path) %>% 
#' filter(plottype == "CP")
#' dendro_by_plot <- differentiate_managed_plots(dataset = dendro_by_plot)
#' }
#'
differentiate_managed_plots <- function(dataset){

  con <- odbcConnectAccess2007(path_to_strata_db)
  management <- sqlFetch(con, "strata_remaining", stringsAsFactors = FALSE) %>% 
    select(plot_id, Management, Management_type)
  odbcClose(con)
  
  dataset <- dataset %>% 
    left_join(management) %>% 
    # mutate(forest_reserve = ifelse(Management == "managed forest"
    #                                , paste0(forest_reserve, "_managed")
    #                                , forest_reserve)
  # OF
    mutate(forest_reserve = 
             ifelse(Management == "managed forest"
                    , ifelse(Management_type == "Non-intervention"
                             , paste0(forest_reserve, "_managed_non_intervention")
                             , paste0(forest_reserve, "_managed")
                             )
                    , forest_reserve)) %>% 
    select(-contains("manag"))

  dataset
}


#' get year range per forest reserve, based on dendro_by_plot - TEMPORARY
#' 
#' This is a temporary function that replaces `include_year_range = TRUE`, in 
#' the function `create_statistics()`.
#' Input dataset is output from the function `create_statistics()`
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics with extra info on year_range
#'
#' @examples
#' \dontrun{
#' dendro_by_plot <- read_forresdat(name_dataset, repo_path) %>% 
#' filter(plottype == "CP")
#' resultaat <- create_statistics(
#'   dataset = dendro_by_plot,
#'   level = c("forest_reserve", "period"),
#'   variables = "vol_alive_m3_ha"
#' )
#' resultaat <- get_year_range(resultaat)
#' }
#'
#' @importFrom forrescalc read_forresdat create_statistics
#'
get_year_range <- function(dataset, repo_path = path_to_git_forresdat){
  plotinfo <- read_forresdat("plotinfo", repo_path, join_plotinfo = FALSE) %>% 
    filter(survey_trees == TRUE)
  plotinfo <- differentiate_managed_plots(plotinfo)
  year_range <- plotinfo %>% 
    group_by(forest_reserve, period) %>%
    summarize(min_year = min(year_dendro), 
              max_year = max(year_dendro),
              year_range = paste0(min_year, " - ", max_year)) %>% 
    ungroup()
  
  resultaat <- dataset %>% 
    left_join(year_range, by = c("forest_reserve", "period"))
  
  resultaat
}


get_year_range_reg <- function(dataset, repo_path = path_to_git_forresdat){
  reg_by_plot <- read_forresdat("regeneration_by_plot", repo_path, join_plotinfo = TRUE) %>% 
    filter(plottype == "CP")
  reg_by_plot <- differentiate_managed_plots(reg_by_plot)
  year_range <- reg_by_plot %>% 
    group_by(forest_reserve, period) %>%
    summarize(min_year = min(year), 
              max_year = max(year),
              year_range = paste0(min_year, " - ", max_year)) %>% 
    ungroup()
  
  resultaat <- dataset %>% 
    left_join(year_range, by = c("forest_reserve", "period"))
  
  resultaat
}


get_year_range_veg <- function(dataset, repo_path = path_to_git_forresdat){
  veg_by_plot <- read_forresdat("vegetation_by_plot", repo_path, join_plotinfo = TRUE) %>% 
    filter(plottype == "CP")
  veg_by_plot <- differentiate_managed_plots(veg_by_plot)
  year_range <- veg_by_plot %>% 
    mutate(year = year(date_vegetation)) %>% 
    group_by(forest_reserve, period) %>%
    summarize(min_year = min(year), 
              max_year = max(year),
              year_range = paste0(min_year, " - ", max_year)) %>% 
    ungroup()
  
  resultaat <- dataset %>% 
    left_join(year_range, by = c("forest_reserve", "period"))
  
  resultaat
}



#' get the height classes used per forest reserve and per period
#' 
#' This function helps to remove the incorrect zeros added by the function `add_zeros()`
#' 
#' @inheritParams get_open_area
#' 
#' @return dataframe with, per forest reserve and period, the unique heightclasses used
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("regeneration_by_plot_height", repo_path) %>% 
#' filter(plottype == "CP")
#' heightclasses_BR <- get_heights_per_reserve(
#'   dataset = dataset
#' }
#'
#' @importFrom forrescalc read_forresdat create_statistics
#'
get_heights_per_reserve <- function(dataset){
  resultaat <- dataset %>% 
    group_by(forest_reserve, period, height_class) %>% 
    # summarize(n_tree_species = sum(number_of_tree_species)) %>% 
    summarize() %>% 
    ungroup() %>% 
    filter(!is.na(height_class))
  
  resultaat
}


#' get a list of all species occurring per forest reserve and per period
#' 
#' This function helps to remove unnecessary zeros added by the function `add_zeros()`
#' 
#' @inheritParams get_open_area
#' 
#' @return dataframe with, per forest reserve and period, a list of species 
#' found per forest reserve
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("regeneration_by_plot_height_species", repo_path) %>% 
#' filter(plottype == "CP")
#' heightclasses_BR <- get_species_per_reserve(
#'   dataset = dataset
#' }
#'
#' @importFrom forrescalc read_forresdat create_statistics
#'
get_species_per_reserve <- function(dataset){
  resultaat <- dataset %>% 
    group_by(forest_reserve, period, species) %>% 
    summarize(n_heightclasses = n_distinct(height_class)) %>% 
    ungroup() %>% 
    filter(!is.na(species)) 
  
  resultaat
}


#' get number of plots per forest reserve
#' 
#' This function helps to calculate percentage of plots with occurrence of a 
#' particular species (and heightclass)
#' 
#' @inheritParams get_open_area
#' 
#' @return dataframe with number of plots monitored, per forest reserve and period
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("regeneration_by_plot_height_species", repo_path) %>% 
#' filter(plottype == "CP")
#' n_plots <- get_n_plots_per_reserve(
#'   dataset = dataset
#' }
#'
#' @importFrom forrescalc read_forresdat create_statistics
#'
get_n_plots_per_reserve <- function(dataset){
  resultaat <- dataset %>% 
    group_by(forest_reserve, period) %>% 
    summarize(n_max_plots = n_distinct(plot_id)) %>% 
    ungroup() 
  
  resultaat
}


#' create statistics per forest reserve, based on dendro_by_plot
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_plot`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'dendro_by_plot'
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_dendro()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots
#' @importFrom forrescalc read_forresdat create_statistics
#'
statistics_dendro <- function(repo_path = path_to_git_forresdat){
  forest_plot <- get_forest_plot()
  # plotinfo <- read_forresdat("plotinfo", repo_path, join_plotinfo = FALSE)
  dataset <- read_forresdat("dendro_by_plot", repo_path) %>% 
    select(-contains("eg"), -contains("min40cm")) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id)
  
  dataset <- differentiate_managed_plots(dataset)
    
  variables_for_statistics <- dataset %>% 
    select(contains(c("_ha", "tree")), -contains("survey")) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset,
    level = c("period", "forest_reserve"),
    variables = variables_for_statistics,
    include_year_range = FALSE,   
    # year_range: nu nog bug in package, op termijn wel interessant
    na_rm = TRUE # stems_per_tree soms NA
  ) %>% 
    round_df(., 2) %>% 
    # rename(strata = forest_reserve) %>% 
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range
  
  resultaat
}



#' create statistics per forest reserve, based on dendro_by_plot_species
#' 
#' This function first selects all the circular, forested plots, and adds 
#' zero values for all missing combinations of plot and species.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_plot_species`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and species on all of the variables included in 
#' 'dendro_by_plot_species' 
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_dendro_species()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots
#' @importFrom forrescalc read_forresdat add_zeros create_statistics 
#'
statistics_dendro_species <- function(repo_path = path_to_git_forresdat){
  
  con <- odbcConnectAccess2007(path_to_fieldmap_db)
  qSpecies <- sqlFetch(con, "qspecies", stringsAsFactors = FALSE) %>% 
    select(ID, name_nl = Value1, name_sc = Value2)
  odbcClose(con)

  forest_plot <- get_forest_plot()
  
  dataset <- read_forresdat("dendro_by_plot_species", repo_path) %>% 
    select(-contains("eg"), -contains("min40cm")) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) 
  
  dataset_0 <- add_zeros(dataset = dataset %>% 
                            select(plot_id, period, species, contains("_ha")),
                          comb_vars = c("plot_id", "species"),
                          grouping_vars = c("period")
                          ) %>%
    left_join(forest_plot %>% select(plot_id, period, forest_reserve))
  
  dataset_0 <- differentiate_managed_plots(dataset_0)
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha", "tree"))) %>% 
    names()

  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "species"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = TRUE # stems_per_tree soms NA, als soort niet voorkomt
  ) %>% 
    select(-logaritmic) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qSpecies, by = c("species" = "ID")) %>% 
    mutate(strata = "species",
           stratum_name = name_nl,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range()
  
  resultaat
}


#' create statistics per forest reserve, based on dendro_by_diam_plot 
#' 
#' This function first selects all the circular, forested plots, and adds 
#' zero values for all missing combinations of plot and diameter class.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_diam_plot`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and diameter class on all of the variables included in 
#' 'dendro_by_diam_plot' 
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_dendro_diam()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots
#' @importFrom forrescalc read_forresdat add_zeros create_statistics
#'

statistics_dendro_diam <- function(repo_path = path_to_git_forresdat){
  
  # repo_path <- path_to_git_forresdat
  forest_plot <- get_forest_plot()
  
  dataset <- read_forresdat("dendro_by_diam_plot", repo_path) %>% 
    select(-contains("eg"), -contains("min40cm")) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) 
  
  dataset_0 <- add_zeros(dataset = dataset %>% 
                           select(plot_id, period, dbh_class_5cm, contains("_ha")),
                         comb_vars = c("plot_id", "dbh_class_5cm"),
                         grouping_vars = c("period")
                         ) %>%
    left_join(forest_plot %>% select(plot_id, period, forest_reserve))
  
  dataset_0 <- differentiate_managed_plots(dataset_0)
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha", "tree"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "dbh_class_5cm"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = FALSE
  ) %>% 
    select(-logaritmic) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    round_df(., 2) %>% 
    mutate(strata = "dbh_class",
           stratum_name = dbh_class_5cm,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range()
  
  resultaat
}


#' create statistics per forest reserve, based on logs_by_decay_plot
#' 
#' This function first selects all the circular, forested plots, and adds 
#' zero values for all missing combinations of plot and decaystage. 
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Forest_reserves without decaystage of deadwood (Kersselaerspleyn, period 1), 
#' or without full survey of deadwood (Kluisbos_managed, period 2: LIS) 
#' were removed from the dataset.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `logs_by_decay_plot`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and decaystage on all of the variables included in 
#' 'logs_by_decay_plot' 
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_logs_decay()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots
#' @importFrom forrescalc read_forresdat add_zeros create_statistics
#'

statistics_logs_decay <- function(repo_path = path_to_git_forresdat){
  
  con <- odbcConnectAccess2007(path_to_fieldmap_db)
  qDecaystage <- sqlFetch(con, "qdecaystage", stringsAsFactors = FALSE) %>% 
    select(ID, decaystageTxt = Value2)  #afbraak = Value1
  odbcClose(con)

  # repo_path <- path_to_git_forresdat
  forest_plot <- get_forest_plot()
  
  dataset <- read_forresdat("logs_by_decay_plot", repo_path) %>% 
    select(-contains("eg"), -contains("min40cm")) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) %>% 
    full_join(forest_plot %>% 
                filter(survey_deadw == TRUE) %>% 
                select(forest_reserve, plot_id, period)) %>% 
    filter(period != 1 | forest_reserve != "Kersselaerspleyn") %>% 
            # geen decaystage genoteerd in Kerss, periode 1    
    mutate(decaystage = ifelse(is.na(decaystage)
                                , 9999
                                , decaystage),
           vol_log_m3_ha = ifelse(is.na(vol_log_m3_ha)
                                  , 0
                                  , vol_log_m3_ha)
           )
  
  dataset_0 <- add_zeros(dataset = dataset %>% 
                           select(plot_id, period, decaystage, contains("_ha")),
                         comb_vars = c("plot_id", "decaystage"),
                         grouping_vars = c("period")
                         ) %>%
    left_join(forest_plot %>% select(plot_id, period, forest_reserve))
  
  # dataset4_0 <- add_zeros(dataset = dataset4 %>% 
  #                           select(plot_id, period, decay_stage, contains("_ha")),
  #                         comb_vars = c("plot_id", "decay_stage"),
  #                         grouping_vars = c("period")
  # ) %>%
  #   # left_join(qDecaystage, by = c("decay_stage" = "Value2")) %>% 
  #   # rename(afbraak = Value1, 
  #   #        decay_stage_code = ID) %>% 
  #   left_join(plotinfo %>% select(plot_id, period, forest_reserve))
  # 
  
  dataset_0 <- differentiate_managed_plots(dataset_0) %>% 
    filter(forest_reserve != "Kluisbos_managed")
              # niet in elke plot van managed deel van Kluisbos werden logs genoteerd
              # => beter volledig weg, geen representatief gemiddelde anders
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "decaystage"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = FALSE
  ) %>% 
    select(-logaritmic) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qDecaystage, by = c("decaystage" = "ID")) %>% 
    mutate(decaystageTxt = ifelse(decaystage == 9999
                                  , "unknown"
                                  , decaystageTxt),
           strata = "decaystage",
           stratum_name = decaystageTxt,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range()

  resultaat
}


#' create statistics per forest reserve, based on logs_by_decay_plot_species

statistics_logs_decay_species <- function(repo_path = path_to_git_forresdat){
  # TO DO
}


#' create statistics on dendrometry per forest reserve

statistics_dendrometry <- function(repo_path = path_to_git_forresdat){
  
  by_reserve <- statistics_dendro()
  by_species <- statistics_dendro_species()
  by_diam <- statistics_dendro_diam()
  by_decay <- statistics_logs_decay()

  return(
    list(
      stat_dendro_by_reserve = by_reserve
      , stat_dendro_by_species = by_species
      , stat_dendro_by_diam = by_diam
      # , stat_dendro_by_diam_species = by_diam_species,
      , stat_dendro_by_decay = by_decay
      # , stat_dendro_by_decay_species = by_decay_species
    )
  )
}



#' create statistics per forest reserve, based on regeneration_by_plot
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `regeneration_by_plot`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'regeneration_by_plot'
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_reg()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots get_year_range_reg
#' @importFrom forrescalc read_forresdat create_statistics


# NIET - hier ev. keuzemogelijkheid om Els' haar methode  mee te nemen
# (om mean, lci en uci weg te halen van number_ha, en obv de drie waardes 
# een mean en BI per reservaat te berekenen)
# !! werkt volgens mij niet naar behoren

statistics_reg <- function(repo_path = path_to_git_forresdat){
  forest_plot <- get_forest_plot()
  # plotinfo <- read_forresdat("plotinfo", repo_path, join_plotinfo = FALSE)
  dataset <- read_forresdat("regeneration_by_plot", repo_path) %>% 
    select(-contains(c("lci", "mean", "uci", "subplot"))) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id)
  
  dataset <- differentiate_managed_plots(dataset)
  
  variables_for_statistics <- dataset %>% 
    select(contains(c("_ha", "tree", "perc")), -contains("survey")) %>% 
    names()
  # approx_nr (x2), nr_tree_species, rubbing_damage_perc
  
  resultaat <- create_statistics(
    dataset = dataset,
    level = c("period", "forest_reserve"),
    variables = variables_for_statistics,
    include_year_range = FALSE,   
    # year_range: nu nog bug in package, op termijn wel interessant
    na_rm = TRUE # stems_per_tree soms NA
  ) %>% 
    round_df(., 2) %>% 
    # rename(strata = forest_reserve) %>% 
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range_reg
  
  resultaat

}



#' create statistics per forest reserve, based on regeneration_by_plot_height
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `regeneration_by_plot_height`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'regeneration_by_plot_height'
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_reg_height()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots get_year_range_reg
#' @importFrom forrescalc read_forresdat create_statistics
#' 
#' 
statistics_reg_height <- function(repo_path = path_to_git_forresdat){
  
  con <- odbcConnectAccess2007(path_to_fieldmap_db)
  qHeightClass <- sqlFetch(con, "qHeightClass_regeneration", stringsAsFactors = FALSE) %>% 
    select(ID, heightclass_txt = Value1)
  odbcClose(con)
  
  forest_plot <- get_forest_plot()
  
  dataset <- read_forresdat("regeneration_by_plot_height", repo_path) %>% 
    select(-contains(c("lci", "mean", "uci", "subplot"))) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id)
  
  heightclasses_BR <- get_heights_per_reserve(dataset)
  # deze functie maakt een lijst van de heightclasses die voorkomen in elk BR 
  # (om teveel zero's weer te verwijderen)
  
  dataset_0 <- add_zeros(dataset = dataset %>% 
                            select(plot_id, period, height_class, 
                                   contains(c("_perc", "number_of_tree_species", "approx"))),
                          comb_vars = c("plot_id", "height_class"),
                          grouping_vars = c("period")
                         ) %>%
    left_join(plotinfo %>% select(plot_id, period, forest_reserve)) %>% 
    inner_join(heightclasses_BR) %>% 
    mutate(rubbing_damage_perc = ifelse(number_of_tree_species == 0 & rubbing_damage_perc == 0,
                                        NA,
                                        rubbing_damage_perc)
    )
  
  dataset_0 <- differentiate_managed_plots(dataset_0)
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_perc", "number_of_tree_species", "approx"))) %>%  
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "height_class"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = TRUE # stems_per_tree soms NA, als soort niet voorkomt
  ) %>% 
    select(-logaritmic) %>% 
    filter(!is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qHeightClass, by = c("height_class" = "ID")) %>% 
    mutate(strata = "height_class",
           stratum_name = heightclass_txt,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range_reg()
  
  resultaat

}


#' create statistics per forest reserve, based on regeneration_by_plot_height_species
#' 
#' This function first selects all the circular plots within the forested areas. 
#' Subsequently, the managed portion of 'Kluisbos' is modified to 'Kluisbos_managed' 
#' and 'Kluisbos_managed_non_intervention'. 
#' Lastly, the function `create_statistics()` is used to generate statistics for 
#' all the variables within the `regeneration_by_plot_height_species` dataset. 
#' Moreover, as part of the calculation process, the percentage of plots 
#' displaying rejuvenation is determined for each species and height class.
#' 
#' @inheritParams get_open_area
#' 
#' @return dataframe with information on the mean number of rejuvenations per hectare, the percentage of rubbing damage, and the percentage of plots with rejuvenation, categorized by species, height class, and forest reserve.
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_reg_height_species()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots get_year_range_reg
#' @importFrom forrescalc read_forresdat create_statistics
#' 
#' 
statistics_reg_height_species <- function(repo_path = path_to_git_forresdat){

    con <- odbcConnectAccess2007(path_to_fieldmap_db)
    qHeightClass <- sqlFetch(con, "qHeightClass_regeneration", stringsAsFactors = FALSE) %>% 
      select(ID, heightclass_txt = Value1)
    qSpecies <- sqlFetch(con, "qspecies", stringsAsFactors = FALSE) %>% 
      select(ID, name_nl = Value1, name_sc = Value2)
    odbcClose(con)
    
    forest_plot <- get_forest_plot()
    
    dataset <- read_forresdat("regeneration_by_plot_height_species", repo_path) %>% 
      select(-contains(c("lci", "mean", "uci", "subplot"))) %>% 
      filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) %>% 
      differentiate_managed_plots()
    
    heightclasses_BR <- get_heights_per_reserve(dataset)
    # deze functie maakt een lijst van de heightclasses die voorkomen in elk BR 
    # (om onjuiste zero's weer te verwijderen)
    species_BR <- get_species_per_reserve(dataset) 
    # deze functie maakt een lijst van de soorten die voorkomen in elk BR 
    # (om onnodige zero's weer te verwijderen)
    
    dataset_0 <- add_zeros(dataset = dataset %>% 
                             select(plot_id, period, species, height_class, 
                                    contains(c("_perc", "approx"))),
                           comb_vars = c("plot_id", "species", "height_class"),
                           grouping_vars = c("period")
    ) %>%
      left_join(plotinfo %>% select(plot_id, period, forest_reserve)) %>% 
      inner_join(heightclasses_BR) %>% 
      inner_join(species_BR %>% select(-n_heightclasses)) %>% 
      mutate(rubbing_damage_perc = ifelse(approx_nr_regeneration_ha == 0 & rubbing_damage_perc == 0,
                                          NA,
                                          rubbing_damage_perc)
      )
    
    dataset_0 <- differentiate_managed_plots(dataset_0)
    
    variables_for_statistics <- dataset_0 %>% 
      select(contains(c("_perc", "approx"))) %>%  
      names()
    
    # standaard statistieken
    resultaat1 <- create_statistics(
      dataset = dataset_0,
      level = c("period", "forest_reserve", "height_class", "species"),
      variables = variables_for_statistics,
      include_year_range = FALSE,
      na_rm = TRUE # stems_per_tree soms NA, als soort niet voorkomt
    ) %>% 
      select(-logaritmic) %>% 
      filter(!is.na(mean)) %>% 
      round_df(., 2) %>% 
      left_join(qHeightClass, by = c("height_class" = "ID")) %>% 
      left_join(qSpecies, by = c("species" = "ID")) %>% 
      mutate(strata = "height_class",
             stratum_name = heightclass_txt,
             strata2 = "species",
             stratum_name2 = name_nl
      ) %>% 
      get_year_range_reg()
    
    # percentage plots waar soort per hoogteklasse voorkomt
    n_plots_reg <- get_n_plots_per_reserve(dataset)
    
    resultaat2 <- dataset %>% 
      filter(!is.na(approx_nr_regeneration_ha)) %>% 
      group_by(forest_reserve, period, species, height_class) %>% 
      summarize(n_plots = n()) %>% 
      ungroup() %>% 
      left_join(n_plots_reg) %>% 
      mutate(perc_plots = 100*n_plots/n_max_plots) %>% 
      left_join(qSpecies, by = c("species" = "ID")) %>% 
      left_join(qHeightClass, by = c("height_class" = "ID")) %>% 
      round_df(., 2) %>% 
      mutate(variable = "perc_plots_verjonging"
             , n_obs = n_max_plots
             , mean = perc_plots
             , variance = NA, lci = NA, uci = NA
             , strata = "height_class"
             , stratum_name = heightclass_txt
             , strata2 = "species"
             , stratum_name2 = name_nl) %>% 
      get_year_range_reg()
    
    names(resultaat1)
    names(resultaat2)
    resultaat1 <- resultaat1 %>% 
      select(-contains(c("height", "species", "name_", "min", "max")))
    resultaat2 <- resultaat2 %>% 
      select(-contains(c("height", "species", "name_", "min", "max", "plots")))
    
    resultaat <- rbind(resultaat1, resultaat2)
  
}


#' create statistics on regeneration per forest reserve

statistics_regeneration <- function(repo_path = path_to_git_forresdat){
  
  reg_by_reserve <- statistics_reg()
  reg_by_height <- statistics_reg_height()
  reg_by_height_species <- statistics_reg_height_species()
  
  return(
    list(
      stat_reg_by_reserve = reg_by_reserve
      , stat_reg_by_height = reg_by_height
      , stat_reg_by_height_species = reg_by_height_species
    )
  )
}



#' create statistics per forest reserve, based on vegetation_by_plot
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `vegetation_by_plot`.
#' 
#' @inheritParams get_open_area
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'vegetation_by_plot'
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_veg()
#' }
#'
#' @importFrom functions get_forest_plot differentiate_managed_plots get_year_range_veg
#' @importFrom forrescalc read_forresdat create_statistics

statistics_veg <- function(repo_path = path_to_git_forresdat){
  forest_plot <- get_forest_plot()
  
  dataset <- read_forresdat("vegetation_by_plot", repo_path) %>% 
    select(-contains(c("lci", "mean", "uci", "subplot"))) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id)
  
  dataset <- differentiate_managed_plots(dataset)
  
  variables_for_statistics <- dataset %>% 
    select(contains(c("mid", "perc", "number_of_species"))) %>% 
    names()
  # xxx_cover_mid, cumm_herb_coverage_class_average_perc, number_of_species
  
  resultaat <- create_statistics(
    dataset = dataset,
    level = c("period", "forest_reserve"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = TRUE   # regelmatig een NA bij één of andere bedekking, vaak op reesrvaatsniveau
    # ingevulde bedekkingen beter toch meenemen (door na_rm = TRUE) en kijken naar n_obs 
    ) %>% 
    round_df(., 2) %>% 
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range_veg
  
  resultaat

}

#' create statistics per forest reserve, based on herblayer_by_plot
#' 
#' This function first selects all the circular plots within the forested areas. 
#' Subsequently, the managed portion of 'Kluisbos' is modified to 'Kluisbos_managed' 
#' and 'Kluisbos_managed_non_intervention'. 
#' Finally the calculation includes determining the percentage of plots where 
#' the species occurs, as well as calculating the mean cover of the species 
#' based on the plots where it is present.
#' 
#' @inheritParams get_open_area
#' 
#' @return mean cover of each species and percentage of the plots where the species 
#' occurs, per period and forest_reserve
#'
#' @examples
#' \dontrun{
#' resultaat <- statistics_herbs()
#' }
#'
#' @importFrom functions get_forest_plot get_n_plots_per_reserve 
#' differentiate_managed_plots get_year_range_veg
#' @importFrom forrescalc read_forresdat create_statistics

repo_path <- path_to_git_forresdat

statistics_herbs <- function(repo_path = path_to_git_forresdat){
  forest_plot <- get_forest_plot()
  
  # aantal plots obv veg-opname
  veg_by_plot <- read_forresdat("vegetation_by_plot", repo_path) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) %>% 
    differentiate_managed_plots()
  n_plots_veg <- get_n_plots_per_reserve(veg_by_plot)
  
  dataset <- read_forresdat("herblayer_by_plot", repo_path) %>% 
    select(-contains(c("subplot"))) %>% 
    filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) %>% 
    differentiate_managed_plots()
  
  # percentage plots waar soort voorkomt
  resultaat1 <- dataset %>% 
    filter(!is.na(coverage_class_average_perc)) %>% 
    group_by(forest_reserve, period, species) %>% 
    summarize(n_plots = n()) %>% 
    ungroup() %>% 
    left_join(n_plots_veg) %>% 
    mutate(perc_plots = 100*n_plots/n_max_plots) %>% 
    left_join(qHerbSpecies, by = c("species" = "ID")) %>% 
    round_df(., 2) %>% 
    mutate(variable = "perc_plots_met_soort"
           , n_obs = n_max_plots
           , mean = perc_plots
           , variance = NA, lci = NA, uci = NA
           , strata = "species"
           , stratum_name = name_nl
           , strata2 = NA
           , stratum_name2 = NA) %>% 
    get_year_range_veg() 
  
  # karakt. bedekking
  resultaat2 <- dataset %>% 
    group_by(forest_reserve, period, species) %>% 
    summarize(sum_cover = sum(coverage_class_average_perc)) %>% 
    ungroup() %>% 
    left_join(resultaat1 %>% select(forest_reserve, period, species, n_plots, n_max_plots)) %>% 
    mutate(karakt_bedekking = sum_cover/n_plots) %>% 
    left_join(qHerbSpecies, by = c("species" = "ID")) %>% 
    round_df(., 2) %>% 
    mutate(variable = "karakt_bedekking"
           , n_obs = n_max_plots
           , mean = karakt_bedekking
           , variance = NA, lci = NA, uci = NA
           , strata = "species"
           , stratum_name = name_nl
           , strata2 = NA
           , stratum_name2 = NA)  %>% 
    get_year_range_veg()
    
    
  resultaat1 <- resultaat1 %>% 
    select(-contains(c("plots", "species", "name_nl", "min", "max")))
  resultaat2 <- resultaat2 %>% 
    select(-contains(c("sum", "karakt", "plots", "species", "name_nl", "min", "max")))
    
  resultaat <- rbind(resultaat1, resultaat2)
  
}


#' create statistics on vegetation per forest reserve

statistics_vegetation <- function(repo_path = path_to_git_forresdat){
  
  veg_by_reserve <- statistics_veg()
  herbs_by_reserve <- statistics_herbs()
  
  return(
    list(
      stat_veg_by_reserve = veg_by_reserve
      , stat_herbs_by_reserve = herbs_by_reserve
    )
  )
}


