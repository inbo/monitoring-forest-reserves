
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
    include_year_range = FALSE
  ) %>% 
    round_df(., 2) %>% 
    # rename(strata = forest_reserve) %>% 
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA)
  
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
    na_rm = FALSE,
    interval_information = suppressMessages(read_csv2(system.file("extdata/class_data.csv", package = "forrescalc")))
  ) %>% 
    select(-logaritmic) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qSpecies, by = c("species" = "ID")) %>% 
    mutate(strata = "species",
           stratum_name = name_nl,
           strata2 = NA,
           stratum_name2 = NA)
  
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
    na_rm = FALSE,
    interval_information = suppressMessages(read_csv2(system.file("extdata/class_data.csv", package = "forrescalc")))
  ) %>% 
    select(-logaritmic) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    round_df(., 2) %>% 
    mutate(strata = "dbh_class",
           stratum_name = dbh_class_5cm,
           strata2 = NA,
           stratum_name2 = NA)
  
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
  
  dataset_ <- read_forresdat("logs_by_decay_plot", repo_path) %>% 
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
    na_rm = FALSE,
    interval_information = suppressMessages(read_csv2(system.file("extdata/class_data.csv", package = "forrescalc")))
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
           stratum_name2 = NA)

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
      stat_by_reserve = by_reserve
      , stat_by_species = by_species
      , stat_by_diam = by_diam
      # , stat_by_diam_species = by_diam_species,
      , stat_by_decay = by_decay
      # , stat_by_decay_species = by_decay_species
    )
  )
}



#' create statistics per forest reserve, based on regeneration_by_plot

# hier ev. keuzemogelijkheid om Els' haar methode  mee te nemen
# (om mean, lci en uci weg te halen van number_ha, en obv de drie waardes 
# een mean en BI per reservaat te berekenen)
# !! werkt volgens mij niet naar behoren


statistics_reg <- function(repo_path = path_to_git_forresdat){
  # TO DO
}



#' create statistics per forest reserve, based on regeneration_by_plot_height

statistics_reg_height <- function(repo_path = path_to_git_forresdat){
  # TO DO
}




#' create statistics per forest reserve, based on regeneration_by_plot_height_species

statistics_reg_height_species <- function(repo_path = path_to_git_forresdat){
  # TO DO
}



#' create statistics per forest reserve, based on vegetation_by_plot

statistics_veg <- function(repo_path = path_to_git_forresdat){
  # TO DO
}