
##########################################################################
#### Functies ikv dataverwerking op bosreservaatniveau (cirkelplots) ----
#########################################################################

# OPGEPAST ------

# Onderstaande functies enkel van toepassing op cirkelplots van bosreservaten.
# 
# Mocht ik ooit statistieken willen van andere onderzoekssites, moet ik checken 
# of functies voldoen (vooral mbt NA's)


# dendro en reg-functies enkel van toepassing op forest plots


# FUNCTIES -------------------------

#' split Ename into plots situated in open area and in forest plots
#' 
#' This function is developed especially for Ename, which consists of open area 
#' as well as forest.
#' The function changes the field 'forest_reserve' from 'Ename' into 
#' 'Ename_open_area' and 'Ename_forest'.
#' The input dataset can only contain circular plots and has to include the 
#' variables `plot_id` & `forest_reserve`.
#' 
#' @param dataset name of the dataframe where 'forest_reserve' should be split up
#' in open area and forest
#'  
#' @return same dataset as the input, with forest_reserve 'Ename' split up in 
#' 'Ename_open_area' and 'Ename_forest'(resp. 27 plots and xx plots). 
#'
#' @examples
#' \dontrun{
#' dendro_by_plot <- read_forresdat_table(tablename = "dendro_by_plot", 
#'     git_ref_type = "branch", 
#'     git_reference = "develop") %>% 
#'     filter(plottype == "CP")
#' dendro_by_plot <- split_Ename(dataset = dendro_by_plot)
#' }
#'
split_Ename <- function(dataset){
  
  con <- odbcConnectAccess2007(path_to_strata_db)
  management <- sqlFetch(con, "strata_remaining", stringsAsFactors = FALSE) %>% 
    select(plot_id, Management)
  odbcClose(con)

  dataset <- dataset %>% 
    left_join(management) %>% 
    mutate(forest_reserve = 
             ifelse(forest_reserve == "Ename" & Management == "conversion + grazing"
                    , paste0(forest_reserve, "_open")
                    , ifelse(forest_reserve == "Ename"
                             , paste0(forest_reserve, "_forest")
                             , forest_reserve)
             )
    ) %>% 
  select(-Management)
  
  return(dataset)
}


#' split Kluisbos into official forest reserve and managed forest
#' 
#' This function is developed especially for Kluisbos, which consists of a strict 
#' forest reserve ("_SFR", 67 plots 'unmanaged') and a managed forest (55 plots 
#' 'thinned' and 6 plots 'unthinned'). (source: methodology report)
#' Surveys differ between those parts.
#' The function changes the field 'forest_reserve' from 'Kluisbos' into 
#' 'Kluisbos_SFR','Kluisbos_managed'.
#' The input dataset can only contain circular plots and has to include the 
#' variables `plot_id` & `forest_reserve`.
#' 
#' @param dataset name of the dataframe where 'forest_reserve' should be split up
#' in official forest reserve and managed forest.
#'  
#' @return same dataset as the input, with forest_reserve 'Kluisbos' split up in 
#' 'Kluisbos_SFR' and Kluisbos_managed'
#' (resp. 67 plots and 61 plots). 
#'
#' @examples
#' \dontrun{
#' dendro_by_plot <- read_forresdat_table(tablename = "dendro_by_plot", 
#'     git_ref_type = "branch", 
#'     git_reference = "develop") %>% 
#'     filter(plottype == "CP")
#' dendro_by_plot <- split_Kluisbos(dataset = dendro_by_plot)
#' }
#'
split_Kluisbos <- function(dataset){
  
  con <- odbcConnectAccess2007(path_to_strata_db)
  management <- sqlFetch(con, "strata_remaining", stringsAsFactors = FALSE) %>% 
    select(plot_id, SubArea)
  odbcClose(con)
  
  dataset <- dataset %>% 
    left_join(management) %>% 
    mutate(forest_reserve = 
             ifelse(forest_reserve == "Kluisbos" & str_detect(SubArea, "SFR")
                    , paste0(forest_reserve, "_SFR")
                    , ifelse(forest_reserve == "Kluisbos"
                           , paste0(forest_reserve, "_managed")
                           , forest_reserve)
                    )
           ) %>% 
  select(-SubArea)

  return(dataset)
}


#' get plotinfo for statistics based on circular plots
#' 
#' This function gets plotinfo from the datapackage, selects only circular plots
#' and splits some forest_reserves further up.
#' (f.e. 'Kluisbos' into 'Kluisbos_managed' and Kluisbos_unmanaged'; 
#' 'Ename' in 'Ename_open' and 'Ename_forest').
#' It's a helper function for the statistics functions.
#'
#' @param datapackage the datapackage with forresdat data
#' 
#' @return plotinfo of circular plots with some forest_reserves split up 
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' plotinfo <- get_plotinfo_cp_for_stat(datapackage)
#' }
#'
#'@importFrom forrescalc read_resource 
get_plotinfo_cp_for_stat <- function(datapackage){
  plotinfo <- read_resource(datapackage, "plotinfo") %>% 
    split_Kluisbos() %>% 
    split_Ename() %>%
    # op termijn ev. split_sevendonck (?)
    filter(plottype == "CP")
  
  return(plotinfo)
}



#' get dataframe with list of circular plots located in strict forest reserves
#'
#' This function creates a dataframe with all the circular plots 
#' (only processed data) located in strict forest reserves.
#' The dataframe is derived from the data saved in the forresdat-folder  
#' (`plotinfo`: only processed plots)
#' 
#' @param datapackage the datapackage with forresdat data 
#' 
#' @return dataframe with all circular plots located in forested area
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' forest_plot <- get_strict_forest_reserve_plots()
#' }
#'
#'@importFrom forrescalc read_resource 
get_strict_forest_reserve_plots <- function(datapackage){
  plotinfo <- read_resource(datapackage, "plotinfo") %>% 
    filter(plottype == "CP") %>% 
    split_Kluisbos()
  
  not_strict_reserves <- c("De heide", "Ename", "Ename_open", "Ename_forest"
                           , "Kluisbos_managed")
  
  strict_reserve_plots <- plotinfo %>% 
    filter(!forest_reserve %in% not_strict_reserves) %>% 
    select(forest_reserve, plot_id) %>% 
    unique()
  
  return(strict_reserve_plots)
}


#' get year range per forest reserve, based on dendro_by_plot - TEMPORARY
#' 
#' This is a temporary function that replaces `include_year_range = TRUE`, in 
#' the function `create_statistics()`.
#' Input dataset is output from the function `create_statistics()`
#' 
#' @inheritParams get_strict_forest_reserve_plots
#' 
#' @return statistics with extra info on year_range
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' dendro_by_plot <- read_resource(datapackage, "dendro_by_plot") %>% 
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
#'@examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' stat <- get_year_range(statistics)
#' }
#'
#'@importFrom functions split_Kluisbos split_Ename
#'@importFrom forrescalc read_resource 
#'
get_year_range <- function(statistics, datapackage){
  plotinfo <- read_resource(datapackage, "plotinfo") %>% 
    filter(survey_trees == TRUE & plottype == "CP") %>% 
    split_Kluisbos() %>% 
    # gaan we altijd doen, zelfs al wordt communal deel van Kluisbos ook meegenomen
    split_Ename()
      
  year_range <- plotinfo %>% 
    group_by(forest_reserve, period) %>%
    summarize(min_year = min(year_dendro)
              , max_year = max(year_dendro)
              # ,year_range = paste0(min_year, " - ", max_year)
              ) %>% 
    ungroup()
  
  resultaat <- statistics %>% 
    left_join(year_range, by = c("forest_reserve", "period"))
  
  return(resultaat)
}


get_year_range_reg <- function(statistics, datapackage){
  reg_by_plot <- read_resource(datapackage, "reg_by_plot") %>% 
    filter(plottype == "CP") %>% 
    split_Kluisbos() %>% 
    # gaan we altijd doen, zelfs al wordt communal deel van Kluisbos ook meegenomen
    split_Ename()
  
  year_range <- reg_by_plot %>% 
    group_by(forest_reserve, period) %>%
    summarize(min_year = min(year) 
              , max_year = max(year)
              # , year_range = paste0(min_year, " - ", max_year)
              ) %>% 
    ungroup()
  
  resultaat <- statistics %>% 
    left_join(year_range, by = c("forest_reserve", "period"))
  
  return(resultaat)
}


get_year_range_veg <- function(statistics, datapackage){
  veg_by_plot <- read_resource(datapackage, "veg_by_plot") %>% 
    filter(plottype == "CP") %>% 
    split_Kluisbos() %>% 
    # gaan we altijd doen, zelfs al wordt communal deel van Kluisbos ook meegenomen
    split_Ename()
  
  year_range <- veg_by_plot %>% 
    mutate(year = year(date_vegetation)) %>% 
    group_by(forest_reserve, period) %>%
    summarize(min_year = min(year) 
              , max_year = max(year)
              # , year_range = paste0(min_year, " - ", max_year)
              ) %>% 
    ungroup()
  
  resultaat <- statistics %>% 
    left_join(year_range, by = c("forest_reserve", "period"))
  
  return(resultaat)
}



#' get the height classes of regeneration used per forest reserve and per period
#' 
#' This function helps to remove the incorrect zeros added by the function `add_zeros()`
#' 
#' @inheritParams get_strict_forest_reserve_plots
#' 
#' @return dataframe with, per forest reserve and period, the unique heightclasses used
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("reg_by_plot_height", repo_path) %>% 
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
  
  return(resultaat)
}


#' get the diameter classes used per forest reserve and per period
#' 
#' This function helps to remove the unnecessary zeros added by the function `add_zeros()`
#' 
#' @inheritParams get_strict_forest_reserve_plots
#' 
#' @return dataframe with, per forest reserve and period, the unique diameter classes
#' encountered
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("dendro_by_diam_plot_species", repo_path) %>% 
#' filter(plottype == "CP")
#' diamclasses_BR <- get_diamclasses_per_reserve(
#'   dataset = dataset
#' }
#'
#' @importFrom forrescalc read_forresdat create_statistics
#'
get_diamclasses_per_species_per_reserve <- function(dataset){
  resultaat <- dataset %>% 
    group_by(forest_reserve, period, species, dbh_class_5cm) %>% 
    # summarize(n_tree_species = sum(number_of_tree_species)) %>% 
    summarize() %>% 
    ungroup()
  
  return(resultaat)
}



#' get a list of all species occurring per forest reserve and per period
#' 
#' This function helps to remove unnecessary zeros added by the function `add_zeros()`
#' 
#' @inheritParams get_strict_forest_reserve_plots
#' 
#' @return dataframe with, per forest reserve and period, a list of species 
#' found per forest reserve
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("reg_by_plot_height_species", repo_path) %>% 
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
    summarize() %>% 
    ungroup() %>% 
    filter(!is.na(species)) 
  
  return(resultaat)
}


#' get number of plots per forest reserve
#' 
#' This function helps to calculate percentage of plots with occurrence of a 
#' particular species (and heightclass)
#' 
#' @inheritParams get_strict_forest_reserve_plots
#' 
#' @return dataframe with number of plots monitored, per forest reserve and period
#'
#' @examples
#' \dontrun{
#' dataset <- read_forresdat("reg_by_plot_height_species", repo_path) %>% 
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
  
  return(resultaat)
}


#' create statistics per forest reserve, based on dendro_by_plot
#' 
#' This function first selects all the circular plots and splits some 
#' forest_reserves further up.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_plot`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'dendro_by_plot'
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_dendro(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range
#' @importFrom forrescalc read_resource create_statistics
#'
statistics_dendro <- function(datapackage){
  plotinfo <- get_plotinfo_cp_for_stat(datapackage)
 
  dataset <- read_resource(datapackage, "dendro_by_plot") %>% 
    inner_join(plotinfo %>% select(forest_reserve, plot_id, period)) %>% # only cp
    select(-contains(c("40cm")))
    
  variables_for_statistics <- dataset %>% 
    select(contains(c("_ha", "tree"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset,
    level = c("period", "forest_reserve"),
    variables = variables_for_statistics,
    include_year_range = TRUE,   
    na_rm = TRUE # stems_per_tree soms NA
    ) %>% 
    round_df(., 2) %>% 
    # rename(strata = forest_reserve) %>% 
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    select(-contains(c("log")))
  
  return(resultaat)
}

#' correct NA and 0 values for lis- and log-volumes after use of add_zero()
#' 
#' This function is based on info on survey_lis and survey_deadw and is a helper 
#' function for the statistics function to correct for false zeros
#'  
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return dataframe woth correct NA or 0 value for llis- and log-volumes
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_dendro_species(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range
#' @importFrom forrescalc read_resource add_zeros create_statistics 
#'
correct_deadw_after_add_zeros <- function(dataset, plotinfo){
  dataset <- dataset %>% 
    left_join(plotinfo %>% select(plot_id, period
                                  , survey_lis, survey_deadw
                                  , diam_min_a4_logs))

    # TIJDELIJK TOT AANGEVULD IN PLOTINFO op forresdat (27/2/2026)
    # 
    # mutate(diam_min_a4_logs = ifelse(forest_reserve == "Zoniën UITBR_1995" & 
    #                                    period == 1 & survey_deadw
    #                                  , 100
    #                                  , diam_min_a4_logs)) 
  
  if (!"dbh_class_5cm" %in% colnames(dataset)) {
    dataset <- dataset %>% 
      mutate(
        # !!!! OPGEPAST: op termijn misschien voor alle periodes 
        # "vol_log_above30cm_m3_ha" berekend
        # dan mag "vol_log_above30cm_m3_ha" NIET op NA gezet worden !!!!!!!!!!!!!
      vol_log_above30cm_m3_ha = ifelse(survey_deadw == TRUE & 
                                         diam_min_a4_logs == 300
                                       , vol_log_above30cm_m3_ha
                                       , NA)
      , vol_lis_m3_ha = ifelse(survey_lis == TRUE & !is.na(survey_lis)
                               , vol_lis_m3_ha
                               , NA)
      , vol_lis_10_30cm_m3_ha = ifelse(survey_lis == TRUE & !is.na(survey_lis)
                                       , vol_lis_10_30cm_m3_ha
                                       , NA)
      # !! ook omgekeerd: vol_log_m3_ha = NA wanneer LIS gebruikt is én 
      # treshold logs = 300
      , vol_log_m3_ha = ifelse((survey_deadw == TRUE & diam_min_a4_logs != 100) |
                                 survey_deadw == FALSE
                               , NA 
                               , vol_log_m3_ha)
      )
  }
  
  # (1): logs werden niet opgemeten in diameterklasse 5-10 cm, maar worden nu als 0 toegevoegd 
  # => terug NA van maken  
  # (ter info: waar staande bomen voorkwamen in 5-10 cm, is NA behouden; enkel als er niks voorkwam in
  # 5-10 cm werd overal (ook bij logs) een 0 geplaatst)
  
  # (2) weliswaar geen LIS die NA moet zijn in periode 1 en 2 (ipv 0)
  # maar wél vol_log_m3_ha die in periode 3 NA moet zijn in lage diameterklasses
  if ("dbh_class_5cm" %in% colnames(dataset)) {
    diam_klasses_under_10cm <- c("5 - 10 cm")
    diam_klasses_under_30cm <- c("5 - 10 cm", "10 - 15 cm", "15 - 20 cm", "20 - 25 cm", "25 - 30 cm")
    
    dataset <- dataset %>% 
      left_join(plotinfo %>% select(plot_id, period
                                    , survey_lis, survey_deadw
                                    , diam_min_a4_logs)) %>% 
      mutate(vol_log_m3_ha = ifelse(survey_deadw == FALSE, NA, vol_log_m3_ha)
               , vol_log_m3_ha = ifelse(dbh_class_5cm %in% diam_klasses_under_10cm
                                   , NA, vol_log_m3_ha)  # always
             , vol_log_m3_ha = ifelse(survey_deadw == T & diam_min_a4_logs == 300 & 
                                       dbh_class_5cm %in% diam_klasses_under_30cm
                                     , NA, vol_log_m3_ha)
      )
    }
  
  # UITZ: kers 1ste decade (zit niet in fieldmap, data uit 1ste rapport gehaald)
  # Géén info over diameterklasse of afbraakklasse, enkel soort 
  # => daar alles op NA zetten
  if ("decaystage" %in% colnames(dataset) | "dbh_class_5cm" %in% colnames(dataset)) {
    dataset <- dataset %>% 
      mutate(vol_log_m3_ha = ifelse(between(plot_id, 2000, 2070) & period == 1
                                    #forest_reserve %in% c("Zoniën UITBR_1995") & plottype == "CP" 
                                    , NA
                                    , vol_log_m3_ha)) 
  }
  
  # t <- dataset2_0_ %>% filter(is.na(vol_lis_m3_ha) & !is.na(vol_log_above30cm_m3_ha))
  
  dataset <- dataset %>% 
    select(-survey_lis, -survey_deadw, -diam_min_a4_logs)
  
  return(dataset)
}
  
#' correct NA values for deadwood volumes after right_join with plotinfo
#' 
#' This function is based on info on survey_trees, survey_lis and survey_deadw 
#' and is a helper function for the statistics function to correct for false zeros
#'  
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return dataframe woth correct 0 value for deadwood volumes
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_dendro_species(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range
#' @importFrom forrescalc read_resource add_zeros create_statistics 
#'
correct_deadw_after_right_join <- function(dataset, plotinfo){
  dataset <- dataset %>% 
    left_join(plotinfo %>% select(plot_id, period
                                  , survey_lis, survey_deadw, survey_trees
                                  , diam_min_a4_logs)) %>% 
    mutate(
      vol_dead_standing_m3_ha = ifelse(is.na(vol_dead_standing_m3_ha) &
                                         survey_trees == TRUE
                                       , 0, vol_dead_standing_m3_ha)
      , vol_bole_dead_m3_ha = ifelse(is.na(vol_bole_dead_m3_ha) &
                                       survey_trees == TRUE
                                     , 0, vol_bole_dead_m3_ha)
      # !!!! OPGEPAST: op termijn misschien voor alle periodes 
      # "vol_log_above30cm_m3_ha" berekend
      # dan mag "vol_log_above30cm_m3_ha" NIET op NA gezet worden !!!!!!!!!!!!!
      , vol_log_above30cm_m3_ha = ifelse(is.na(vol_log_above30cm_m3_ha) &
                                           survey_deadw == TRUE & 
                                           diam_min_a4_logs == 300
                                         , 0, vol_log_above30cm_m3_ha)
      , vol_lis_m3_ha = ifelse(is.na(vol_lis_m3_ha) &
                                 survey_lis == TRUE & !is.na(survey_lis)
                               , 0, vol_lis_m3_ha)
      , vol_lis_10_30cm_m3_ha = ifelse(is.na(vol_lis_10_30cm_m3_ha) &
                                         survey_lis == TRUE & !is.na(survey_lis)
                                       , 0, vol_lis_10_30cm_m3_ha)
      # !! ook omgekeerd: vol_log_m3_ha enkel = 0 zetten, wanneer treshold logs = 100
      , vol_log_m3_ha = ifelse(is.na(vol_log_m3_ha) & diam_min_a4_logs == 100
                               , 0, vol_log_m3_ha)
    )
 
  dataset <- dataset %>% 
    select(-survey_lis, -survey_deadw, -survey_trees
           , -diam_min_a4_logs)
  
  return(dataset)
}



#' create statistics per forest reserve, based on dendro_by_plot_species
#' 
#' This function first selects all the circular plots and splits some 
#' forest_reserves further up.
#' Then zero values for all missing combinations of plot and species are added.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_plot_species`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and species on all of the variables included in 
#' 'dendro_by_plot_species' 
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_dendro_species(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range 
#' @importFrom functions correct_deadw_after_add_zeros
#' @importFrom forrescalc read_resource add_zeros create_statistics 
#'
statistics_dendro_species <- function(datapackage){
  plotinfo <- get_plotinfo_cp_for_stat(datapackage)
  
  dataset <- read_resource(datapackage, "dendro_by_plot_species") %>% 
    inner_join(plotinfo %>% select(forest_reserve, plot_id, period)) %>% 
        # only cp + info on forest_reserve
    select(-contains(c("40cm")))
  
  qspecies <- read_resource(datapackage, "qspecies") %>% 
    select(ID, name_nl = Value1, name_sc = Value2)

  dataset_0 <- add_zeros(dataset %>% 
                           select(forest_reserve, plot_id, period, species
                                  , contains(c("_ha", "stems"))),
                         comb_vars = c("plot_id", "species", "period"),
                         grouping_vars = c("forest_reserve"),
                         defaults_to_na = c("stems_per_tree"))  
  
  dataset_0 <- correct_deadw_after_add_zeros(dataset_0, plotinfo)
  # vol_lis soms incorrect als 0 toegevoegd mbv add_zeros(() 
  # awanneer de soort helemaal niet voorkomt in de plot (ook niet levend of als log)
    
  dataset_0 <- dataset_0 %>% 
    left_join(plotinfo %>% select(plot_id, period, year = year_dendro))
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha", "stems"))) %>% 
    names()

  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "species"),
    variables = variables_for_statistics,
    include_year_range = TRUE,
    na_rm = TRUE # stems_per_tree soms NA, als soort niet voorkomt
    ) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qspecies, by = c("species" = "ID")) %>% 
    mutate(strata = "species",
           stratum_name = name_sc,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    select(-contains(c("log", "species", "name_sc")))
  
  return(resultaat)
}


#' create statistics per forest reserve, based on dendro_by_diam_plot 
#' 
#' This function first selects all the circular plots and splits some 
#' forest_reserves further up.
#' Then zero values for all missing combinations of plot and diameterclass are added.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_diam_plot`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and diameter class on all of the variables included in 
#' 'dendro_by_diam_plot' 
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_dendro_diam(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range add_zeros
#' @importFrom functions correct_deadw_after_add_zeros
#' @importFrom forrescalc read_resource create_statistics
#'
statistics_dendro_diam <- function(datapackage){
  plotinfo <- get_plotinfo_cp_for_stat(datapackage)
  
  dataset <- read_resource(datapackage, "dendro_by_diam_plot") %>% 
    # filter(plot_id %in% plotinfo$plot_id) # only cp, géén veld forest_reserve
    inner_join(plotinfo %>% select(forest_reserve, plot_id, period))
    # only cp + info on forest_reserve
                
  dataset_0 <- add_zeros(dataset %>% 
                           select(forest_reserve, plot_id, period
                                  , dbh_class_5cm, contains(c("_ha"))),
            comb_vars = c("plot_id", "dbh_class_5cm", "period"),
            grouping_vars = c("forest_reserve")
            )

  dataset_0 <- correct_deadw_after_add_zeros(dataset_0, plotinfo)
  # vol_lis soms incorrect als 0 toegevoegd mbv add_zeros()
  # ook dbh_classes < 30 cm soms foute 0
  
  dataset_0 <- dataset_0 %>% 
    left_join(plotinfo %>% select(plot_id, period, year = year_dendro))

  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "dbh_class_5cm"),
    variables = variables_for_statistics,
    include_year_range = TRUE,
    na_rm = TRUE,
    interval_information = 
      suppressMessages(read_csv2(system.file("extdata/class_data.csv",
                                             package = "forrescalc")))
    ) %>% 
    filter(mean != 0 & !is.na(mean)) %>% 
    forestmangr::round_df(., 2) %>% 
    mutate(strata = "dbh_class",
           stratum_name = dbh_class_5cm,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    select(-contains(c("log", "dbh")))

  return(resultaat)
}


#' create statistics per forest reserve, based on dendro_by_diam_plot_species 
#' 
#' This function first selects all the circular plots and splits some 
#' forest_reserves further up.
#' Then zero values for all missing combinations of plot and species and
#' diameterclasse are added.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `dendro_by_diam_plot_species`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and diameter class on all of the variables included in 
#' 'dendro_by_diam_plot_species' 
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_dendro_diam_species(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range add_zeros
#' @importFrom functions correct_deadw_after_add_zeros
#' @importFrom forrescalc read_resource create_statistics
#'

statistics_dendro_diam_species <- function(datapackage){
  plotinfo <- get_plotinfo_cp_for_stat(datapackage)
  
  dataset <- read_resource(datapackage, "dendro_by_diam_plot_species") %>% 
    inner_join(plotinfo %>% select(forest_reserve, plot_id, period))  # only cp

  qspecies <- read_resource(datapackage, "qspecies") %>% 
    select(ID, name_nl = Value1, name_sc = Value2)
  
  diam_spec_BR <- get_diamclasses_per_species_per_reserve(dataset)
  # deze functie maakt een lijst van de diameter classes die per soort voorkomen 
  # in elk BR (om onnodige zero's weer te verwijderen)
  
  dataset_0 <- add_zeros(dataset = dataset %>% 
                           select(plot_id, period, dbh_class_5cm, species, contains("_ha")),
                         comb_vars = c("plot_id", "dbh_class_5cm", "species"),
                         grouping_vars = c("period")
                         ) %>%
    left_join(plotinfo %>% select(forest_reserve, plot_id, period)) %>% 
    inner_join(diam_spec_BR)
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "dbh_class_5cm", "species"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = FALSE
  ) %>% 
    filter(!is.na(mean)) %>% # Kerss periode 1 en Kluisbos_managed geen liggend dood hout
    round_df(., 2) %>% 
    left_join(qSpecies, by = c("species" = "ID")) %>% 
    mutate(strata = "dbh_class",
           stratum_name = dbh_class_5cm,
           strata2 = "species",
           stratum_name2 = name_sc) %>% 
    get_year_range(datapackage) %>% 
    select(-contains(c("log", "dbh", "species", "name_sc")))
  
  return(resultaat)
}



#' create statistics per forest reserve, based on deadw_by_decay_plot
#' 
#' This function first selects all the circular, forested plots, and adds 
#' zero values for all missing combinations of plot and decaystage. 
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Forest_reserves without decaystage of deadwood (Kersselaerspleyn, period 1), 
#' or without full survey of deadwood (Kluisbos_managed, period 2: LIS) 
#' were removed from the dataset.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `deadw_by_decay_plot`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and decaystage on all of the variables included in 
#' 'deadw_by_decay_plot' 
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_logs_decay(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range 
#' @importFrom functions correct_deadw_after_right_join 
#' @importFrom functions correct_deadw_after_add_zeros
#' @importFrom forrescalc read_resource add_zeros create_statistics
#'

statistics_deadw_decay <- function(datapackage){
  plotinfo <- get_plotinfo_cp_for_stat(datapackage)
  
  dataset <- read_resource(datapackage, "deadw_by_decay_plot") %>% 
    mutate(decaystage = ifelse(is.na(decaystage)
                               , 9999
                               , decaystage)) %>%
    # enkel een right_join bij deadwood, want niet in elke plot deadwood
    # anders voldoet een inner_join
    right_join(plotinfo %>% select(forest_reserve, plot_id, period)) %>% # only cp
    select(-contains("40cm"))  
  
  # NA's van extra plots op "0" zetten
  dataset <- correct_deadw_after_right_join(dataset, plotinfo) 
  
  # decaystage_txt uitbreiden met "unknown"
  qdecaystage <- read_resource(datapackage, "qdecaystage") %>% 
    select(ID, decaystage_txt = Value2)
  tmp <- data.frame(ID = 9999, decaystage_txt = "unknown")
  qdecaystage <- rbind(qdecaystage, tmp)
  
  dataset_0 <- add_zeros(dataset = dataset2 %>% 
                           select(forest_reserve, plot_id, period
                                  , decaystage, contains("_ha")),
                         comb_vars = c("plot_id", "decaystage", "period"),
                         grouping_vars = c("forest_reserve")
                         ) %>% 
    left_join(qdecaystage, by = c("decaystage" = "ID")) 
  
  dataset_0 <- correct_deadw_after_add_zeros(dataset_0, plotinfo)
  # vol_log/vol_lis soms incorrect als 0 toegevoegd mbv add_zeros()
  # wanneer de soort helemaal niet voorkomt in de plot (ook niet levend of als log) 
  
  dataset_0 <- dataset_0 %>% 
    left_join(plotinfo %>% select(plot_id, period, year = year_dendro))
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "decaystage"),
    variables = variables_for_statistics,
    include_year_range = TRUE,
    na_rm = TRUE,  #verschil met TRUE is enkel nObs, wordt 1 ipv 33 bij TRUE
    interval_information = 
      suppressMessages(read_csv2(system.file("extdata/class_data.csv",
                                             package = "forrescalc")))
    ) %>% 
    filter(!is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qdecaystage, by = c("decaystage" = "ID")) %>% 
    mutate(strata = "decaystage",
           stratum_name = decaystage_txt,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    select(-contains(c("log", "decayst")))

  return(resultaat)
}


#' create statistics per forest reserve, based on deadw_by_decay_plot_species

#' This function first selects all the circular, forested plots, and adds 
#' zero values for all missing combinations of plot and decaystage. 
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Forest_reserves without decaystage of deadwood (Kersselaerspleyn, period 1), 
#' or without full survey of deadwood (Kluisbos_managed, period 2: LIS) 
#' were removed from the dataset.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `deadw_by_decay_plot_species`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and decaystage on all of the variables included in 
#' 'deadw_by_decay_plot_species' 
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_logs_decay_species(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range
#' @importFrom functions correct_deadw_after_right_join 
#' @importFrom functions correct_deadw_after_add_zeros
#' @importFrom forrescalc read_resource add_zeros create_statistics
#'

statistics_deadw_decay_species <- function(repo_path = path_to_git_forresdat){
  
  con <- odbcConnectAccess2007(path_to_fieldmap_db)
  qDecaystage <- sqlFetch(con, "qdecaystage", stringsAsFactors = FALSE) %>% 
    select(ID, decaystage_txt = Value2)  #afbraak = Value1
  qSpecies <- sqlFetch(con, "qspecies", stringsAsFactors = FALSE) %>% 
    select(ID, name_nl = Value1, name_sc = Value2)
  odbcClose(con)
  
  forest_plot <- get_forest_plots()
  
  dataset <- read_forresdat("deadw_by_decay_plot_species", repo_path) %>% 
    select(-contains("eg"), -contains("40cm")) %>% 
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
  
  species_BR <- get_species_per_reserve(dataset)
  # deze functie maakt een lijst van de soorten die voorkomen in elk BR 
  # (om onnodige zero's weer te verwijderen)
  
  dataset_0 <- add_zeros(dataset = dataset %>% 
                           select(plot_id, period, species, decaystage, contains("_ha")),
                         comb_vars = c("plot_id", "species", "decaystage"),
                         grouping_vars = c("period")
                         ) %>% 
    left_join(plotinfo %>% select(forest_reserve, plot_id, period)) %>% 
    inner_join(species_BR) %>%
    filter(forest_reserve != "Kluisbos_managed")
  # niet in elke plot van managed deel van Kluisbos werden logs genoteerd
  # => beter volledig weg, geen representatief gemiddelde anders
  
  variables_for_statistics <- dataset_0 %>% 
    select(contains(c("_ha"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset_0,
    level = c("period", "forest_reserve", "species", "decaystage"),
    variables = variables_for_statistics,
    include_year_range = FALSE,
    na_rm = FALSE
    ) %>% 
    filter(!is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qDecaystage, by = c("decaystage" = "ID")) %>% 
    left_join(qSpecies, by = c("species" = "ID")) %>% 
    mutate(decaystage_txt = ifelse(decaystage == 9999
                                  , "unknown"
                                  , decaystage_txt),
           strata = "decaystage",
           stratum_name = decaystage_txt,
           strata2 = "species",
           stratum_name2 = name_sc) %>% 
    get_year_range(datapackage) %>% 
    select(-contains(c("log", "decay", "species", "name_sc")))
  
  return(resultaat)
}


#' create statistics per forest reserve, based on carbon_by_plot
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `carbon_by_plot`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'carbon_by_plot'
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_carbon(datapackage)
#' }
#'
#' @importFrom functions get_plotinfo_cp_for_stat get_year_range
#' @importFrom forrescalc read_resource create_statistics
#'
statistics_carbon <- function(datapackage){
  plotinfo <- get_plotinfo_cp_for_stat(datapackage)
  
  dataset <- read_resource(datapackage, "carbon_by_plot") %>% 
    inner_join(plotinfo %>% select(forest_reserve, plot_id, period)) %>% # only cp
    select(-contains(c("40cm")))

  variables_for_statistics <- dataset %>% 
    select(contains(c("m3_ha", "_t_ha"))) %>% 
    names()
  
  resultaat <- create_statistics(
    dataset = dataset,
    level = c("period", "forest_reserve"),
    variables = variables_for_statistics,
    include_year_range = TRUE,   
    na_rm = FALSE
    ) %>% 
    round_df(., 2) %>% 
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    select(-contains(c("log")))
  
  return(resultaat)
}

#' create statistics on dendrometry per forest reserve

statistics_dendrometry <- function(repo_path = path_to_git_forresdat){
  
  by_reserve <- statistics_dendro()
  by_species <- statistics_dendro_species()
  by_diam <- statistics_dendro_diam()
  by_diam_species <- statistics_dendro_diam_species()
  by_decay <- statistics_logs_decay()
  by_decay_species <- statistics_logs_decay_species()
  carbon_by_reserve <- statistics_carbon()

  return(
    list(
      stat_dendro = by_reserve
      , stat_dendro_by_species = by_species
      , stat_dendro_by_diam = by_diam
      , stat_dendro_by_diam_species = by_diam_species
      , stat_logs_by_decay = by_decay
      , stat_logs_by_decay_species = by_decay_species
      , stat_carbon = carbon_by_reserve
    )
  )
}



#' create statistics per forest reserve, based on reg_by_plot
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `reg_by_plot`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'reg_by_plot'
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_reg(datapackage)
#' }
#'
#' @importFrom functions get_forest_plots differentiate_managed_plots get_year_range_reg
#' @importFrom forrescalc read_resource create_statistics


# NIET - hier ev. keuzemogelijkheid om Els' haar methode  mee te nemen
# (om mean, lci en uci weg te halen van number_ha, en obv de drie waardes 
# een mean en BI per reservaat te berekenen)
# !! werkt volgens mij niet naar behoren

statistics_reg <- function(repo_path = path_to_git_forresdat){
  forest_plot <- get_forest_plots()
  # plotinfo <- read_forresdat("plotinfo", repo_path, join_plotinfo = FALSE)
  dataset <- read_forresdat("reg_by_plot", repo_path) %>% 
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
    mutate(strata = NA,
           stratum_name = NA,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range_reg() %>% 
    select(-contains(c("log")))
  
  return(resultaat)

}



#' create statistics per forest reserve, based on reg_by_plot_height
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `reg_by_plot_height`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'reg_by_plot_height'
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_reg_height(datapackage)
#' }
#'
#' @importFrom functions get_forest_plots differentiate_managed_plots get_year_range_reg
#' @importFrom forrescalc read_resource add_zeros create_statistics
#' 
#' 
statistics_reg_height <- function(repo_path = path_to_git_forresdat){
  
  con <- odbcConnectAccess2007(path_to_fieldmap_db)
  qHeightClass <- sqlFetch(con, "qHeightClass_regeneration", stringsAsFactors = FALSE) %>% 
    select(ID, heightclass_txt = Value1)
  odbcClose(con)
  
  plotinfo <- read_forresdat("plotinfo", repo_path, join_plotinfo = FALSE) %>% 
    filter(survey_reg == TRUE) %>% 
    differentiate_managed_plots()
  
  forest_plot <- get_forest_plots()
  
  dataset <- read_forresdat("reg_by_plot_height", repo_path) %>% 
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
    filter(!is.na(mean)) %>% 
    round_df(., 2) %>% 
    left_join(qHeightClass, by = c("height_class" = "ID")) %>% 
    mutate(strata = "height_class",
           stratum_name = heightclass_txt,
           strata2 = NA,
           stratum_name2 = NA) %>% 
    get_year_range_reg() %>% 
    select(-contains(c("log", "height")))
  
  return(resultaat)

}


#' create statistics per forest reserve, based on reg_by_plot_height_species
#' 
#' This function first selects all the circular plots within the forested areas. 
#' Subsequently, the managed portion of 'Kluisbos' is modified to 'Kluisbos_managed' 
#' and 'Kluisbos_managed_non_intervention'. 
#' Lastly, the function `create_statistics()` is used to generate statistics for 
#' all the variables within the `reg_by_plot_height_species` dataset. 
#' Moreover, as part of the calculation process, the percentage of plots 
#' displaying rejuvenation is determined for each species and height class.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return dataframe with information on the mean number of rejuvenations per hectare, the percentage of rubbing damage, and the percentage of plots with rejuvenation, categorized by species, height class, and forest reserve.
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_reg_height_species(datapackage)
#' }
#'
#' @importFrom functions get_forest_plots differentiate_managed_plots get_year_range_reg
#' @importFrom forrescalc read_resource add_zeros create_statistics
#' 
#' 
statistics_reg_height_species <- function(repo_path = path_to_git_forresdat){

    con <- odbcConnectAccess2007(path_to_fieldmap_db)
    qHeightClass <- sqlFetch(con, "qHeightClass_regeneration", stringsAsFactors = FALSE) %>% 
      select(ID, heightclass_txt = Value1)
    qSpecies <- sqlFetch(con, "qspecies", stringsAsFactors = FALSE) %>% 
      select(ID, name_nl = Value1, name_sc = Value2)
    odbcClose(con)
    
    plotinfo <- read_forresdat("plotinfo", repo_path, join_plotinfo = FALSE) %>% 
      filter(survey_reg == TRUE) %>% 
      differentiate_managed_plots()
    
    forest_plot <- get_forest_plots()
    
    dataset <- read_forresdat("reg_by_plot_height_species", repo_path) %>% 
      select(-contains(c("lci", "mean", "uci", "subplot"))) %>% 
      filter(plottype == "CP" & plot_id %in% forest_plot$plot_id) %>% 
      differentiate_managed_plots() 
    
    n_plots_reg <- get_n_plots_per_reserve(dataset) # totaal aantal plots per BR
    
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
      inner_join(species_BR) %>% 
      mutate(rubbing_damage_perc = ifelse(approx_nr_regeneration_ha == 0 & rubbing_damage_perc == 0,
                                          NA,
                                          rubbing_damage_perc)
             )
    
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
      filter(!is.na(mean)) %>% 
      round_df(., 2) %>% 
      left_join(qHeightClass, by = c("height_class" = "ID")) %>% 
      left_join(qSpecies, by = c("species" = "ID")) %>% 
      mutate(strata = "height_class",
             stratum_name = heightclass_txt,
             strata2 = "species",
             stratum_name2 = name_sc
      ) %>% 
      get_year_range_reg() %>% 
      select(-contains(c("log", "height", "species", "name_sc")))
    
    # percentage plots waar soort per hoogteklasse voorkomt
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
      mutate(variable = "perc_plots_rejuvenation"
             , n_obs = n_max_plots
             , mean = perc_plots
             , variance = NA, lci = NA, uci = NA
             , strata = "height_class"
             , stratum_name = heightclass_txt
             , strata2 = "species"
             , stratum_name2 = name_sc) %>% 
      get_year_range_reg() %>% 
      select(-contains(c("height", "species", "name_sc", "plots")))
    
    resultaat <- rbind(resultaat1, resultaat2)
    return(resultaat)
}


#' create statistics on regeneration per forest reserve

statistics_regeneration <- function(repo_path = path_to_git_forresdat){
  
  reg_by_reserve <- statistics_reg()
  reg_by_height <- statistics_reg_height()
  reg_by_height_species <- statistics_reg_height_species()
  
  return(
    list(
      stat_reg = reg_by_reserve
      , stat_reg_by_height = reg_by_height
      , stat_reg_by_height_species = reg_by_height_species
    )
  )
}



#' create statistics per forest reserve, based on veg_by_plot
#' 
#' This function first selects all the circular, forested plots.
#' Then the managed part of 'Kluisbos' is changed into 'Kluisbos_managed' and 
#' 'Kluisbos_managed_non_intervention'.
#' Finally the function `create_statistics()` is used to create statistics on 
#' all of the variables in `veg_by_plot`.
#' 
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return statistics (mean, variance, lci, uci, n_obs) per period, 
#' forest_reserve and all of the variables included in 'veg_by_plot'
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_veg(datapackage)
#' }
#'
#' @importFrom functions get_forest_plots differentiate_managed_plots get_year_range_veg
#' @importFrom forrescalc read_resource create_statistics

statistics_veg <- function(repo_path = path_to_git_forresdat){
  forest_plot <- get_forest_plots()
  
  dataset <- read_forresdat("veg_by_plot", repo_path) %>% 
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
    get_year_range_veg() %>% 
    select(-contains(c("log")))
  
  return(resultaat)

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
#' @inheritParams get_plotinfo_cp_for_stat
#' 
#' @return mean cover of each species and percentage of the plots where the species 
#' occurs, per period and forest_reserve
#'
#' @examples
#' \dontrun{
#' datapackage <- read_forresdat(git_ref_type = "branch", git_reference = "develop")
#' resultaat <- statistics_herbs(datapackage)
#' }
#'
#' @importFrom functions get_forest_plots get_n_plots_per_reserve 
#' differentiate_managed_plots get_year_range_veg
#' @importFrom forrescalc read_resource create_statistics

repo_path <- path_to_git_forresdat

statistics_herbs <- function(repo_path = path_to_git_forresdat){
  
  con <- odbcConnectAccess2007(path_to_fieldmap_db)
  qHerbSpecies <- sqlFetch(con, "qHerbSpecies240810", stringsAsFactors = FALSE) %>% 
    select(ID, name_nl = Value1, name_sc = Value2)
  odbcClose(con)
  
  forest_plot <- get_forest_plots()
  
  # aantal plots obv veg-opname
  veg_by_plot <- read_forresdat("veg_by_plot", repo_path) %>% 
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
    mutate(variable = "perc_plots_species_herblayer"
           , n_obs = n_max_plots
           , mean = perc_plots
           , variance = NA, lci = NA, uci = NA
           , strata = "species"
           , stratum_name = name_sc
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
    mutate(variable = "characteristic_cover"
           , n_obs = n_max_plots
           , mean = karakt_bedekking
           , variance = NA, lci = NA, uci = NA
           , strata = "species"
           , stratum_name = name_sc
           , strata2 = NA
           , stratum_name2 = NA)  %>% 
    get_year_range_veg()
    
  resultaat1 <- resultaat1 %>% 
    select(-contains(c("plots", "species", "name_sc")))
  resultaat2 <- resultaat2 %>% 
    select(-contains(c("sum", "karakt", "plots", "species", "name_sc")))
    
  resultaat <- rbind(resultaat1, resultaat2)
  return(resultaat)
}


#' create statistics on vegetation per forest reserve

statistics_vegetation <- function(repo_path = path_to_git_forresdat){
  
  veg_by_reserve <- statistics_veg()
  herbs_by_reserve <- statistics_herbs()
  
  return(
    list(
      stat_veg = veg_by_reserve
      , stat_herbs = herbs_by_reserve
    )
  )
}


