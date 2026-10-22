
##########################################################################
#### Functies ikv aanmaak dhcurves ----
#########################################################################

# OPGEPAST ------

# Vooraf Omtrek en logOmtrek berekenen

# stems <- stems_height %>% 
#   mutate(
#     Omtrek = pi*.data$dbh_mm/1000,
#     logOmtrek = log(.data$Omtrek),
#     logOmtrek2 = .data$logOmtrek ^ 2)


# FUNCTIES -------------------------

#' Fit model based on all available periods, no difference between periods
#' 
#' This function creates a dataframe with  amodel per forest_reserve and species
#' Input dataframe has to include the variables forest_reserve, species, dbh_mm 
#' and height_m
#'
#'
#' @return dataframe with a model per forest_reserve and species
#'
#' @examples
#' \dontrun{
#' model_all <- fit.all(stems)
#' }
#'
fit.all <- function(trees) {
  
  mod_fun <- function(df) {
    lm(
      height_m ~ logOmtrek + logOmtrek2,
      data = df
    )
  }
  
  model <- trees %>%
    group_by(.data$species, .data$forest_reserve) %>%
    nest() %>%
    mutate(
      Model = map(.data$data, mod_fun)
    ) %>%
    ungroup() %>%
    select(-"data")
  
  return(model)
}


#' Fit model based on all available periods, no difference between periods
#' 
#' This function creates a dataframe with  amodel per forest_reserve and species
#' Input dataframe has to include the variables forest_reserve, period, species, 
#' dbh_mm and height_m
#'
#'
#' @return dataframe with a model per forest_reserve, period and species
#'
#' @examples
#' \dontrun{
#' model_per_period <- fit.period(stems)
#' }
#'
fit.period <- function(trees) {
  
  mod_fun <- function(df) {
    lm(
      height_m ~ logOmtrek + logOmtrek2,
      data = df
    )
  }
  
  model <- trees %>%
    group_by(.data$species, .data$forest_reserve, .data$period) %>%
    nest() %>%
    mutate(
      Model = map(.data$data, mod_fun)
    ) %>%
    ungroup() %>%
    select(-"data")
  
  return(model)
}


#' @title Basisstap om modelparameters te extraheren uit een dataframe met modellen 
#' aangemaakt met functies `fit.all()` of `fit.period()`
#' (één rij/één model model per soort, forest_reserve en al dan niet per periode)
#'
#' @description
#' Functie die de modelparameters A, B en C in een dataframe giet op basis van 
#' een opgegeven model.  
#'
#' @param Soortmodel model voor boomsoort-domeincombinatie
#'
#' @return dataframe met parameters voor dh-model (`Ad`, `Bd` en `Cd`)
#'
#' @noRd
#'
#' @importFrom stats coef
#'

modelparameters.basisstap <- function(Soortmodel) {
  
  Parameters.soort <- data.frame(Ad = coef(Soortmodel)[[1]],
                                 Bd = coef(Soortmodel)[[2]],
                                 Cd = coef(Soortmodel)[[3]],
                                 stringsAsFactors = FALSE)
  
  return(Parameters.soort)
}


#' @title Haalt modelparameters uit dataframe met modellen per reservaat en species
#'
#' @description
#' Functie die de modelparameters berekent op basis van een opgegeven
#' model 
#'
#' (Deze functie verwijst naar de interne functie `modelparameters.basisstap()`)
#'
#' @param model_all model per `forest_reserve` en `species` 
#' @param Data meetgegevens
#'
#' @return Dataframe met parameters voor het dh-model (`Ad`, `Bd` en `Cd`) 
#' 
#' @importFrom dplyr %>% rowwise do inner_join group_by ungroup select
#' distinct
#' @importFrom plyr .
#' @importFrom rlang .data
#'
#'#' @examples
#' \dontrun{
#'parameters_all <- modelparameters_all(model_all, Data = stems)
# stems_modeled_height <- stems %>% 
#   left_join(parameters_per_period, by = c("species", "forest_reserve")) %>% 
#   mutate(hoogte_obv_all = Ad + Bd*logOmtrek + Cd*logOmtrek2)
#'} 
#'
modelparameters_all <- function(model_all, Data = NULL) {
  Parameters <- model_all %>%
    # onderstaande inner_join is strikt genomen niet nodig, maar maakt wel duidelijk dat 
    # je best zelfde data gebruikt om model aan te maken, en om er dan de parameters 
    # aan te koppelen
    inner_join(
      x = Data,
      by = c("species", "forest_reserve")
    ) %>%
    group_by(
      .data$species,
      .data$forest_reserve
    ) %>%
    do(
      modelparameters.basisstap(.$Model[[1]])
    ) %>%
    ungroup()
  return(Parameters)
}


#' @title Haalt modelparameters uit dataframe met modellen per reservaat, periode
#' en species
#'
#' @description
#' Functie die de modelparameters berekent op basis van een opgegeven
#' model
#'
#' (Deze functie verwijst naar de interne functie `modelparameters.basisstap()`)
#'
#' @param model_per_period model per `forest_reserve`, `period` en `species` 
#' @param Data meetgegevens
#'
#' @return Dataframe met parameters voor het dh-model (`Ad`, `Bd` en `Cd`) 
#' 
#' @importFrom dplyr %>% rowwise do inner_join group_by ungroup select
#' distinct
#' @importFrom plyr .
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'parameters_per_period <- modelparameters_per_period(model_per_period, Data = stems)
# stems_modeled_height2 <- stems %>% 
#   left_join(parameters_per_period, by = c("species", "forest_reserve", "period")) %>% 
#   mutate(hoogte_per_period = Ad + Bd*logOmtrek + Cd*logOmtrek2)
#'} 
#'
modelparameters_per_period <- function(model_per_period, Data = NULL) {
  Parameters <- model_per_period %>%
    # onderstaande inner_join is strikt genomen niet nodig, maar maakt wel duidelijk dat 
    # je best zelfde data gebruikt om model aan te maken, en om er dan de parameters 
    # aan te koppelen
    inner_join(
      x = Data,
      by = c("species", "forest_reserve", "period")
    ) %>%
    group_by(
      .data$species,
      .data$forest_reserve,
      .data$period
    ) %>%
    do(
      modelparameters.basisstap(.$Model[[1]])
    ) %>%
    ungroup()
  return(Parameters)
}

