source(here::here("scripts/Setup.R"))

library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), database)
con <- odbcConnectAccess2007(path_to_fieldmap_db)

# sqlite lukt niet, checken met echte db (fieldmap) - lukt wel 
# selectie voor periode 2

# check_data_regeneration <- function(database, forest_reserve = "all") {
#   selection <-
#     ifelse(
#       forest_reserve == "all", "",
#       paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
#     )
  query_regeneration <-
    "SELECT g.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Regeneration_YN AS survey_reg,
      g.ID AS subplot_id,
      g.Date AS date_,
      g.Fieldteam AS fieldteam
    FROM ((Plots
        INNER JOIN Regeneration_2eSet g ON Plots.ID = g.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID)
      INNER JOIN Plotdetails_2eSet pd ON Plots.ID = pd.IDPlots;"
  
  data_regeneration <- sqlQuery(con, query_regeneration) %>%
    mutate(period = 2)
  
  incorrect_regeneration <- data_regeneration %>%
    filter(.data$survey_reg == 10) %>%
    select(-"survey_reg") %>%
    group_by(.data$forest_reserve, .data$period, .data$plottype) %>%
    mutate(
      forest_reserve_date = median(.data$date_)
    ) %>%
    ungroup() %>%
    mutate(
      field_date = ifelse(is.na(.data$date_), "missing", NA),
      field_date =
        ifelse(
          is.na(.data$field_date) &
            year(.data$date_) != year(.data$forest_reserve_date),
          "deviating",
          .data$field_date
        ),
      field_fieldteam = ifelse(is.na(.data$fieldteam), "missing", NA)
    ) %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL,
      date = as.character(.data$date_),
      date_ = NULL,
      forest_reserve_date = NULL,
      fieldteam = as.character(.data$fieldteam)
    ) %>%
    pivot_longer(
      cols = !c("plot_id", "subplot_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(
      .data$aberrant_field == .data$varname
    ) %>%
    select(-"varname")
  
#   return(incorrect_regeneration)
# }
