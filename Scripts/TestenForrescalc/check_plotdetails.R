source(here::here("scripts/Setup.R"))

library(DBI)
library(RSQLite)

database <- "C:/03_BR/2_Forrescalc/inst/example/testdb/mdb_bosres_test.sqlite"


con <- dbConnect(RSQLite::SQLite(), database)
con <- odbcConnectAccess2007(path_to_fieldmap_db)



# check_data_plotdetails <- function(database, forest_reserve = "all") {
#   selection <-
#     ifelse(
#       forest_reserve == "all", "",
#       paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
#     )


# sqlite lukt niet, checken met echte db (fieldmap) - lukt wel 
# selectie voor periode 1

  
  query_plotdetails <-
    "SELECT pd.IDPlots As plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Date_Dendro_1eSET AS date_dendro,
      pd.FieldTeam_Dendro_1eSET AS fieldteam,
      pd.rA1 AS ra1,
      pd.rA2 AS ra2,
      pd.rA3 AS ra3,
      pd.rA4 AS ra4,
      pd.LengthCoreArea_m AS length_core_area_m,
      pd.WidthCoreArea_m AS width_core_area_m,
      pd.Area_ha AS area_ha
    FROM (Plots
        INNER JOIN Plotdetails_1eSET pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"
  
  data_plotdetails <-
    sqlQuery(con, query_plotdetails) %>% 
    mutate(period = 1)
  
  incorrect_plotdetails <- data_plotdetails %>%
    group_by(.data$forest_reserve, .data$period, .data$plottype) %>%
    mutate(
      forest_reserve_date = median(.data$date_dendro)
    ) %>%
    ungroup() %>%
    mutate(
      field_forest_reserve =
        ifelse(is.na(.data$forest_reserve), "missing", NA),
      field_date_dendro =
        ifelse(is.na(.data$date_dendro) &
                 (.data$survey_trees == 10 | .data$survey_deadw == 10)
               , "missing", NA),
      field_date_dendro =
        ifelse(
          is.na(.data$field_date_dendro) &
            year(.data$date_dendro) != year(.data$forest_reserve_date),
          "deviating",
          .data$field_date_dendro
        ),
      field_fieldteam = ifelse(is.na(.data$fieldteam) &
                                 (.data$survey_trees == 10 |
                                    .data$survey_deadw == 10)
                               , "missing", NA),
      field_ra1 =
        ifelse(is.na(.data$ra1) & .data$plottype == "CP" &
                 .data$survey_reg == 10
               , "missing", NA),
      field_ra2 =
        ifelse(is.na(.data$ra2) & .data$plottype == "CP" &
                 .data$survey_reg == 10
               , "missing", NA),
      field_ra3 =
        ifelse(is.na(.data$ra3) & .data$plottype == "CP" &
                 (.data$survey_trees == 10 | .data$survey_deadw == 10)
               , "missing", NA),
      field_ra4 =
        ifelse(is.na(.data$ra4) & .data$plottype == "CP" &
                 (.data$survey_trees == 10 | .data$survey_deadw == 10)
               , "missing", NA),
      field_length_core_area_m =
        ifelse(
          is.na(.data$length_core_area_m) & .data$plottype == "CA" &
            (.data$survey_trees == 10 | .data$survey_deadw == 10
             | .data$survey_reg == 10 | .data$survey_veg == 10)
          , "missing",
          NA
        ),
      field_width_core_area_m =
        ifelse(
          is.na(.data$width_core_area_m) & .data$plottype == "CA" &
            (.data$survey_trees == 10 | .data$survey_deadw == 10
             | .data$survey_reg == 10 | .data$survey_veg == 10)
          , "missing",
          NA
        ),
      field_area_ha =
        ifelse(is.na(.data$area_ha) & .data$plottype == "CA" &
                 (.data$survey_trees == 10 | .data$survey_deadw == 10
                  | .data$survey_reg == 10 | .data$survey_veg == 10)
               , "missing", NA)
    ) %>%
    select(-"survey_trees", -"survey_deadw", -"survey_reg", -"survey_veg") %>%
    pivot_longer(
      cols = c(starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field),
      plottype = NULL,
      forest_reserve = NA_character_,
      date_dendro = as.character(.data$date_dendro),
      fieldteam = as.character(.data$fieldteam),
      forest_reserve_date = NULL,
      across(starts_with("ra"), as.character),
      across(ends_with("_core_area_m"), as.character),
      area_ha = as.character(.data$area_ha)
    ) %>%
    pivot_longer(
      cols = !c("plot_id", "period", "aberrant_field", "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(.data$aberrant_field == .data$varname) %>%
    select(-"varname")
  
  return(incorrect_plotdetails)
}
