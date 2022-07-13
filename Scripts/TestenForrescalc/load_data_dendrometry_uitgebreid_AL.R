#' retrieve dendrometry data from fieldmap database
#'
#' This function queries the given database to retrieve data on dendrometry (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap structure) including path
#'
#' @return Dataframe with dendrometry data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows mutate
#' @importFrom lubridate round_date year
#'
load_data_dendrometry <- function(database) {
  query_dendro <-
    "SELECT Plots.ID AS plot_id,
      IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS Area_ha,
      Trees.ID AS tree_measure_id,
      pd.ForestReserve,
      pd.Date_dendro_1eSet AS date_dendro,
      Trees.DBH_mm,
      Trees.Height_m,
      Trees.Species AS species,
      Trees.AliveDead,
      Trees.DecayStage AS decaystage,
      Trees.Adjust_Vol_tot_m3,
      Trees.AdjustBasalArea_m2,
      Trees.TreeNUmber,
      Trees.X_m,
      Trees.Y_m,
      Plots.Xorig_m,
      Plots.Yorig_m
    FROM (Plots INNER JOIN Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN PlotDetails_1eSet pd ON Plots.ID = pd.IDPlots
    WHERE Plots.Plottype = 20;"

  query_dendro2 <-
    "SELECT Plots.ID AS plot_id,
      IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS Area_ha,
      Trees.ID AS tree_measure_id,
      pd.ForestReserve,
      pd.Date_dendro_2eSet AS date_dendro,
      Trees.DBH_mm,
      Trees.Height_m,
      Trees.Species AS species,
      Trees.AliveDead,
      Trees.DecayStage AS decaystage,
      Trees.Adjust_Vol_tot_m3,
      Trees.AdjustBasalArea_m2,
      Trees.OldID,
      Trees.TreeNUmber,
      Trees.X_m,
      Trees.Y_m,
      Plots.Xorig_m,
      Plots.Yorig_m
    FROM (Plots INNER JOIN Trees_2eSET Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN PlotDetails_2eSet pd ON Plots.ID = pd.IDPlots
    WHERE Plots.Plottype = 20;"

  con <- odbcConnectAccess2007(database)
  data_dendro <- sqlQuery(con, query_dendro, stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, query_dendro2, stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    mutate(
      year = year(round_date(.data$date_dendro, "year")) - 1,
      plottype =
        ifelse(
          .data$AliveDead == 11 & .data$DBH_mm >= 400,
          "A4",
          ifelse(
            .data$AliveDead == 12 & .data$DBH_mm >= 100,
            "A4",
            "A3"
          )
        ),
      X_local = X_m - Xorig_m,  # Xorig_m = 0 als X in lokale coordinaten staat
      Y_local = Y_m - Yorig_m,
      plotarea_ha =
        ifelse(
          .data$plottype == "A4",
          (pi * 18 ^ 2)/10000,
          (pi * 9 ^ 2)/10000
        )
    )
  odbcClose(con)

  return(data_dendro)
}
