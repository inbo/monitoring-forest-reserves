#' check userinput and convert to final part of query
#'
#' Internal helper function that checks if the input values are present in the
#' fieldmap database (with help of function check_input()) and translates
#' this input to a query part that makes the desired selection if pasted as a
#' final part of a query.  This query can contain a join!
#'
#' @inheritParams load_data_dendrometry
#'
#' @noRd
#'
#'
translate_input_to_selectionquery <-
  function(database, plottype, forest_reserve) {
    if (!is.na(plottype)) {
      check_input(plottype, database, "qPlotType", "Value2")
      selection <-
        paste0(
          " INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID",
          " WHERE qPlotType.Value2 in ('", plottype, "')")
    } else {
      selection <- ""
    }
    if (!is.na(forest_reserve)) {
      check_input(
        forest_reserve, database, "PlotDetails_1eSet", "ForestReserve",
        "PlotDetails_2eSet"
      )
      if (selection == "") {
        selection <- "WHERE"
      } else {
        selection <- paste(selection, "AND")
      }
      selection <-
        paste0(selection, " pd.ForestReserve in ('", forest_reserve, "')")
    } else {
      selection <- ""
    }
    return(selection)
}
