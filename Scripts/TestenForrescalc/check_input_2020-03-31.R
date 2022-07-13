#' check input value based on column in fieldmap database
#'
#' Internal helper function that checks if the input value is present in the
#' given table and column of the fieldmap database (e.g. possible values in a
#' lookup table).  This function drops an error if the value is not present.
#'
#' @param input value to check if it is present
#' @param table table name of the fieldmap database
#' @param column column that mentions all possible values of the variable
#' @param table2 possibility for 2nd table name (e.g. if data from 2 different
#' periods are in different tables but the column has the same name)
#' @inheritParams load_data_dendrometry
#'
#' @noRd
#'
#' @importFrom RODBC odbcConnectAccess2007 sqlQuery
#'
#'
check_input <- function(input, database, table, column, table2 = NA) {
  query <- paste("SELECT DISTINCT", column, "FROM", table)
  con <- odbcConnectAccess2007(database)
  values <- sqlQuery(con, query, stringsAsFactors = FALSE)[, column]
  if (!is.na(table2)) {
    query2 <- paste("SELECT DISTINCT", column, "FROM", table)
    values <-
      unique(
        c(values, sqlQuery(con, query2, stringsAsFactors = FALSE)[, column])
      )
  }
  if (!input %in% values) {
    stop(
      paste0(
        "Input value '", input, "' should be one of the following values:",
        paste0(values, collapse = ", ")
      )
    )
  }
}
