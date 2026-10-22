# test load_data_shoots: extra var alive_dead_shoots
connect_to_database <-
  function(database) {
    
    if (grepl(".accdb$", database) || grepl(".mdb$", database)) {
      con <-
        DBI::dbConnect(
          odbc::odbc(),
          .connection_string =
            paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                   database)
        )
    } else if (grepl(".sqlite$", database)) {
      con <- DBI::dbConnect(SQLite(), database)
    } else if (grepl(".fdb$", database) || grepl(".gdb", database)) {
      con <-
        DBI::dbConnect(
          odbc::odbc(),
          .connection_string =
            paste0(
              "Driver={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", #nolint: line_length_linter
              database
            )
        )
    } else {
      stop(
        "This database type is not supported, please use .mdb, .accdb, .fdb, .gdb or .sqlite" #nolint: line_length_linter
      )
    }
    
    return(con)
  }




load_data_shoots_AL <- function(database, extra_variables = FALSE) {
  add_fields <-
    ifelse(
      extra_variables,
      ", Shoots.IUFROHght AS iufro_hght_shoots,
        Shoots.IUFROVital AS iufro_vital_shoots,
        Shoots.IUFROSocia AS iufro_socia_shoots,
        Shoots.Remark AS remark_shoots,
        Shoots.CommonRemark AS common_remark_shoots",
      ""
    )
  # in the below query, 'default values for columns are added to set the columns
  # in the correct order, they are overwritten later in the R script
  query_shoots <-
    "SELECT Shoots.IDPlots AS plot_id,
      99 AS period,
      Shoots.IDTrees%2$s AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.AliveDeadShoots AS alive_dead_shoots,
      Shoots.IntactSnag AS intact_snag,
      Shoots.DecayStage_Shoots AS decaystage %4$s
    FROM Shoots%2$s Shoots;"
  
  query_shoots_1986 <-
    "SELECT Shoots.IDPlots AS plot_id,
      0 AS period,
      Shoots.IDTrees_1986 AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.AliveDeadShoots AS alive_dead_shoots,
      Shoots.IntactSnag AS intact_snag,
      Shoots.DecayStage_Shoots AS decaystage
    FROM Shoots_1986 Shoots;"
  
  con <- connect_to_database(database)
  shoots_1986 <- DBI::dbGetQuery(con, query_shoots_1986) %>%
    mutate(period = 0)
  DBI::dbDisconnect(con)
  
  data_shoots <- query_database(database, query_shoots, add_fields = add_fields)
  
  if (nrow(shoots_1986) > 0) {
    data_shoots <- data_shoots %>%
      bind_rows(
        shoots_1986
      )
  }
  
  data_shoots <- data_shoots %>%
    mutate(
      intact_snag = ifelse(is.na(.data$intact_snag), 11, .data$intact_snag)
    )
  
  attr(data_shoots, "database") <-
    sub("^.*\\/(.*)\\/.*\\.\\w*$", "\\1", database)
  attr(data_shoots, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))
  
  return(data_shoots)
}
