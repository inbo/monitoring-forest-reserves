
extra_variables <- TRUE

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
query_shoots <-
  "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees%2$s AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.IntactSnag AS intact_snag,
      Shoots.DecayStage_Shoots AS decaystage %4$s
    FROM Shoots%2$s Shoots;"

query_shoots_1986 <-
  "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees_1986 AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_mm AS dbh_mm,
      Shoots.Height_m AS height_m,
      Shoots.IntactSnag AS intact_snag,
      Shoots.DecayStage_Shoots AS decaystage
    FROM Shoots_1986 Shoots;"

con <- odbcConnectAccess2007(path_to_fieldmap_db)
shoots_1986 <- sqlQuery(con, query_shoots_1986
                        , stringsAsFactors = FALSE
                        # , add_fields = add_fields
                        ) %>%
  mutate(period = 0)
odbcClose(con)

data_shoots <- query_database(path_to_fieldmap_db, query_shoots, add_fields = add_fields)

if (nrow(shoots_1986) > 0) {
  data_shoots <- data_shoots %>%
    bind_rows(
      shoots_1986
    )
}

data_shoots <- data_shoots %>%
  mutate(intact_snag = ifelse(is.na(.data$intact_snag), 11, .data$intact_snag))

return(data_shoots)