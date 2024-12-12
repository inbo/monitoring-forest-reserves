
#####################################
#### FUNCTIES
#####################################


#' save results of calculations as csv-files
#'
#' This function saves the results from calculations in the forrescalc package (or any other named list with dataframes) in a predefined folder.  List item names will be used to name each of the tables, which contain as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a named list
#' @param output_dir name of output folder including path in which results should be saved
#'
#' @return No value is returned, data are saved in the specified folder
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_regeneration <-
#'   load_data_regeneration(database = path_to_fieldmap)
#' regeneration <- calculate_regeneration(data_regeneration)
#' save_results_csv(result = regeneration, output_dir = path_to_plotlevel_csv)
#' }


save_results_csv <- function(results, output_dir){
  for (tablename in names(results)) {
    write.csv2(results[[tablename]], file = paste0(output_dir, tablename, ".csv"))
  }
}



save_results_xlsx <- function(results, output_dir){
  for (tablename in names(results)) {
    write.xlsx2(results[[tablename]]
                , file = paste0(output_dir, tablename, ".xlsx")
                , showNA = FALSE)
                # , row.names = FALSE)
  }
}

save_results_tsv <- function(results, root = path_to_forresdat_data, strict = TRUE){
  sorting_max <-
    c("period", "year", "plot_id", "dbh_class_5cm", "decaystage", "subplot_id", 
      "tree_measure_id", "height_class", "species", "ID")
  sorting_max <-
    c("period", "year", "plot_id", "dbh_class_5cm", "decaystage", "subplot_id", "tree_measure_id", "height_class", "species", "ID")
  for (tablename in names(results)) {
    sorting <- sorting_max[sorting_max %in% colnames(results[[tablename]])]
    write_vc(results[[tablename]], tablename,
              root = root,
              sorting = sorting,
              strict = strict
              )
  }
}

#' retrieve height model data from (local) xlsx files
#'
#' This function groups the information on height models from the `.xlsx` files
#' in the given folder together in one dataframe.
#'
#' @param path_to_height_models path to folder where height models are stored
#'
#' @return Dataframe with height model data
#'
#' @importFrom dplyr %>% distinct mutate select transmute
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_extract str_split
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom readxl read_xlsx
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_height_models("C:/bosreservaten/Hoogtemodellen/")
#' }
#'
#' @export
#'
load_height_models_local <- function(path_to_height_models) {
  path_to_height_models <-
    ifelse(
      str_detect(path_to_height_models, "^(.+)\\/$"),
      path_to_height_models,
      paste0(path_to_height_models, "/")
    )
  heightmodels <-
    data.frame(
      filename = list.files(path = path_to_height_models, pattern = "xlsx")
    ) %>%
    mutate(
      no_extension = str_extract(.data$filename, "^(.+)(?=\\.)"),
      x = str_split(.data$no_extension, "_"),
      plottype = sapply(.data$x, `[`, 3),
      period = as.numeric(sapply(.data$x, `[`, 4)),
      path_file = paste0(path_to_height_models, .data$filename)
    ) %>%
    select(-.data$no_extension, -.data$x) %>%
    mutate(
      data = map(.data$path_file, add_models)
    ) %>%
    unnest(cols = c(.data$data)) %>%
    select(-.data$filename, -.data$path_file) %>%
    distinct()
  if (nrow(heightmodels) == 0) {
    warning("No height models (.xlsx files) found on the given path.")
  }
  
  return(heightmodels)
}


add_models <- function(path_file) {
  read_xlsx(path_file) %>%
    transmute(
      species =
        ifelse(
          is.na(.data$Species) | .data$Species == "<ALL>", -Inf, .data$Species
        ),
      species = as.numeric(.data$species),
      species = ifelse(.data$species == -Inf, NA_real_, .data$species),
      model = .data$Model,
      .data$P1, .data$P2,
      forest_reserve = .data$BR
    )
}


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
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
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
  odbcClose(con)
  if (!input %in% values) {
    stop(
      paste0(
        "Input value '", input, "' should be one of the following values:",
        paste0(values, collapse = ", ")
      )
    )
  }
}



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
      selection <- selection
    }
    return(selection)
  }

#' @title query all periods from fieldmap database
#'
#' @description
#' This helper function retrieves data of all periods from the given database
#' by using the given query and adding in the period dependent parameters.
#' Reason for this function is to avoid repetition in the load functions.
#'
#' @param query query that is adapted to be used in different periods,
#' in which the following parameters can be used: \itemize{
#'   \item `\%1$d` : replaced by the period number,
#'   \item `\%2$s` : replaced by an empty string in period 1 and by '_?eSet' in all other periods (with ? the period)
#'   \item `\%3$s` : replaced by the string given in argument selection
#'   \item `\%4$s` : replaced by the string given in argument add_fields
#'   \item `\%5$s` : replaced by the string given in argument conjunction
#' }
#' @param selection string that will be added to `\%3$s` in the query
#' @param add_fields string that will be added to `\%4$s` in the query
#' @param conjunction string that will be added to `\%5$s` in the query
#' @param n_periods highest period number that is present in the database
#' (change default here when a new period is added to the database)
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe containing the result of the query and an additional column
#' period
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom dplyr %>% bind_rows mutate

#' @importFrom rlang .data
#'

query_database <-
  function(database, query, selection = "", add_fields = "", conjunction = "",
           n_periods = 3) {
    
    con <- odbcConnectAccess2007(database)
    dataset <-
      sqlQuery(
        con,
        sprintf(query, 1, "", selection, add_fields, conjunction),
        stringsAsFactors = FALSE
      ) %>%
      mutate(
        period = 1
      )
    
    if (n_periods >= 2) {
      for (n in 2:n_periods) {
        dataset <- dataset %>%
          bind_rows(
            sqlQuery(
              con,
              sprintf(query, n, paste0("_", n, "eSet"), selection, add_fields, conjunction),
              stringsAsFactors = FALSE
            ) %>%
              mutate(
                period = n
              )
          )
      }
    }
    
    odbcClose(con)
    
    return(dataset)
  }



#' retrieve dendrometry data from fieldmap database - AANPASSING
#' --> ook mogelijk zonder dat plotdetails aanwezig zijn (left_join ipv inner_join)
#' (bv. voor opnames die niet in monitoring netwerk)
#' --> geen berekening van lokale X, Y: X, Y zoals in layer trees genoteerd
#' (KV's meestal globaal, CP's meestal lokaal)
#' 
#' 
#' This function queries the given database to retrieve data on dendrometry
#' (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#' @param extra_variables Should additional variables such as
#' x_m, y_m,
#' coppice_id, iufro_hght, iufro_vital, iufro_socia, remark and common_remark be added?
#' Default is FALSE (no).
#'
#' @return Dataframe with dendrometry data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate round_date year
#'

load_data_dendrometry_all <-
  function(database, plottype = NA, forest_reserve = NA, extra_variables = TRUE) {
    selection <-
      translate_input_to_selectionquery(database, plottype, forest_reserve)
    add_fields <-
      ifelse(
        extra_variables,
        ", Trees.X_m, Trees.Y_m,
        (Trees.X_m - Plots.Xorig_m) AS x_local, (Trees.Y_m - Plots.Yorig_m) AS y_local,
        Trees.CoppiceID AS coppice_id, Trees.IUFROHght AS iufro_hght,
        Trees.IUFROVital AS iufro_vital, Trees.IUFROSocia AS iufro_socia,
        Trees.Remark AS remark, Trees.CommonRemark AS common_remark",
        ""
      )
    query_dendro <-
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        Trees.OldID AS old_id,
        pd.ForestReserve AS forest_reserve,
        pd.Date_Dendro_%1$deSet AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.IntactSnag AS intact_snag,
        Trees.DecayStage AS decaystage,
        Trees.Calcheight_m AS calc_height_fm,
        cvr.Value3 AS crown_volume_reduction,
        blr.Value3 AS branch_length_reduction,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS tree_number %4$s
      FROM ((((Plots LEFT JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
        LEFT JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
        LEFT JOIN qCrownVolRedu cvr ON Trees.CrownVolumeReduction = cvr.ID)
        LEFT JOIN qBranchLenghtReduction blr ON Trees.BranchLengthReduction = blr.ID) %3$s;"
    
    data_dendro <-
      query_database(database, query_dendro,
                     selection = selection, add_fields = add_fields) %>%
      mutate(
        year = year(round_date(.data$date_dendro, "year")) - 1,
        subcircle =
          ifelse(
            .data$alive_dead == 11 & .data$dbh_mm >= .data$dbh_min_a4,
            "A4",
            ifelse(
              .data$alive_dead == 12 & .data$dbh_mm >= .data$dbh_min_a4_dead,
              "A4",
              "A3"
            )
          ),
        subcirclearea_ha =
          ifelse(
            .data$subcircle == "A4",
            (pi * .data$r_A4 ^ 2) / 10000,
            (pi * .data$r_A3 ^ 2) / 10000
          ),
        plotarea_ha =
          ifelse(
            .data$plottype == 20,
            .data$subcirclearea_ha,
            NA
          ),
        plotarea_ha =
          ifelse(
            .data$plottype == 30,
            (.data$length_core_area_m * .data$width_core_area_m) / 10000,
            .data$plotarea_ha
          ),
        plotarea_ha =
          ifelse(
            .data$plottype == 30 & is.na(.data$plotarea_ha),
            .data$core_area_ha,
            .data$plotarea_ha
          ),
        plotarea_ha =
          ifelse(
            is.na(.data$plotarea_ha),
            .data$totalplotarea_ha,
            .data$plotarea_ha
          ),
        dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm)
      )
    
    return(data_dendro)
  }


#' retrieve dendrometry data from fieldmap database - AANPASSING
#' --> ook mogelijk zonder plotdetails (left_join ipv inner_join)
#' --> geen berekening van lokale X, Y: X, Y zoals in layer trees genoteerd
#' (KV's meestal globaal, CP's meestal lokaal)
#' 
#' 
#' This function queries the given database to retrieve data on dendrometry
#' (ready for use in calculate_dendrometry function).
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#' @param extra_variables Should additional variables such as
#' x_m, y_m,
#' coppice_id, iufro_hght, iufro_vital, iufro_socia, remark and common_remark be added?
#' Default is FALSE (no).
#'
#' @return Dataframe with dendrometry data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate round_date year
#'

load_data_dendrometry_1986 <-
  function(database) {
    query_dendro <-
      "SELECT Plots.ID AS plot_id,
        Plots.Plottype AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
        Trees.ID AS tree_measure_id,
        Trees.OldID AS old_id,
        pd.ForestReserve AS forest_reserve,
        pd.Date_Dendro_1986 AS date_dendro,
        pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
        pd.TresHoldDBH_Trees_A3_alive AS dbh_min_a3,
        pd.TresHoldDBH_Trees_A3_dead AS dbh_min_a3_dead,
        pd.TresHoldDBH_Trees_A4_alive AS dbh_min_a4,
        pd.TresHoldDBH_Trees_A4_dead AS dbh_min_a4_dead,
        pd.TresHoldDBH_Trees_CoreArea_alive AS dbh_min_core_area,
        pd.TresHoldDBH_Trees_CoreArea_dead AS dbh_min_core_area_dead,
        pd.LengthCoreArea_m AS length_core_area_m,
        pd.WidthCoreArea_m AS width_core_area_m,
        pd.Area_ha AS core_area_ha,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.Species AS species,
        Trees.AliveDead AS alive_dead,
        Trees.IntactSnag AS intact_snag,
        Trees.DecayStage AS decaystage,
        Trees.Calcheight_m AS calc_height_fm,
        cvr.Value3 AS crown_volume_reduction,
        blr.Value3 AS branch_length_reduction,
        Trees.IndShtCop AS ind_sht_cop,
        Trees.TreeNumber AS tree_number,
        Trees.X_m, Trees.Y_m,
        (Trees.X_m - Plots.Xorig_m) AS x_local, 
        (Trees.Y_m - Plots.Yorig_m) AS y_local,
        Trees.CoppiceID AS coppice_id, Trees.IUFROHght AS iufro_hght,
        Trees.IUFROVital AS iufro_vital, Trees.IUFROSocia AS iufro_socia,
        Trees.Remark AS remark, Trees.CommonRemark AS common_remark
      FROM ((((Plots INNER JOIN Trees_1986 Trees ON Plots.ID = Trees.IDPlots)
        LEFT JOIN PlotDetails_1986 pd ON Plots.ID = pd.IDPlots)
        LEFT JOIN qCrownVolRedu cvr ON Trees.CrownVolumeReduction = cvr.ID)
        LEFT JOIN qBranchLenghtReduction blr ON Trees.BranchLengthReduction = blr.ID);"
    
    con <- odbcConnectAccess2007(database)
    
    data_dendro <-
      sqlQuery(
        con,
        query_dendro,
        stringsAsFactors = FALSE
      ) 
    
    odbcClose(con)
      
    data_dendro_ <- data_dendro %>%
      mutate(
        period = -1
      ) %>% 
      mutate(
        year = year(round_date(.data$date_dendro, "year")) - 1,
        plotarea_ha =
          ifelse(
            .data$plottype == 30,
            (.data$length_core_area_m * .data$width_core_area_m) / 10000,
            .data$plotarea_ha
          ),
        plotarea_ha =
          ifelse(
            .data$plottype == 30 & is.na(.data$plotarea_ha),
            .data$core_area_ha,
            .data$plotarea_ha
          ),
        plotarea_ha =
          ifelse(
            is.na(.data$plotarea_ha),
            .data$totalplotarea_ha,
            .data$plotarea_ha
          ),
        dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm)
      )
    
    return(data_dendro_)
  }


###------------------------------------------------
### Resultaten wegschrijven naar resultatendatabank - oaangepast, zonder bosreservaat
###-----------------------------------------------

#' Resultaten wegschrijven naar resultatendatabank
#'
#' Langere functiebeschrijving
#' @param results Resultaten van de functie My.WgtParEstimation
#' @param dbHandle Pad naar databank waarin resultaten worden opgeslagen, default gelijk aan "dbResultaten_path" (RODBC-connectie in functie zelf)
#' @param tblName Naam van tabel waarin resultaten worden opgeslagen, default gelijk aan "tblResultaten"
#' @param scriptName Naam van script
#' @param description Beschrijving van de analyse
#' @param request_from verwijzing naar project waarbinnen data-aanvraag kadert, standaard leeg (bv. NARA2020)
#' @param run_by geeft aan door wie analyse gerund is, en of het al dan niet een test-run betreft (bv. "run_AL", "run_LG", "test", "test_AL", "test_LG", ....)
#' @return
#' @importFrom
#' @examples

save_results_statistics <-function (results, 
                                 dbHandle = resultsdb, 
                                 tblName ="tblResultaten", 
                                 forest_reserve = NA,
                                 strata = NA, 
                                 stratumName = NA, 
                                 strata2 = NA, 
                                 stratumName2 = NA,
                                 scriptName=NA, 
                                 description, 
                                 request_from=NA, 
                                 run_by=NA)
{
  tblResults <-data.frame(variable=results$variable,
                          scriptName = scriptName,
                          date = date(),
                          description = description,
                          forest_reserve = forest_reserve,
                          strata=strata,
                          stratumName = stratumName,
                          strata2=strata2,
                          stratumName2 = stratumName2,
                          period = results$period,
                          nbObservations = results$n_obs,
                          gemiddelde = round(results$mean,4),
                          variantie=round(results$variance,4),
                          BI_ondergrens=round(results$lci,4),
                          BI_bovengrens=round(results$uci,4),
                          request_from = request_from,
                          run_by = run_by
  )
  
  connectieResultaten <- odbcConnectAccess2007(dbHandle) 
  
  listTbl<-sqlTables(connectieResultaten)
  
  # als tblResultaten nog niet is aangemaakt --> nieuwe tabel aanmaken
  if(!tblName %in% listTbl$TABLE_NAME){
    
    tblData <- sqlSave(connectieResultaten, tblResults, tblName)
    
  }  else { #als tabel bestaat records toevoegen aan tabel
    
    tblData <- sqlSave(connectieResultaten,tblResults, tblName,append = TRUE)
    
  }
  
  odbcClose(connectieResultaten)
  
} 



###------------------------------------------------
### Resultaten wegschrijven naar resultatendatabank - obv VBI - per bosreservaat
###-----------------------------------------------

#' Resultaten wegschrijven naar resultatendatabank
#'
#' Langere functiebeschrijving
#' @param results Resultaten van de functie My.WgtParEstimation
#' @param dbHandle Pad naar databank waarin resultaten worden opgeslagen, default gelijk aan "dbResultaten_path" (RODBC-connectie in functie zelf)
#' @param tblName Naam van tabel waarin resultaten worden opgeslagen, default gelijk aan "tblResultaten"
#' @param scriptName Naam van script
#' @param scriptLocation Naam van folder waar script opgeslagen is (vaste lijst)
#' @param type "D" voor design-based analyse en "M" voor model-based analyse
#' @param description Beschrijving van de analyse
#' @param datasource versie van de gebruikte analysedb bv. "VBI_Analysedatabank_v2019-11-07.accdb"
#' @param datasource_hash file-hash die aangemaakt wordt in het script om te verwijzen naar gebruikte analysedb
#' @param request_from verwijzing naar project waarbinnen data-aanvraag kadert, standaard leeg (bv. NARA2020)
#' @param run_by geeft aan door wie analyse gerund is, en of het al dan niet een test-run betreft (bv. "run_AL", "run_LG", "test", "test_AL", "test_LG", ....)
#' @return
#' @importFrom
#' @examples

My.ResultsToDatabase <-function (results, 
                                 dbHandle = dbResults, 
                                 tblName ="tblResultaten", 
                                 scriptName=NULL, 
                                 description, 
                                 request_from=NULL, 
                                 run_by=NULL)
{
  
  if(is.na(run_by)){
    x <- system("ipconfig", intern=TRUE)
    z <- x[grep("IPv4", x)]
    IP <- gsub(".*? ([[:digit:]])", "\\1", z)
    run_by <- IP
    warning('!! WARNING: "RUn" werd niet gespecifieerd --> Run_by = IP-adres')
  }  else { #als run_by wél gespecifieerd is 
    run_by <- run_by
  }
  

   tblResults <-data.frame(variable=results$variable,
                              scriptName = scriptName,
                              date = date(),
                              description = description,
                              forest_reserve = results$forest_reserve,
                              strata=results$strata,
                              stratumName = results$stratum_name,
                              strata2=results$strata2,
                              stratumName2 = results$stratum_name2,
                              period = results$period,
                              nbObservations = results$n_obs,
                              gemiddelde = round(results$mean,4),
                              variantie=round(results$variance,4),
                              BI_ondergrens=round(results$lci,4),
                              BI_bovengrens=round(results$uci,4),
                              request_from = request_from,
                              run_by = run_by
                            )
      
    connectieResultaten <- odbcConnectAccess2007(dbHandle) 
    
    listTbl<-sqlTables(connectieResultaten)
    
    # als tblResultaten nog niet is aangemaakt --> nieuwe tabel aanmaken
    if(!tblName %in% listTbl$TABLE_NAME){
      
      tblData <- sqlSave(connectieResultaten, tblResults, tblName)
      
    }  else { #als tabel bestaat records toevoegen aan tabel
      
      tblData <- sqlSave(connectieResultaten,tblResults, tblName,append = TRUE)
      
    }
    
    odbcClose(connectieResultaten)
    
} 


# VOLUMEBEREKENINGEN ------

# Functies om volumes te berekenen
# enkel snag wordt anders berekend dan in FM ...


# SNAG ----

# my.CalcVolSnag <-function(data_dendro, varNameDiameter="dbh_mm",varNameHeight="calc_height_m"){
#   
#   data_dendro_snag <- data_dendro %>% 
#     filter(intact_snag == 10) %>% 
#     select(period, plot_id, tree_measure_id, species, alive_dead, intact_snag, 
#            dbh_mm, height_m, calc_height_m)
#     mutate(i = )
#     
#     
#     
# }
# 
# 
# 
# (Eq. 1)
# where	dh – stem diameter at height h
# H – total height 
# d1.3 – diameter at breast height
# i, p, q – model parameters
# 
# 
# 
# 


# INLEZEN TARIEVEN ----------
# zie dbExterneData

query_tarieven2ing<-"
SELECT
tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value1
, tblTarieven_2ing.Tarief
, tblTarieven_2ing.groepNaam
, tblTarieven_2ing.a
, tblTarieven_2ing.b
, tblTarieven_2ing.c
, tblTarieven_2ing.d
, tblTarieven_2ing.e
, tblTarieven_2ing.f
, tblTarieven_2ing.g
, tblTarieven_2ing.Formule_type
FROM tblTariefgroepBoomsoort
INNER JOIN tblTarieven_2ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_2ing.groepID
;"

query_tarieven1ing<-"
SELECT tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value1
, tblTariefgroepBoomsoort.TariefID
, tblTarieven_1ing.groepNaam
, tblTarieven_1ing.a
, tblTarieven_1ing.b
, tblTarieven_1ing.c
, tblTarieven_1ing.d
, tblTarieven_1ing.Tarief
FROM tblTariefgroepBoomsoort
LEFT JOIN tblTarieven_1ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_1ing.groepID
;"

query_tarieven1ingKroon<-"
SELECT tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value1
, tblTariefgroepBoomsoort.TariefID
, tblTarieven_1ingKroon.groepNaam
, tblTarieven_1ingKroon.a
, tblTarieven_1ingKroon.b
, tblTarieven_1ingKroon.c
, tblTarieven_1ingKroon.d
, tblTarieven_1ingKroon.Tarief
FROM tblTariefgroepBoomsoort
LEFT JOIN tblTarieven_1ingKroon ON tblTariefgroepBoomsoort.TariefID = tblTarieven_1ingKroon.groepID
;"


connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file

cat("\n\n tarieven 2 ing\n--------------------------------\n")
tarieven2ingOrig <- sqlQuery(connectieExterneData, query_tarieven2ing, stringsAsFactors = TRUE);print(str(tarieven2ingOrig ))

cat("\n\n tarieven 1 ing\n--------------------------------\n")
tarieven1ingOrig <- sqlQuery(connectieExterneData, query_tarieven1ing, stringsAsFactors = TRUE);print(str(tarieven1ingOrig ))

cat("\n\n tarieven 1 ing kroonhout\n--------------------------------\n")
tarieven1ingKroonOrig <- sqlQuery(connectieExterneData, query_tarieven1ingKroon, stringsAsFactors = TRUE);print(str(tarieven1ingKroonOrig ))

#
# boomEigenschapOrig<-sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)

odbcClose(connectieExterneData)

tarieven1ing<-plyr::rename(tarieven1ingOrig,c(ID="species",Value1="NameNl"))
tarieven2ing<-plyr::rename(tarieven2ingOrig,c(ID="species",Value1="NameNl"))
tarieven1ingKroon <- plyr::rename (tarieven1ingKroonOrig,c(ID="species",Value1="NameNl"))



####----------------------------------------------------------------
#### Functie die grondvlak berekent en volume waarbij tarieven en aantal ingangen worden gespecifieerd. De berekeningen gebeuren op basis van een data.frame met de velden 'Perimeter_cm' en 'Height_m' (in geval van 2 ingangen)
####----------------------------------------------------------------

# FUNCTIE
my.CalcVolBA<-function(treeMeasurements,tarieven,nIngang,varNameDiameter="dbh_mm",varNameHeight="calc_height_m"){
  
  tarieven<-tarieven[,names(tarieven) != "NameNl"]
  trees<-merge(treeMeasurements,tarieven,by="species",all.x=TRUE)
  
  #Hulpvariabelen
  perimeter <- pi*trees[,varNameDiameter]/10
  radius_m <- trees[,varNameDiameter]/2000
  
  #Hulpvariabelen bewaard in dataset
  trees$basal_area_m2 <- pi * radius_m^2
  trees$D <- trees[,varNameDiameter]/10
  
  if (nIngang==2){
    
    trees$vol_stem_m3 <-
      ifelse( trees$Formule_type == 1,
              yes =
                trees$a + trees$b * perimeter +
                trees$c *(perimeter^2)+ trees$d *(perimeter^3) +
                trees$e*trees[,varNameHeight] + trees$f*trees[,varNameHeight]* perimeter +
                trees$g*trees[,varNameHeight]*(perimeter^2),
              no =
                1/1000 *
                #spil
                (exp(1.10597 * log(trees[,varNameHeight]) + 1.78865 * log(trees$D) - 3.07192) -
                   #Verlies
                   exp(-4.608923 * log(trees$D) + 3.005989 * log(trees[,varNameHeight]) -
                         1.3209 * log(trees[,varNameHeight])*log(trees[,varNameHeight])+ 1.605266 * log(trees$D) * log(trees[,varNameHeight]) + 5.410272))
      )
    
    
  } else if (nIngang==1){
    trees$vol_stem_m3<- trees$a + trees$b * perimeter + trees$c *(perimeter^2)+ trees$d *(perimeter^3)
  } else {
    trees$vol_stem_m3 = NaN
  }
  
  trees<-trees[,!names(trees) %in% c("a","b","c","d","e","f","g","Formule_type","Tarief","groepNaam", "TariefID", "D")]
  trees$vol_stem_m3<-pmax(0,trees$vol_stem_m3)
  trees
}




####----------------------------------------------------------------
#### Functie die volume van het (zwaar) kroonhout berekent, waarbij tarief wordt gespecifieerd.
# De berekeningen gebeuren op basis van een data.frame met het veld 'Perimeter_cm'
####----------------------------------------------------------------

my.CalcVolBranches<-function(treeMeasurements,tarieven,varNameDiameter="dbh_mm",varNameHeight="calc_height_m"){
  
  tarieven<-tarieven[,names(tarieven) != "NameNl"]
  trees<-merge(treeMeasurements,tarieven,by="species",all.x=TRUE)
  
  #Hulpvariabelen
  perimeter <- pi*trees[,varNameDiameter]/10
  radius_m <- trees[,varNameDiameter]/2000
  
  #Hulpvariabelen bewaard in dataset
  trees$vol_crown_m3<- trees$a + trees$b * perimeter + trees$c *(perimeter^2)+ trees$d *(perimeter^3)
  
  trees<-trees[,!names(trees) %in% c("a","b","c","d","Tarief","groepNaam", "TariefID", "D")]
  trees$vol_crown_m3<-pmax(0,trees$vol_crown_m3)
  trees
}



####-------------------------------------------------------------------------------
#### Functie om proportie te berekenen van gemiddelde van bepaalde variabele (bv. grondvlak) voor een soort t.o.v. het gemiddelde van die variabele voor alle soorten (komt overeen met proportie tussen totaal per soort en totaal over alle soorten)
####------------------------------------------------------------------------------

#' Proportie berekenen van totaal per soort en totaal over alle soorten (bv. grondvlak)
#'
#' Langere functiebeschrijving
#' @param data invoer
#' @param spName soortnaam
#' @param variableName grondvlak of volume of ....
#' @param maxReeks
#' @return
#' @importFrom
#' @examples

propMeasure_Species <- function(data, spName, variableName){
  
  data$SpeciesCalc <- spName
  
  data$Measure <- data[,variableName]

  species_Measure <- data %>% 
    group_by(plot_id, period, year) %>% 
    summarise(Measure_species = sum( Measure* (speciesTxt == SpeciesCalc)  ),
              Measure_allSpecies = sum(Measure)) %>% 
    ungroup()
  
  result.species <- create_statistics(species_Measure,
                                      level = c("period"),
                                      variables = c("Measure_species"))

  
  result.species$strata <- "treeSpecies"
  result.species$stratumNaam <- spName
  result.species$variable <- variableName
  
  
  result.allSpecies <- create_statistics(species_Measure,
                                         level = c("period"),
                                         variables = c("Measure_allSpecies"))
  
  result.prop <- data.frame(variable = paste("Prop_",variableName,sep=""),
                            strata = "treeSpecies",
                            stratumNaam = spName,
                            period = result.species$period,
                            n_obs = result.species$n_obs
  )
  
  result.prop$mean <- result.species$mean/result.allSpecies$mean
  
  # f = x/y   --> (sigma_f/f)^2 =(sigma_x/x)^2  + (sigma_y/y)^2 - 2cov_xy/x/y
  
  deel1 <- result.species$variance/(result.species$mean)^2
  deel2 <- result.allSpecies$variance/(result.allSpecies$mean)^2
  
  cov_period1 <- cov(species_Measure[species_Measure$period==1,]$Measure_species,species_Measure[species_Measure$period==1,]$Measure_allSpecies)
  cov_period2 <- cov(species_Measure[species_Measure$period==2,]$Measure_species,species_Measure[species_Measure$period==2,]$Measure_allSpecies)
  
  deel3 <- 2* c(cov_period2,cov_period1)/result.species$mean/result.allSpecies$mean
  
  # sigma_f = ((deel1 + deel2)^0.5) * f
  
  result.prop$variance <- (deel1 + deel2 - deel3) * (result.prop$mean)^2
  
  result.prop$variance <- ifelse(result.prop$variance > 0, result.prop$variance, result.species$variance/((result.allSpecies$mean)^2))
  
  #test.variance <- result.species$variance/result.allSpecies$mean^2
  
  result.prop$mean <- result.prop$mean*100
  
  result.prop$variance <- result.prop$variance*10000
  
  n <- c(nrow(species_Measure %>% filter(period==2)),nrow(species_Measure %>% filter(period==1)))
  
  result.prop$lci <- round(pmax(result.prop$mean - 1.96*((result.prop$variance)^0.5)/(n^0.5),0),2)
  result.prop$uci <- round(result.prop$mean + 1.96*((result.prop$variance)^0.5)/(n^0.5),2)
  
  result.prop$mean <- round(result.prop$mean,2)
  
  # result.species$mean <- round(result.species$mean,2)
  # result.species$lci <- round(result.species$lci,2)
  # result.species$uci <- round(result.species$uci,2)
  # result <- rbind(result.species,result.prop)
  
  result <- result.prop
  # want grondvlak op zich zegt niet veel (bv. zomereik 7 (vs 29 totaal))
  
  return(result)
  
}




####-------------------------------------------------------------------------------
#### Functie om proportie te berekenen van gemiddelde van bepaalde variabele (bv. grondvlak) voor een soort t.o.v. het gemiddelde van die variabele voor alle soorten (komt overeen met proportie tussen totaal per soort en totaal over alle soorten)
####------------------------------------------------------------------------------

#' Proportie berekenen van totaal per soort en totaal over alle soorten (bv. grondvlak)
#' EN ook het gemiddelde per soort (bv. BA en prop_BA)
#'
#' Langere functiebeschrijving
#' @param data invoer
#' @param spName soortnaam
#' @param variableName grondvlak of volume of ....
#' @param maxReeks
#' @return
#' @importFrom
#' @examples

propMeasure_Species2 <- function(data, spName, variableName){
  
  data$SpeciesCalc <- spName
  
  data$Measure <- data[,variableName]
  
  species_Measure <- data %>% 
    group_by(plot_id, period, year) %>% 
    summarise(Measure_species = sum( Measure* (speciesTxt == SpeciesCalc)  ),
              Measure_allSpecies = sum(Measure)) %>% 
    ungroup()
  
  result.species <- create_statistics(species_Measure,
                                      level = c("period"),
                                      variables = c("Measure_species"))
  
  
  result.species$strata <- "treeSpecies"
  result.species$stratumNaam <- spName
  result.species$variable <- variableName
  
  
  result.allSpecies <- create_statistics(species_Measure,
                                         level = c("period"),
                                         variables = c("Measure_allSpecies"))
  
  result.prop <- data.frame(variable = paste("Prop_",variableName,sep=""),
                            strata = "treeSpecies",
                            stratumNaam = spName,
                            period = result.species$period,
                            n_obs = result.species$n_obs
  )
  
  result.prop$mean <- result.species$mean/result.allSpecies$mean
  
  # f = x/y   --> (sigma_f/f)^2 =(sigma_x/x)^2  + (sigma_y/y)^2 - 2cov_xy/x/y
  
  deel1 <- result.species$variance/(result.species$mean)^2
  deel2 <- result.allSpecies$variance/(result.allSpecies$mean)^2
  
  cov_period1 <- cov(species_Measure[species_Measure$period==1,]$Measure_species,species_Measure[species_Measure$period==1,]$Measure_allSpecies)
  cov_period2 <- cov(species_Measure[species_Measure$period==2,]$Measure_species,species_Measure[species_Measure$period==2,]$Measure_allSpecies)
  
  deel3 <- 2* c(cov_period2,cov_period1)/result.species$mean/result.allSpecies$mean
  
  # sigma_f = ((deel1 + deel2)^0.5) * f
  
  result.prop$variance <- (deel1 + deel2 - deel3) * (result.prop$mean)^2
  
  result.prop$variance <- ifelse(result.prop$variance > 0, result.prop$variance, result.species$variance/((result.allSpecies$mean)^2))
  
  #test.variance <- result.species$variance/result.allSpecies$mean^2
  
  result.prop$mean <- result.prop$mean*100
  
  result.prop$variance <- result.prop$variance*10000
  
  n <- c(nrow(species_Measure %>% filter(period==2)),nrow(species_Measure %>% filter(period==1)))
  
  result.prop$lci <- round(pmax(result.prop$mean - 1.96*((result.prop$variance)^0.5)/(n^0.5),0),2)
  result.prop$uci <- round(result.prop$mean + 1.96*((result.prop$variance)^0.5)/(n^0.5),2)
  
  result.prop$mean <- round(result.prop$mean,2)
  
  result.species$mean <- round(result.species$mean,2)
  result.species$lci <- round(result.species$lci,2)
  result.species$uci <- round(result.species$uci,2)
  result <- rbind(result.species,result.prop)
  
  return(result)
  
}
