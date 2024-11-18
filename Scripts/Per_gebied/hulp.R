
plotinfo <- load_plotinfo(database = path_to_fieldmap_db)







translate_input_to_selectquery <-
  function(database, plottype, forest_reserve, processed, survey_name) {
    if (!is.na(plottype)) {
      check_input(plottype, database, "qPlotType", "Value3")
      selection <-
        paste0(" WHERE qPlotType.Value3 in ('", plottype, "')")
    } else {
      selection <- ""
    }
    if (!is.na(forest_reserve)) {
      check_input(
        forest_reserve, database, "PlotDetails_1eSet", "ForestReserve",
        "PlotDetails_2eSet"
      )
      selection <- ifelse(selection == "", "WHERE", paste(selection, "AND"))
      selection <-
        paste0(selection, " pd.ForestReserve in ('", forest_reserve, "')")
    }
    assert_that(is.logical(processed))
    if (processed) {
      selection <- ifelse(selection == "", "WHERE", paste(selection, "AND"))
      selection <-
        paste0(selection, " pd.DataProcessed_YN = 10 AND pd.", survey_name,
               " = 10")
    }
    return(selection)
  }

library(assertthat)
library(DBI)
# assert_that

################
# plotinfo <- load_plotinfo(database = path_to_fieldmap_db) 

database <- path_to_fieldmap_db

load_plotinfo <-
  function(database, plottype = NA, forest_reserve = NA, processed = TRUE) {
    
    
    
    selection <-
      translate_input_to_selectquery(
        database = database, plottype = "CP",
        forest_reserve = forestreserve, processed = TRUE,
        survey_name = "DataProcessed_YN"
      )
    
    query_plot <-
      "SELECT pd.ForestReserve AS forest_reserve,
      Plots.ID AS plot_id,
      qPlotType.Value3 AS plottype,
      99 AS period,  --add column name for right order (to be overwritten)
      99 AS survey_number,
      1234 AS year_dendro, --add column name for right order (to be overwritten)
      pd.Date_Dendro_%1$deSet AS date_dendro,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.GameImpactVegObserved AS game_impact_veg,
      pd.GameImpactRegObserved AS game_impact_reg,
      pd.DataProcessed_YN AS data_processed
    FROM (Plots
      INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID %3$s;"
    
    query_plot_1986 <-
      sprintf(
        "SELECT pd.ForestReserve AS forest_reserve,
        Plots.ID AS plot_id,
        qPlotType.Value3 AS plottype,
        99 AS period,  --add column name for right order (to be overwritten)
        99 AS survey_number,
        1234 AS year_dendro,
        pd.Date_Dendro_1986 AS date_dendro,
        pd.Survey_Trees_YN AS survey_trees,
        pd.Survey_Deadwood_YN AS survey_deadw,
        pd.Survey_Vegetation_YN AS survey_veg,
        pd.Survey_Regeneration_YN AS survey_reg,
        pd.GameImpactVegObserved AS game_impact_veg,
        pd.GameImpactRegObserved AS game_impact_reg,
        pd.DataProcessed_YN AS data_processed
      FROM (Plots
        INNER JOIN PlotDetails_1986 pd ON Plots.ID = pd.IDPlots)
        INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID  %1$s;",
        selection
      )
    
    con <- odbcConnectAccess2007(path_to_fieldmap_db)
    plotinfo_1986 <- sqlQuery(con, query_plot_1986) 
    
    plotinfo_1986 <- plotinfo_1986 %>%
      mutate(period = 0)
    odbcClose(con)
    
    plotinfo <-
      query_database(database, query_plot, selection = selection)
    if (nrow(plotinfo_1986) > 0) {
      if (inherits(con, "SQLiteConnection")) {
        plotinfo_1986 <- plotinfo_1986 %>%
          mutate(
            date_dendro = as.POSIXct(.data$date_dendro, origin = "1970-01-01")
          )
      }
      plotinfo <- plotinfo %>%
        bind_rows(
          plotinfo_1986
        )
    }
    plotinfo <- plotinfo %>%
      distinct() %>%
      mutate(
        survey_trees = (.data$survey_trees == 10 & !is.na(.data$survey_trees)),
        survey_deadw = (.data$survey_deadw == 10 & !is.na(.data$survey_deadw)),
        survey_veg = (.data$survey_veg == 10 & !is.na(.data$survey_veg)),
        survey_reg = (.data$survey_reg == 10 & !is.na(.data$survey_reg)),
        game_impact_veg = (.data$game_impact_veg == 10
                           & !is.na(.data$game_impact_veg)),
        game_impact_reg = (.data$game_impact_reg == 10
                           & !is.na(.data$game_impact_reg)),
        
        data_processed =
          (.data$data_processed == 10 & !is.na(.data$data_processed))
      )
    
    plotinfo <- plotinfo %>%
      left_join(plotinfo %>%
                  filter(.data$survey_trees == TRUE) %>%
                  group_by(.data$plot_id, .data$plottype,
                           .data$forest_reserve, .data$survey_trees) %>%
                  summarise(min_period = min(.data$period)) %>%
                  
                  ungroup()) %>%
      mutate(
        survey_number = .data$period - .data$min_period + 1L,
        year_dendro =
          as.integer(year(.data$date_dendro) - (month(.data$date_dendro) < 5))
      ) %>%
      select(-"min_period", -"date_dendro")
    
    attr(plotinfo, "database") <- sub("^.*\\/(.*)\\/.*\\.\\w*$", "\\1", database)
    attr(plotinfo, "forrescalc") <-
      paste("forrescalc", packageVersion("forrescalc"))
    
    return(plotinfo)
  }


