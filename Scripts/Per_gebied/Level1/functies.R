

# functies forrescalc aangepast aan Level 1

# load_data_dendrometry ----------

# voor één jaar
# period, year, rA4 zelf aanvullen (2009, 2025)
# date: meerdere data vermeld in layer "Observer_date" - veld "Date_dendro" => ? first

# get_surveys <- function(con, year) {
#   res <- dbSendQuery(
#     con,
#     glue::glue("SELECT * FROM surveys WHERE year = {year}")
#   )
#   surveys <- dbFetch(res)
#   dbClearResult(res)
#   return(surveys)
# }
# # Use the R function
# get_surveys(con, 1980)

load_data_dendrometry_L1 <-
  function(con, jaar) {
    
    if (!jaar %in% c(2009, 2025)) {
      stop("Geen geldig jaartal, kies 2009 of 2025")
    }

    if (jaar == 2025) {
      query_dendro <-
        "SELECT 
        Plots.ID AS plot_id,
        qPlotTp.Value1 AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
          AS plotarea_ha,
        Trees.ID AS tree_measure_id,
        Trees.OldID AS old_id,
        Trees.Species AS species,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.CalcHeight_m AS calc_height_fm,
        Trees.Status_tree AS alive_dead,
        Trees.IntactTree AS intact_snag,
        Trees.CodeCoppice_Individual AS ind_sht_cop,
        Trees.Decay_Stage AS decaystage,
        Trees.TreeNumber AS nr_of_stems
      FROM ((Plots INNER JOIN Trees2025 AS Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN qPlotTp ON Plots.Plot_type = qPlotTp.ID)
       ;"
      
    } else {
      query_dendro <-
        "SELECT 
        Plots.ID AS plot_id,
        qPlotTp.Value1 AS plottype,
        IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
          AS plotarea_ha,
        Trees.ID AS tree_measure_id,
        Trees.OldID AS old_id,
        Trees.Species AS species,
        Trees.DBH_mm AS dbh_mm,
        Trees.Height_m AS height_m,
        Trees.CalcHeight_m AS calc_height_fm,
        Trees.Status_tree AS alive_dead,
        Trees.IntactTree AS intact_snag,
        Trees.CodeCoppice_Individual AS ind_sht_cop,
        Trees.TreeNumber AS nr_of_stems
      FROM ((Plots INNER JOIN Trees2009 AS Trees ON Plots.ID = Trees.IDPlots)
        INNER JOIN qPlotTp ON Plots.Plot_type = qPlotTp.ID)
       ;"
    } 
    
    # -- geen info over decaystage
    # -- Trees.Decay_Stage AS decaystage,  
    
    data_dendro <- sqlQuery(con, query_dendro)

    data_dendro <- data_dendro |> 
      mutate(year = jaar
             , r_A4 = 18
             , period = ifelse(year == 2009, 1, 2)
             , dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm)
      )
    
    return(data_dendro)
    
  }
    

    


# load_data_shoots ------------------

load_data_shoots_L1 <-
  function(con, jaar) {
    
    if (!jaar %in% c(2009, 2025)) {
      stop("Geen geldig jaartal, kies 2009 of 2025")
    }
    
    if (jaar == 2025) {
      query_shoots <-
        "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees2025 AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_MM AS dbh_mm,
      Shoots.Height_m AS height_m
    FROM Shoots2025 Shoots;"
      
    } else {
      query_shoots <-
        "SELECT Shoots.IDPlots AS plot_id,
      Shoots.IDTrees2009 AS tree_measure_id,
      Shoots.ID AS shoot_measure_id,
      Shoots.DBH_MM AS dbh_mm,
      Shoots.Height_m AS height_m
    FROM Shoots2009 Shoots;"
    } 
    
    data_shoots <- sqlQuery(con, query_shoots)
    
    data_shoots <- data_shoots |> 
      mutate(period = ifelse(jaar == 2009, 1, 2))
    
    return(data_shoots)
    
  }


# compose_stem_data ------------

compose_stem_data_L1 <-
  function(data_dendro, data_shoots) {
    #omit data that could be misinterpreted if data on shoot level are added
    data_dendro_relevant <- data_dendro %>%
      select(-"nr_of_stems")
    stem_data <- data_dendro_relevant %>%
      filter(.data$ind_sht_cop != 20) %>%
      bind_rows(
        data_dendro_relevant %>%
          select(-"dbh_mm", -"height_m",
                 -"intact_snag", -"decaystage") %>%
          filter(.data$ind_sht_cop == 20) %>%
          inner_join(data_shoots, by = c("plot_id", "tree_measure_id"
                                         , "period", "forest_reserve"))
      ) %>%
      mutate(
        dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm),
        basal_area_m2 = pi * (.data$dbh_mm / 2000) ^ 2
      ) %>%
      relocate("shoot_measure_id", .after = "old_id") 
      
      # %>%
      # relocate(all_of(c("dbh_class_5cm", "basal_area_m2")), .after = "decaystage")
  
    return(stem_data)
  }

# load_data_deadwood ----

load_data_deadwood_L1 <-
  function(con, jaar) {
    
    if (!jaar %in% c(2009, 2025)) {
      stop("Geen geldig jaartal, kies 2009 of 2025")
    }
    
    if (jaar == 2025) {
      query_deadwood <-
        "SELECT Plots.ID AS plot_id,
      qPlotTp.Value1 AS plottype,
     IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
        AS plotarea_ha,
      Deadwood.ID AS lying_deadw_id,
      Deadwood.Species AS species,
      Deadwood.Decay_Stage AS decaystage,
      Deadwood.Volume_m3 AS calc_volume_m3,
      Deadwood.Length_m AS calc_length_m,
      Deadw_Diam.total_length_m,
      Deadw_Diam.min_diam_mm,
      Deadw_Diam.max_diam_mm
    FROM (((Plots
      INNER JOIN Deadwood_2025 AS Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN qPlotTp ON Plots.Plot_type = qPlotTp.ID)
      LEFT JOIN
        (SELECT IDDeadwood_2025, IDPlots,
          MAX(Distance_m) AS total_length_m,
          MIN(Diameter_mm) AS min_diam_mm,
          MAX(Diameter_mm) AS max_diam_mm
        FROM Deadwood_2025_Diameters
        GROUP BY IDDeadwood_2025, IDPlots) AS Deadw_Diam
        ON Deadwood.ID = Deadw_Diam.IDDeadwood_2025)
      WHERE Plots.ID = Deadw_Diam.IDPlots;"
      
      
    } else {
      query_deadwood <-
        "SELECT Plots.ID AS plot_id,
      qPlotTp.Value1 AS plottype,
     IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha)
        AS plotarea_ha,
      Deadwood.ID AS lying_deadw_id,
      Deadwood.Species AS species,
      Deadwood.Decay_Stage AS decaystage,
      Deadwood.Volume_m3 AS calc_volume_m3,
      Deadwood.Length_m AS calc_length_m,
      Deadw_Diam.total_length_m,
      Deadw_Diam.min_diam_mm,
      Deadw_Diam.max_diam_mm
    FROM (((Plots
      INNER JOIN Deadwood AS Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN qPlotTp ON Plots.Plot_type = qPlotTp.ID)
      LEFT JOIN
        (SELECT IDDeadwood, IDPlots,
          MAX(Distance_m) AS total_length_m,
          MIN(Diameter_mm) AS min_diam_mm,
          MAX(Diameter_mm) AS max_diam_mm
        FROM Deadwood_Diameters
        GROUP BY IDDeadwood, IDPlots) AS Deadw_Diam
        ON Deadwood.ID = Deadw_Diam.IDDeadwood)
      WHERE Plots.ID = Deadw_Diam.IDPlots;"
    } 
    
    data_deadwood <- sqlQuery(con, query_deadwood)
    
    data_deadwood <- data_deadwood |> 
      mutate(period = ifelse(jaar == 2009, 1, 2)
             , year = jaar
             , dbh_class_5cm = give_diamclass_5cm(.data$max_diam_mm))
    
    return(data_deadwood)
    
  }

#  load_height_models -----
# niet nodig: gewoon met calc_height werken


# calc_stem_volume --------------
calc_stem_volume <- function(data_stems) {
  
  data_stems <- data_stems %>%
    # (1) calculate bole volume - tariff 1 entry
    left_join(tarieven_1ingang %>%
        select(
          -"name_nl", -"tariff_id", -"tariff_group"),
      by = "species"
    ) %>%
    mutate(
      perimeter = pi * .data$dbh_mm / 10,
      vol_bole_t1_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3,
      # vol_bole_t1/t2_m3 set to a minimum of 0.001
      # as every bole (minimum dbh is 5 cm) has a  minimal volume
      vol_bole_t1_m3 = pmax(0.001, .data$vol_bole_t1_m3)
    ) %>%
    select(
      -"a", -"b", -"c", -"d"
    ) %>%
    # (2) calculate crown volume - tariff 1 entry
    left_join(tarieven_1ingang_kroon %>%
        select(
          -"name_nl", -"tariff_id", -"tariff_group"),
      by = "species"
    ) %>%
    mutate(
      vol_crown_m3 =
        .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
        .data$d * .data$perimeter ^ 3,
      vol_crown_m3 = pmax(0, .data$vol_crown_m3)
    ) %>%
    select(
      -"a", -"b", -"c", -"d"
    ) %>%
    # (3) calculate bole volume - tariff 2 entries
    left_join(coef_omzet_omtrek,
      by = "species"
    ) %>%
    mutate(
      perimeter_150 = (.data$perimeter - .data$a) / .data$b
    ) %>%
    select(
      -"a", -"b"
    ) %>%
    left_join(tarieven_2ingang %>%
        select(
          -"name_nl", -"tariff_id", -"tariff_group"),
      by = "species"
    ) %>%
    mutate(
      perimeter =
        ifelse(.data$formula == 3, .data$perimeter_150, .data$perimeter),
      d_cm = .data$dbh_mm / 10,
      vol_bole_t2_m3 =
        ifelse(
          .data$formula %in% c(1, 3),
          yes =
            .data$a + .data$b * .data$perimeter +
            .data$c * .data$perimeter ^ 2 +
            .data$d * .data$perimeter ^ 3 +
            .data$e * .data$calc_height_m +
            .data$f * .data$calc_height_m * .data$perimeter +
            .data$g * .data$calc_height_m * .data$perimeter ^ 2,
          no =
            1 / 1000 *
            #spil
            (exp(1.10597 * log(.data$calc_height_m) +
                   1.78865 * log(.data$d_cm) - 3.07192) -
               #Verlies
               exp(
                 -4.608923 * log(.data$d_cm) +
                   3.005989 * log(.data$calc_height_m) -
                   1.3209 * log(.data$calc_height_m) * log(.data$calc_height_m) +
                   1.605266 * log(.data$d_cm) * log(.data$calc_height_m) +
                   5.410272
               )
            )
        ),
      vol_bole_t2_m3 = pmax(0.001, .data$vol_bole_t2_m3),
      vol_bole_m3 =
        ifelse(
          .data$ind_sht_cop == 12 & is.na(.data$vol_bole_t2_m3),
          .data$vol_bole_t1_m3,
          .data$vol_bole_t2_m3
        )
    ) %>%
    select(
      -"a", -"b", -"c", -"d", -"e", -"f", -"g",
      -"formula", -"d_cm", -"perimeter", -"perimeter_150",
      -"vol_bole_t1_m3", -"vol_bole_t2_m3"
    ) %>%
    mutate(
      # (4) volume correction for snags
      # crown volume = 0
      vol_crown_m3 = ifelse(.data$intact_snag == 20 & !is.na(.data$intact_snag), 0, .data$vol_crown_m3),
      # bole volume = volume cilinder
      vol_bole_m3 =
        ifelse(
          .data$intact_snag == 20 & !is.na(intact_snag),
          pi * .data$height_m * .data$dbh_mm^2 / 2000^2,
          .data$vol_bole_m3
        )
    )
  
  return(data_stems)
}


# calc_variables_stem_level-----

calc_variables_stem_level_L1 <-
  function(data_stems, height_model) {
    
    # (1) calculate height using height models (calc_height_r)
    data_stems1 <- data_stems %>%
      left_join(
        height_model,
        by = c("species", "forest_reserve", "period", "plot_id")
      )
    
    # data_stems1 |> filter(is.na(P1)) |> nrow()
    # data_stems1 |> filter(is.na(model)) |> nrow()
    
    data_stems2 <- data_stems1 %>%
      filter(!is.na(.data$model)) %>%
      bind_rows(
        data_stems1 %>%
          filter(is.na(.data$model)) %>%
          select(-"model", -"P1", -"P2") %>%
          left_join(
            height_model %>%
              filter(is.na(.data$species)) %>%
              select(-"species"),
            by = c("forest_reserve", "period", "plot_id")
          )
      ) %>%
      mutate(
        calc_height_r =
          ifelse(
            grepl("exp", .data$model),
            1.3 + exp(.data$P1 + .data$P2 / (.data$dbh_mm / 10)),
            1.3 + .data$P1 + .data$P2 * log(.data$dbh_mm / 10)
          ),
        dh_model = ifelse(!is.na(.data$P1), TRUE, FALSE),
        # if no height_model is available, calc_height_fm on tree level (< FM-IA)
        # is used
        calc_height_m =
          ifelse(is.na(.data$calc_height_r)
                 , pmax(1.3, .data$calc_height_fm)
                 , pmax(1.3, .data$calc_height_r))
      ) %>%
      select(
        -"model", -"P1", -"P2"
      )

    data_stems2 <- calc_stem_volume(data_stems2) %>%
      mutate(
        # volume correction for broken crown or branches
        reduction_crown =
          ifelse(is.na(.data$crown_volume_reduction), 0,
                 as.numeric(.data$crown_volume_reduction)),
        vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_crown),
        reduction_branch =
          ifelse(is.na(.data$branch_length_reduction), 0,
                 as.numeric(.data$branch_length_reduction)),
        vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_branch),
        # total volume
        vol_tot_m3 = .data$vol_bole_m3 + .data$vol_crown_m3
      ) %>%
      # (3) results per hectare
      mutate(stem_number_alive_ha =
               ifelse(
                 .data$alive_dead == 11,
                 1 / .data$plotarea_ha,
                 0
               ),
             stem_number_dead_ha =
               ifelse(
                 .data$alive_dead == 12,
                 1 / .data$plotarea_ha,
                 0
               ),
             basal_area_alive_m2_ha =
               ifelse(
                 .data$alive_dead == 11,
                 .data$basal_area_m2 / .data$plotarea_ha,
                 0
               ),
             basal_area_dead_m2_ha =
               ifelse(
                 .data$alive_dead == 12,
                 .data$basal_area_m2 / .data$plotarea_ha,
                 0
               ),
             vol_alive_m3_ha =
               ifelse(
                 .data$alive_dead == 11,
                 .data$vol_tot_m3 / .data$plotarea_ha,
                 0
               ),
             vol_dead_standing_m3_ha =
               ifelse(
                 .data$alive_dead == 12,
                 .data$vol_tot_m3 / .data$plotarea_ha,
                 0
               ),
             vol_bole_alive_m3_ha =
               ifelse(
                 .data$alive_dead == 11,
                 .data$vol_bole_m3 / .data$plotarea_ha,
                 0
               ),
             vol_bole_dead_m3_ha =
               ifelse(
                 .data$alive_dead == 12,
                 .data$vol_bole_m3 / .data$plotarea_ha,
                 0
               )
      ) %>%
      select(
        -"calc_height_fm", -"calc_height_r", -"dh_model",
        -"reduction_crown", -"reduction_branch")
    
    return(data_stems2)
  }


# calc_variables_tree_level ----------

calc_variables_tree_level_L1 <-
  function(data_dendro, data_stems_calc) {

    data_dendro1 <- data_dendro %>%
      select(
        -"dbh_mm", -"nr_of_stems", -"calc_height_fm",
        -"intact_snag", -"decaystage"
      ) %>%
      left_join(
        data_stems_calc %>%
          group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
          summarise(
            nr_of_stems = n(),
            decaystage =
              as.integer(
                round(
                  sum(.data$decaystage * .data$dbh_mm ^ 2 / 4) /
                    sum(.data$dbh_mm ^ 2 / 4)
                )
              ),
            intact_snag = max(.data$intact_snag),
            calc_height_m = sum(.data$calc_height_m * .data$dbh_mm ^ 2 / 4) /
              sum(.data$dbh_mm ^ 2 / 4),
            dbh_mm = round(sqrt(sum(.data$dbh_mm ^ 2) / n())),
            basal_area_m2 = sum(.data$basal_area_m2),
            vol_bole_m3 = sum(.data$vol_bole_m3),
            vol_crown_m3 = sum(.data$vol_crown_m3),
            vol_tot_m3 = sum(.data$vol_tot_m3),
            # RESULTS PER HECTARE
            basal_area_alive_m2_ha = sum(.data$basal_area_alive_m2_ha),
            basal_area_dead_m2_ha = sum(.data$basal_area_dead_m2_ha),
            vol_alive_m3_ha = sum(.data$vol_alive_m3_ha),
            vol_dead_standing_m3_ha = sum(.data$vol_dead_standing_m3_ha),
            vol_bole_alive_m3_ha = sum(.data$vol_bole_alive_m3_ha),
            vol_bole_dead_m3_ha = sum(.data$vol_bole_dead_m3_ha)
          ) %>%
          ungroup(),
        by = c("plot_id", "tree_measure_id", "period")
      ) %>%
      mutate(
        individual = (.data$ind_sht_cop == 10 | .data$ind_sht_cop == 12)
      ) %>%
      mutate(number_of_trees_alive_ha =
               ifelse(
                 .data$alive_dead == 11,
                 .data$individual / .data$plotarea_ha,
                 0
               ),
             number_of_trees_dead_ha =
               ifelse(
                 .data$alive_dead == 12,
                 .data$individual / .data$plotarea_ha,
                 0
               )
      ) %>%
      select(-"individual")

    return(data_dendro1)
  }

# calculate_dendrometry --------------

calculate_dendrometry_L1 <- function(data_dendro, data_deadwood, data_shoots,
                                  height_model, plotinfo) {
  data_stems <- compose_stem_data_L1(data_dendro, data_shoots)
  
  
  data_stems_calc <- calc_variables_stem_level_L1(data_stems, height_model)
  data_dendro_calc <- calc_variables_tree_level_L1(data_dendro, data_stems_calc)
  # data_deadwood <- calc_intact_deadwood(data_deadwood)
  by_plot <- calc_dendro_plot(data_dendro_calc, data_deadwood, plotinfo)
  by_plot_species <-
    calc_dendro_plot_species(data_dendro_calc, data_deadwood, plotinfo)
  by_decay_plot <-
    calc_deadw_decay_plot(plotinfo, data_deadwood, data_dendro_calc)
  by_decay_plot_species <-
    calc_deadw_decay_plot_species(plotinfo, data_deadwood, data_dendro_calc)
  by_diam_plot <- calc_diam_plot(data_stems_calc, data_deadwood, plotinfo)
  by_diam_plot_species <-
    calc_diam_plot_species(data_stems_calc, data_deadwood, plotinfo)
  
  return(
    list(
      dendro_by_plot = by_plot,
      dendro_by_plot_species = by_plot_species,
      dendro_by_diam_plot = by_diam_plot,
      dendro_by_diam_plot_species = by_diam_plot_species,
      deadw_by_decay_plot = by_decay_plot,
      deadw_by_decay_plot_species = by_decay_plot_species
    )
  )
}

# give_diamclass_5cm ------

give_diamclass_5cm <- function(diameterdata) {
  diameterclass <-
    factor(
      ifelse(
        diameterdata >= 2500,
        49,
        floor(diameterdata / 50)
      ),
      levels = 1:49,
      labels =
        c(paste(seq(5, 240, 5), "-", seq(10, 245, 5), "cm"), "245 cm +")
    )
  return(diameterclass)
}

