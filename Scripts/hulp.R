calc_variables_tree_level <-
  function(data_dendro, data_stems, height_models) {
    
    height_model <- height_models
   
    data_stems1 <- data_stems %>%
      left_join(
        height_model,
        by = c("species", "forest_reserve", "period", "plottype")
      )
    
    data_stems2 <- data_stems1 %>%
      filter(!is.na(.data$model)) %>%
      bind_rows(
        data_stems1 %>%
          filter(is.na(.data$model)) %>%
          select(-.data$model, -.data$P1, -.data$P2) %>%
          left_join(
            height_model %>%
              filter(is.na(.data$species)) %>%
              select(-.data$species),
            by = c("forest_reserve", "period", "plottype")
          )
      ) %>%
      mutate(
        calc_height_r =
          ifelse(
            grepl("exp", .data$model),
            1.3 + exp(.data$P1 + .data$P2 / (.data$dbh_mm / 10)),
            1.3 + .data$P1 + .data$P2 * log(.data$dbh_mm / 10)
          )
        , calc_height_m =
          ifelse(is.na(.data$calc_height_r), .data$calc_height_m, .data$calc_height_r)
      ) %>% 
      select(-.data$model, -.data$P1, -.data$P2)
    
    tarief1 <- read_csv2("C:/3BR/2_VisualisatieDataBR/1Packages/forrescalc/inst/extdata/tariffs1entry.csv") %>%
      select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source)
    tarief2 <- read_csv2("C:/3BR/2_VisualisatieDataBR/1Packages/forrescalc/inst/extdata/tariffs1entry_crown.csv") %>% select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source)
    
    tarief3 <- read_csv2("C:/3BR/2_VisualisatieDataBR/1Packages/forrescalc/inst/extdata/tariffs2entries.csv") %>% select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source)
    
    
    
    data_stems3 <- data_stems2 %>% 
      # bole volume 1 entry
      left_join(tarief1, by = "species") %>% 
      mutate(perimeter = pi * .data$dbh_mm / 10
        , vol_stem_t1_m3 =
         .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
         .data$d * .data$perimeter ^ 3
        , vol_stem_t1_m3 = pmax(0, .data$vol_stem_t1_m3)
      ) %>%
      select(
        -.data$a, -.data$b, -.data$c, -.data$d
      ) %>%
      # crown volume 1 entry on shoot level
      left_join(tarief2,by = "species"
      ) %>%
      mutate(
        vol_crown_m3 =
          .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
          .data$d * .data$perimeter ^ 3,
        vol_crown_m3 = pmax(0, .data$vol_crown_m3)
      ) %>%
      select(
        -.data$a, -.data$b, -.data$c, -.data$d
      ) %>% 
      # bole volume 2 entries on shoot level
      # !! (when DH-model or calc_height_m is available)
      left_join(tarief3, by = "species"
                ) %>%
      mutate(
        d_cm = .data$dbh_mm / 10
        , vol_stem_t2_m3 =
          ifelse(
            .data$formula == 1,
            yes =
              .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
              .data$d * .data$perimeter ^ 3 + .data$e * .data$calc_height_m +
              .data$f * .data$calc_height_m * .data$perimeter +
              .data$g * .data$calc_height_m * .data$perimeter ^ 2,
            no =
              1 / 1000 *
              #spil
              (exp(1.10597 * log(.data$calc_height_m) + 1.78865 * log(.data$d_cm) - 3.07192) -
                 #Verlies
                 exp(-4.608923 * log(.data$d_cm) + 3.005989 * log(.data$calc_height_m) -
                       1.3209 * log(.data$calc_height_m) * log(.data$calc_height_m) +
                       1.605266 * log(.data$d_cm) * log(.data$calc_height_m) + 5.410272))
          )
        , vol_stem_t2_m3 = pmax(0, .data$vol_stem_t2_m3)
        , vol_stem_m3 =
          ifelse(
            .data$ind_sht_cop == 12 & is.na(.data$vol_stem_t2_m3),
            .data$vol_stem_t1_m3,
            .data$vol_stem_t2_m3
          )
        ) %>%
      select(
        -.data$a, -.data$b, -.data$c, -.data$d, -.data$e, -.data$f, -.data$g,
        -.data$formula, -.data$d_cm, -.data$perimeter
      )
    
    
    data_dendro1 <- data_dendro %>%
      # select(
      #   -.data$dbh_mm, -.data$vol_tot_m3, -.data$vol_stem_m3, -.data$vol_crown_m3
      # ) %>%
      select(
        -contains("_ha")
      ) %>%
      left_join(
        data_stems3 %>%
          group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
          summarise(
            tree_number = n()
            , decaystage =
              round(
                sum(.data$decaystage * .data$dbh_mm ^ 2 / 4) /
                  sum(.data$dbh_mm ^ 2 / 4)
              )
            , dbh_mm = sqrt(sum(.data$dbh_mm ^ 2) / n())
            , basal_area_m2 = sum(.data$basal_area_m2)
            , vol_stem_t1_m3 = sum(.data$vol_stem_t1_m3)
            , vol_stem_t2_m3 = sum(.data$vol_stem_t2_m3)
            , vol_stem_m3 = sum(.data$vol_stem_m3)
            , vol_crown_m3 = sum(.data$vol_crown_m3)
          ) %>%
          ungroup(),
        by = c("plot_id", "tree_measure_id", "period")
      ) %>%
      mutate(
        individual = (.data$ind_sht_cop == 10 | .data$ind_sht_cop == 12) 
        # volume correction for snags
        , vol_crown_m3.x = ifelse(.data$intact_snag == 10, 0, .data$vol_crown_m3.x)
        , vol_stem_m3.x = ifelse(.data$intact_snag == 10, .data$calc_height_m*pi*(.data$dbh_mm.y/2000)^2, .data$vol_stem_m3.x)
        # TIJDELIJK vol_stem_m3 berekend als cilinder cfr. VBI (soms over- en soms onderschatting)
        # OP TERMIJN functie van Ifer (in afzonderlijke functie te stoppen)
        # volume correction for broken crown or branches
        , reduction_crown =
          ifelse(is.na(.data$crown_volume_reduction), 0, .data$crown_volume_reduction)
        , vol_crown_m3.x = .data$vol_crown_m3.x * (1 - .data$reduction_crown)
        , reduction_branch =
          ifelse(is.na(.data$branch_length_reduction), 0, .data$branch_length_reduction)
        , vol_crown_m3.x = .data$vol_crown_m3.x * (1 - .data$reduction_branch)
        # total volume
        , vol_tot_m3.x = .data$vol_stem_m3.x + .data$vol_crown_m3.x
        , dbh_mm = round(dbh_mm.y)  
        # ) %>%
      # mutate(
        # volume correction for snags
        , vol_crown_m3.y = ifelse(.data$intact_snag == 10, 0, .data$vol_crown_m3.y)
       , vol_stem_m3.y = ifelse(.data$intact_snag == 10, .data$calc_height_m*pi*(.data$dbh_mm.y/2000)^2, .data$vol_stem_m3.y)
      , vol_crown_m3.Z = .data$vol_crown_m3.y * (1 - .data$reduction_crown)
      , vol_crown_m3.y = .data$vol_crown_m3.y * (1 - .data$reduction_crown)
        # , reduction_branch =
        #   ifelse(is.na(.data$branch_length_reduction), 0, .data$branch_length_reduction)
        , vol_crown_m3.y = .data$vol_crown_m3.y * (1 - .data$reduction_branch)
        # total volume
        , vol_tot_m3.y = .data$vol_stem_m3.y + .data$vol_crown_m3.y
      ) 

    
    
    
    
    ### test --------
    data_stems1 %>% filter(is.na(.data$model)) %>% nrow()
    data_stems2 %>% filter(is.na(.data$calc_height_r)) %>% nrow()

    data_stems2 %>% filter(is.na(.data$calc_height_m)) %>% nrow()
    
    t1 <- data_stems1 %>% 
      select(plot_id, tree_measure_id, plottype, period, forest_reserve, species, alive_dead, ind_sht_cop, dbh_mm, contains("height"), contains("tree_number"))
    
    t2_stems <- data_stems3 %>% 
      select(plot_id, tree_measure_id, plottype, period, forest_reserve, species, alive_dead, ind_sht_cop, dbh_mm, contains("height"), contains("tree_number"), contains("vol_")) 

    
    t3_trees <- data_dendro1 %>% 
      select(plot_id, tree_measure_id, plottype, period, forest_reserve, species, alive_dead, ind_sht_cop, intact_snag, contains("dbh_mm"), contains("height"), contains("tree_number"), contains("basal_"), contains("vol_"), contains("redu")) 

    
    ## CONTROLE VERSCHILLEN x y 
    
    # treenr
    t3_trees %>%  filter(tree_number.x != tree_number.y) %>% nrow()
    
    # dbh
    t3_trees %>%  filter(abs(dbh_mm.x - dbh_mm.y) > 1) %>% nrow()   # 164
    
    t <- t3_trees %>%  filter(abs(dbh_mm.x - dbh_mm) >= 1) # OK - 1409
    t <- t3_trees %>%  filter(abs(dbh_mm.x - dbh_mm) > 1) # OK - 1
    
    # basal area
    t3_trees %>%  filter(basal_area_m2.x*tree_number.x != basal_area_m2.y) %>% nrow()
    t <- t3_trees %>%  
      mutate(BA = tree_number.y * pi * (dbh_mm.y/2)^2 / 1000000,
             test_BA = basal_area_m2.x*tree_number.x - basal_area_m2.y,
             test_perc = abs(test_BA/basal_area_m2.y),
             test_perc2 = test_BA/basal_area_m2.y) %>% 
      filter(abs(test_perc) > 0.02) %>% 
      select(-contains("vol_"))
    
    # ?? waarom verschil op BA: BA : blijkbaar dbh's veranderd sinds dochterdb' - ofwel bij shoots ofwel bij hermeting
    
    # volume
    t <- t3_trees %>%  
      mutate(test_volstem = vol_stem_m3.x*tree_number.x - vol_stem_m3.y,
             test_perc = 100 * abs(test_volstem/vol_stem_m3.y),
             test_perc2 = 100 * test_volstem/vol_stem_m3.y) %>% 
      filter(abs(test_perc) > 100 & vol_stem_m3.y != 0 & vol_crown_m3.y != 0 & vol_stem_m3.x != 0) %>% 
      select(-contains("basal_"))
    # ok voor stem 
    
    t <- t3_trees %>%  
      mutate(test_volcrown = vol_crown_m3.x*tree_number.x - vol_crown_m3.y,
             test_perc = 100 * abs(test_volcrown/vol_crown_m3.y),
             test_perc2 = 100 * test_volcrown/vol_crown_m3.y) %>% 
      filter(abs(test_perc) > 50 & vol_crown_m3.y != 0 & vol_crown_m3.y != 0 & vol_crown_m3.x != 0) %>% 
      select(-contains("basal_"))
    
    t %>% filter(plot_id == 888, tree_measure_id == 23)
    
    t2 <- t3_trees %>%  
      mutate(test_volcrown = vol_crown_m3.x*tree_number.x - vol_crown_m3.Z,
             test_perc = 100 * abs(test_volcrown/vol_crown_m3.Z),
             test_perc2 = 100 * test_volcrown/vol_crown_m3.Z) %>% 
      filter(abs(test_perc) > 50 & vol_crown_m3.Z != 0 & vol_crown_m3.Z != 0 & vol_crown_m3.Z != 0) %>% 
      select(-contains("basal_"))
    
    t2 %>% filter(plot_id == 888, tree_measure_id == 23)
    
    chech <- data_dendro %>%  select(plot_id, tree_measure_id, plottype, period, forest_reserve, species, alive_dead, ind_sht_cop, intact_snag, contains("dbh_mm"), contains("height"), contains("tree_number"), contains("basal_"), contains("vol_")) 
    