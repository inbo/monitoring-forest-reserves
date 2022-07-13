

# calc_variables_tree_level <-
#   function(data_dendro, data_stems, height_models) {
    

# CALCULATIONS ON STEM LEVEL------------

# (1) calculate height using height models (calc_height_r)--------------

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
      ),
    DHmodel = ifelse(!is.na(.data$P1), "ja", "nee"),
    # if no height_model is available, calc_height_fm on tree level (< FM-IA) is used
    calc_height_m =
      ifelse(is.na(.data$calc_height_r), .data$calc_height_fm, .data$calc_height_r)
  ) %>%
  select(-.data$model, -.data$P1, -.data$P2)


# (2) calculate volume (bole and crown; 1 entry and 2 entries)-----------

data_stems3 <- data_stems2 %>%
  # bole volume 1 entry
  left_join(
    suppressMessages(
      read_csv2(
        system.file("extdata/tariffs1entry.csv", package = "forrescalc")
      )
    ) %>%
      select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source),
    by = "species"
  ) %>%
  mutate(
    perimeter = pi * .data$dbh_mm / 10
    , vol_stem_t1_m3 =
      .data$a + .data$b * .data$perimeter + .data$c * .data$perimeter ^ 2 +
      .data$d * .data$perimeter ^ 3
    , vol_stem_t1_m3 = pmax(0, .data$vol_stem_t1_m3)
  ) %>%
  select(
    -.data$a, -.data$b, -.data$c, -.data$d
  ) %>%
  # crown volume 1 entry
  left_join(
    suppressMessages(
      read_csv2(
        system.file("extdata/tariffs1entry_crown.csv", package = "forrescalc")
      )
    ) %>%
      select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source),
    by = "species"
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
  # bole volume 2 entries
  # !! (when DH-model or calc_height_fm is available)
  left_join(
    suppressMessages(
      read_csv2(
        system.file("extdata/tariffs2entries.csv", package = "forrescalc")
      )
    ) %>%
      select(-.data$name_nl, -.data$tariff_id, -.data$tariff_group, -.data$source),
    by = "species"
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
  ) %>%
  mutate(
    # volume correction for snags
    vol_crown_m3 = ifelse(.data$intact_snag == 10, 0, .data$vol_crown_m3),
    upper_diam_snag_mm = ifelse(.data$intact_snag == 10,
                                .data$dbh_mm * (.data$calc_height_m - .data$height_m) / .data$calc_height_m,
                                NA),
    volume_snag_m3 = ifelse(.data$intact_snag == 10,
                            pi * .data$height_m * (.data$dbh_mm^2 + .data$dbh_mm * .data$upper_diam_snag_mm + .data$upper_diam_snag_mm^2) / (3 * 2000^2),
                            # 1/3 x π x h x ( R² + R x r + r² ) - truncated cone
                            # TIJDELIJK vol_stem_m3 berekend als afgeknotte kegel
                            # OP TERMIJN ev. functie van Ifer (in afzonderlijke functie te stoppen)
                            NA),
    # !!! ? als calc_height er niet is, dan ev. wel nog als cilinder???
    # nee, want dan ook geen volumes van de andere bomen ...)
    vol_stem_m3 = ifelse(.data$intact_snag == 10,
                         .data$volume_snag_m3,
                         .data$vol_stem_m3)
  )
# %>%
# select(-upper_diam_snag_mm, -volume_snag_m3, -calc_height_fm, -calc_height_r)


# (3) group_by on tree level--------------

data_stems4 <- data_stems3 %>%
  group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
  summarise(
    tree_number = n(),
    decaystage =
      round(
        sum(.data$decaystage * .data$dbh_mm ^ 2 / 4) /
          sum(.data$dbh_mm ^ 2 / 4)
      ),
    calc_height_m = sum(.data$calc_height_m * .data$dbh_mm ^ 2 / 4) /
      sum(.data$dbh_mm ^ 2 / 4),
    calc_height_fm = sum(.data$calc_height_fm * .data$dbh_mm ^ 2 / 4) /
      sum(.data$dbh_mm ^ 2 / 4),
    calc_height_r = sum(.data$calc_height_r * .data$dbh_mm ^ 2 / 4) /
      sum(.data$dbh_mm ^ 2 / 4),
    dbh_mm = round(sqrt(sum(.data$dbh_mm ^ 2) / n())),
    basal_area_m2 = sum(.data$basal_area_m2),
    vol_stem_t1_m3 = sum(.data$vol_stem_t1_m3),
    vol_stem_t2_m3 = sum(.data$vol_stem_t2_m3),
    vol_stem_m3 = sum(.data$vol_stem_m3),
    vol_crown_m3 = sum(.data$vol_crown_m3)
  ) %>%
  ungroup()


# CALCULATIONS ON TREE LEVEL-----------

data_dendro1 <- data_dendro %>%
  select(
    -.data$dbh_mm, -.data$tree_number, -.data$calc_height_fm,
    -.data$intact_snag, -.data$decaystage, -.data$basal_area_m2,
    -.data$vol_tot_m3, -.data$vol_stem_m3, -.data$vol_crown_m3
  ) %>%
  left_join(
    data_stems4,
    by = c("plot_id", "tree_measure_id", "period")
  ) %>%
  mutate(
    individual = (.data$ind_sht_cop == 10 | .data$ind_sht_cop == 12),
    # volume correction for broken crown or branches
    reduction_crown =
      ifelse(is.na(.data$crown_volume_reduction), 0, .data$crown_volume_reduction),
    vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_crown),
    reduction_branch =
      ifelse(is.na(.data$branch_length_reduction), 0, .data$branch_length_reduction),
    vol_crown_m3 = .data$vol_crown_m3 * (1 - .data$reduction_branch),
    # total volume
    vol_tot_m3 = .data$vol_stem_m3 + .data$vol_crown_m3
  )