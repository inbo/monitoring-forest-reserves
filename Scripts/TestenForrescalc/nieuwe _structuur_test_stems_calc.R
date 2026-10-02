


## inladen uit db --------------------
stems_old <- read_csv2(paste0(path_to_treelevel_csv, "stems_calc.csv"))
trees_old <- read_csv2(paste0(path_to_treelevel_csv, "trees_calc.csv"))


## berekenen met forrescalc --------------------

# tree-niveau ----
data_dendro <- load_data_dendrometry(database = path_to_fieldmap_db, 
                                     extra_variables = TRUE, 
                                     processed = TRUE) %>% 
  filter(plottype != "OTHER")  # enkel CP of CA

trees_old <- read_csv2(paste0(path_to_treelevel_csv, "trees_calc.csv"))

extra_trees <- trees_old |> 
  anti_join(data_dendro, by = c("plot_id", "tree_measure_id"))
nrow(extra_trees)
# de dode delen van coppice dat verwijderd is
extra_trees |> filter(is.na(coppice_id)) |> nrow() == 0
extra_trees |> filter(alive_dead != 12) |> nrow() == 0



# data_dendro nu bij alive_dead ook NVT shoots
table(data_dendro$alive_dead)
# 11    12    15 
# 65315  8978 19990 
table(trees_old$alive_dead)
# 11    12 
# 83316 13346 



# shoots-----
data_shoots <- load_data_shoots_AL(database = path_to_fieldmap_db, extra_variables = TRUE)
# hier nu in forrescalc alive_dead meenemen

data_shoots_old_fction <- load_data_shoots(database = path_to_fieldmap_db, extra_variables = TRUE)
nrow(data_shoots) == nrow(data_shoots_old_fction)
ncol(data_shoots); ncol(data_shoots_old_fction)
# 1 extra kolom levend dood


# stems ---------------
# trees en shoots combineren
data_stems  <- compose_stem_data_AL(data_dendro, data_shoots, extra_variables = TRUE)
data_stems_old_fction  <- compose_stem_data(data_dendro, data_shoots, extra_variables = TRUE)
table(data_stems_old_fction$alive_dead)
# de 15  mmoet vervangen worden door alive_dead_snags
table(data_stems$alive_dead)
data_stems |> filter(is.na(alive_dead)) |> nrow() == 0
nrow(data_stems) == nrow(data_stems_old_fction)

# 
load("height_model.RData")

#
stems_calc <- calc_variables_stem_level(data_stems, height_model)
table(stems_calc$alive_dead)
table(stems_calc$alive_dead_shoots)
stems_calc |> filter(is.na(alive_dead)) |> nrow() == 0

colnames(stems_calc)
unique(stems_calc$forest_reserve)

stems_calc %>% filter(is.na(calc_height_m)) %>% nrow() == 0


# vgl met oude db -----
# waldo::compare(stems_old, data_stems)
nrow(stems_calc) ==  nrow(stems_old)
# 12 records meer in de oude versie (oude db én oude fctie)
diff_stems_old_db <- stems_old |> 
  anti_join(stems_calc, by = c("plot_id", "tree_measure_id", "period"))
# !!! shoot-measure_id is aangepast
range(stems_old$shoot_measure_id, na.rm = T)
range(stems_calc$shoot_measure_id, na.rm = T)
range(data_stems_old_fction$shoot_measure_id, na.rm = T)

mean(stems_old$vol_bole_m3, na.rm = T)
mean(stems_calc$vol_bole_m3, na.rm = T)
mean(data_stems_old_fction$vol_bole_m3, na.rm = T)


# data_dendro_calc ------------
names(data_dendro)
names(stems_calc)

data_dendro_calc <- calc_variables_tree_level_AL(data_dendro, stems_calc)

data_dendro_calc_old_fction <- calc_variables_tree_level(data_dendro, stems_calc)

table(data_dendro_calc$alive_dead)
table(data_dendro_calc_old_fction$alive_dead)


nrow(data_dendro_calc) 
nrow(data_dendro_calc_old_fction)  # oude fctie op nieuwe structuur data : levend en dood samen in één
nrow(trees_old)  # oude db én oude fctie => meer trees, want apart levende en dode stoven

mean(data_dendro_calc$vol_bole_m3, na.rm = T)
mean(data_dendro_calc_old_fction$vol_bole_m3, na.rm = T)
mean(trees_old$vol_bole_m3, na.rm = T)
