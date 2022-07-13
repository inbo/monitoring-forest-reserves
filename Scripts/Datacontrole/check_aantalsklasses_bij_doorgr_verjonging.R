
# checken of en hoeveel aantalsklasses voorkomen bij doorgr verjonging
source(here::here("scripts/Setup_Forrescalc.R"))


data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )

regeneration <- calculate_regeneration(data_regeneration)



str(data_regeneration)

table(data_regeneration$height_class)
test <- data_regeneration %>% 
  filter(height_class %in% c(3000, 4000, 7000, 8000, 6000))
table(test$forest_reserve, test$plottype)

summary(test)


test2 <- test %>% 
  filter(is.na(reg_number))


table(test2$height_class)
table(test2$forest_reserve, test2$plottype)


test3 <- test2 %>% filter(min_number_of_trees != max_number_of_trees)
# 964
table(test3$forest_reserve, test3$plottype)

test2 %>% filter(min_number_of_trees != max_number_of_trees) %>% 
  filter(max_number_of_trees == 1000) %>% nrow()


# dubbele hoogteklasse? -----
check_dubbele_hoogteklasses <- data_regeneration %>% 
  dplyr::group_by(plot_id, subplot_id, forest_reserve, period, height_class, species) %>% 
  dplyr::summarize(n= n()) %>% 
  dplyr::ungroup() %>% 
  filter(n > 1)

