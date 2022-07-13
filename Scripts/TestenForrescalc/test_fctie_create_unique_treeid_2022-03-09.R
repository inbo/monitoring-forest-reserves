### test-data -----------

test_data <- check_1986_tree_id
# test_data <- check_Muiz_tree_id  # 2 decades => OK
# test_data <- check_Kerss_CP_tree_id # 3 decades => niet OK voor derde decade 

### functie uitgesplitst ----------
status_tree <- test_data %>%
    mutate(
      tree_id =
        ifelse(
          is.na(.data$old_id),
          paste(.data$period, .data$plot_id, .data$tree_measure_id, sep = "_"),
          NA
        )
    )

# dataset <- status_tree %>% filter(is.na(tree_id))
dataset <- status_tree

# lookup_tree_id <- function(dataset) {
    # if (any(is.na(dataset$tree_id))) {
      dataset_ <- dataset %>%
        left_join(
          dataset %>%
            transmute(
              .data$plot_id, .data$tree_measure_id, .data$tree_id, .data$old_id,
              period = .data$period + 1
            ) %>%
            filter(!is.na(.data$tree_id)) %>%
            distinct(),
          by = c("plot_id", "old_id" = "tree_measure_id", "period"),
          suffix = c("", "_oldid")  # _oldid bij deze die al een tree_id hadden
        ) %>%
        mutate(
          tree_id =
            ifelse(
              is.na(.data$tree_id),
              .data$tree_id_oldid,
              .data$tree_id
            )
        ) %>%
        select(-.data$tree_id_oldid, -.data$old_id_oldid)
      # dataset <- lookup_tree_id(dataset)
    # }

  
t <- dataset %>%
  # transmute does not retain our original data
  transmute(
    .data$plot_id, .data$tree_measure_id, .data$tree_id, .data$old_id,
    period = .data$period + 1  # want we willen die koppelen aan de volgende periode
                                # by = c("plot_id", "old_id" = "tree_measure_id", "period")
  ) %>%
  filter(!is.na(.data$tree_id)) %>%
  distinct()
  
# je moet dat inderdaad 2 x na elkaar doen!!
  
### 2de keer -------
dataset__ <- dataset_ %>%
  left_join(
    dataset_ %>%
      transmute(
        .data$plot_id, .data$tree_measure_id, .data$tree_id, .data$old_id,
        period = .data$period + 1
      ) %>%
      filter(!is.na(.data$tree_id)) %>%
      distinct(),
    by = c("plot_id", "old_id" = "tree_measure_id", "period"),
    suffix = c("", "_oldid")  # _oldid bij deze die al een tree_id hadden
  ) %>%
  mutate(
    tree_id =
      ifelse(
        is.na(.data$tree_id),
        .data$tree_id_oldid,
        .data$tree_id
      )
  ) %>%
  select(-.data$tree_id_oldid, -.data$old_id_oldid)


### 3de keer -----
dataset_finaal <- dataset__ %>%
  left_join(
    dataset__ %>%
      transmute(
        .data$plot_id, .data$tree_measure_id, .data$tree_id, .data$old_id,
        period = .data$period + 1
      ) %>%
      filter(!is.na(.data$tree_id)) %>%
      distinct(),
    by = c("plot_id", "old_id" = "tree_measure_id", "period"),
    suffix = c("", "_oldid")  # _oldid bij deze die al een tree_id hadden
  ) %>%
  mutate(
    tree_id =
      ifelse(
        is.na(.data$tree_id),
        .data$tree_id_oldid,
        .data$tree_id
      )
  ) %>% 
  select(-.data$tree_id_oldid, -.data$old_id_oldid)


### controle obv XY - enkel Kerss KV
check_XY <- dataset_finaal %>% 
  mutate(x = round(x_local,1),
         y = round(y_local,1)) %>% 
  group_by(plot_id, x, y, tree_id) %>% 
  summarize(aantal_periodes = n(),
            aantal_species = n_distinct(species),
            max_period = max(period)) %>% 
  ungroup() %>% 
  mutate(period_obv_id = 4 - as.integer(str_sub(tree_id, 1, 1)))

check_XY2 <- check_XY %>% 
  group_by(plot_id, tree_id, aantal_species) %>% 
  summarize(aantal_xy = n(),
            range_x = max(x)-min(x),
            range_y = max(y)-min(y)) %>% 
  ungroup() %>% 
  filter(aantal_xy > 1 | aantal_species > 1) %>% #maar één soort telkens!
  filter(range_x > 1 | range_y > 1)

check_XY3 <- dataset_finaal %>% 
  inner_join(check_XY2)

# alles lijkt OK, 2 bomen waar verschuiving van  3 à 5 m op zit, maar lijkt OK!!!



# OUDE CONTROLE

#check koppeling: toch enkel periode 3 aan 2 gekoppeld?
status_tree_2 %>% filter(period == 1 & !is.na(tree_id_oldid) ) %>% nrow()
# enkel Kersselaerspleyn waarbij iets mis is, omdaty daar wel een oldid ingevuld staat bij periode 1
# & plot_id != 11000

t <- status_tree_2 %>% filter(period == 2 & !is.na(tree_id_oldid))
# normaliter hebben deze allemaal een tree_id en wordt die dus NIET overschreven
status_tree_2 %>% filter(period == 2 & !is.na(tree_id_oldid) & is.na(tree_id)) %>% nrow()
# [1] 0

status_tree %>% filter(period == 1 & tree_measure_id != old_id) %>% nrow()

status_tree_3 <- status_tree_2 %>% 
  mutate(
    tree_id =
      ifelse(
        is.na(.data$tree_id) & .data$period == .data$period_oldid + 1,  #= alle periode 3, mét oldID (die zonder werden 3_101_4545)
        .data$tree_id_oldid,   # die nemen dan tree_id van periode 2 over, want gekoppeld obv old_id uit periode 3 en tree_id uit periode 2
        .data$tree_id
      )
  ) 


klopt alvast voor periode 3 en periode 2

hoe krijgt een periode 1 een unieke tree_id? old_id = leeg dus 1_425_45289
hoe krijgt periode 2 een unieke tree_id? 
    als old_id leeg, dan 2_425_5478
    als old_id NIET leeg, dan 1_425_oldid







%>%
  select(-.data$period_oldid, -.data$tree_id_oldid)



tmp <- status_tree %>%
  select(
    .data$plot_id, .data$tree_measure_id, .data$period, .data$tree_id
  ) %>%
  filter(.data$period == 2)
