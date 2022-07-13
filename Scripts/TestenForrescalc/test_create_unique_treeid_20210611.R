

status_tree <- data_dendro %>%
  # oldid's periode 1 verwijderen
  mutate(old_id = ifelse(period == 1, NA, old_id)
  ) %>% 
  mutate(
    tree_id =
      ifelse(
        is.na(.data$old_id),
        paste(.data$period, .data$plot_id, .data$tree_measure_id, sep = "_"),
        ifelse(
          .data$period == 2,
          paste(1, .data$plot_id, .data$old_id, sep = "_"),
          NA
        )
      )
  )


status_tree_2 <- status_tree %>%
  left_join(
    status_tree %>%
      select(
        .data$plot_id, .data$tree_measure_id, .data$period, .data$tree_id
      ) %>%
      filter(.data$period == 2),
    by = c("plot_id", "old_id" = "tree_measure_id"),    
    # dus deze van periode 2 (tree_measure_id) worden aan periode 3 (old_id) gekoppeld, want periode 1 normaal gesproken geen oldID
    suffix = c("", "_oldid")
  ) %>%
  select(contains("id"), period, -contains("width"), plottype, forest_reserve) 

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
