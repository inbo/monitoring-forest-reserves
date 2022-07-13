

t <- data_trees %>%
  filter(.data$IndShtCop == 12) %>%
  anti_join(
    data_shoots,
    by = c("IDPlots", "X_m" = "XTrees", "Y_m" = "YTrees",
           "tree_measure_id" = "IDTrees", "period")
  )




t2 <- data_shoots %>%
  anti_join(
    data_trees %>%
      filter(.data$IndShtCop == 12),
    by = c("IDPlots", "XTrees" = "X_m", "YTrees" = "Y_m",
           "IDTrees" = "tree_measure_id", "period")
  ) %>%
  select(
    .data$IDPlots, X_m = .data$XTrees, Y_m = .data$YTrees,
    tree_measure_id = .data$IDTrees, .data$period
  ) %>%
  mutate(
    coppiceproblem = "tree has shoots in table shoots"
  )

# hoe best meerdere foutmeldingen weergeven?
"tree not in A3 / no coppice tree linked with shoots"
"tree not in A3, no coppice tree linked with shoots"
"tree not in A3/no coppice tree linked with shoots"




