# DOEL: datacontrole trees mbv functie uit forrescalc ----

# !! functie kijkt enkel naar locatie en koppeling shoots-trees


##############################################


library(forrescalc)
# https://github.com/inbo/forresdat


# Algemene documentatie
vignette("overview_forrescalc", package = "forrescalc")


### setup ----
path_to_fieldmap <- "C:/3BR/5_MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb"
path_to_git_forresdat <- "C:/3BR/2_VisualisatieDataBR/2Packages/forresdat"

path_to_results <- "C:/3BR/2_VisualisatieDataBR/1Rproject_BR/Output/BR_resultaten.accdb"


library(tidyr)
library (rlang)
library(dplyr)
library(RODBC)
library(lubridate)


### datacontrole trees (mbv functie) ------
incorrect_trees <- check_data_trees(path_to_fieldmap)

### functie uitgelicht -----
database <- path_to_fieldmap


query_trees <-
  "SELECT Trees.IDPlots,
Plots.Plottype,
Trees.X_m, Trees.Y_m,
Trees.ID AS tree_measure_id,
Trees.DBH_mm,
Trees.Height_m,
Trees.Species,
Trees.IntactSnag,
Trees.AliveDead,
Trees.IndShtCop,
Trees.CoppiceID,
Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
Trees.DecayStage,
Trees.Remark,
Trees.TreeNumber,
Trees.Adjust_Vol_tot_m3,
Trees.AdjustBasalArea_m2
FROM Plots INNER JOIN Trees ON Plots.ID = Trees.IDPlots;"

query_shoots <-
  "SELECT IDPlots, XTrees, YTrees, IDTrees, ID AS shoot_id, DBH_mm, Height_m, IntactSnag
FROM Shoots"

query_trees2 <-
  "SELECT Trees.IDPlots,
Plots.Plottype,
Trees.X_m, Trees.Y_m,
Trees.ID AS tree_measure_id,
Trees.DBH_mm,
Trees.Height_m,
Trees.Species,
Trees.IntactSnag,
Trees.AliveDead,
Trees.IndShtCop,
Trees.CoppiceID,
Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
Trees.DecayStage,
Trees.Remark,
Trees.TreeNumber,
Trees.Adjust_Vol_tot_m3,
Trees.AdjustBasalArea_m2,
Trees.OldID
FROM Plots INNER JOIN Trees_2eSET Trees ON Plots.ID = Trees.IDPlots;"

query_shoots2 <-
  "SELECT IDPlots,
XTrees_2eSET AS XTrees, YTrees_2eSET AS YTrees, IDTrees_2eSET AS IDTrees, ID AS shoot_id,
DBH_mm, Height_m, IntactSnag
FROM Shoots_2eSET"

con <- odbcConnectAccess2007(database)
data_trees <- sqlQuery(con, query_trees, stringsAsFactors = FALSE) %>%
  mutate(
    period = 1
  ) %>%
  bind_rows(
    sqlQuery(con, query_trees2, stringsAsFactors = FALSE) %>%
      mutate(
        period = 2
      )
  )
data_shoots <- sqlQuery(con, query_shoots, stringsAsFactors = FALSE) %>%
  mutate(
    period = 1
  ) %>%
  bind_rows(
    sqlQuery(con, query_shoots2, stringsAsFactors = FALSE) %>%
      mutate(
        period = 2
      )
  )
odbcClose(con)

incorrect_trees <- data_trees %>%
  # niet in A3 of A4
  mutate(
    problem =
      ifelse(
        .data$Plottype == 20 & sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 18,
        "tree not in A4",
        NA
      ),
    problem =
      ifelse(
        .data$Plottype == 20 & .data$AliveDead == 11 & .data$DBH_mm < 400 &
          sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 9,
        "tree not in A3",
        .data$problem
      ),
    problem =
      ifelse(
        .data$Plottype == 20 & .data$AliveDead == 12 & .data$DBH_mm < 100 &
          sqrt(.data$X_m ^ 2 + .data$Y_m ^ 2) > 9,
        "tree not in A3",
        .data$problem
      )
  ) %>%
   7

%
%%



  %
# shoots niet correct gelinkt met trees
  left_join(
    data_trees %>%
      filter(.data$IndShtCop == 12) %>%
      # an anti join returns the rows of the first table where it cannot find a match in the second table
      # OK, getest
      anti_join(
        data_shoots,
        by = c("IDPlots", "X_m" = "XTrees", "Y_m" = "YTrees",
               "tree_measure_id" = "IDTrees", "period")
      ) %>%
      select(
        .data$IDPlots, .data$X_m, .data$Y_m, .data$tree_measure_id,
        .data$period
      ) %>%
      mutate(
        coppiceproblem = "no shoots linked with coppice tree"
      ),
    by = c("IDPlots", "X_m", "Y_m", "tree_measure_id", "period")
  ) %>%
  mutate(
    problem =
      ifelse(
        !is.na(.data$problem),
        ifelse(
          !is.na(.data$coppiceproblem),
          paste(.data$problem, .data$coppiceproblem, sep = ", "),
          .data$problem
        ),
        .data$coppiceproblem
      ),
    coppiceproblem = NULL
  ) %>%
  full_join(
    data_shoots %>%
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
        coppiceproblem = "no coppice tree linked with shoots"
      ),
    by = c("IDPlots", "X_m", "Y_m", "tree_measure_id", "period")
  ) %>%
  mutate(
    problem =
      ifelse(
        !is.na(.data$problem),
        ifelse(
          !is.na(.data$coppiceproblem),
          paste(.data$problem, .data$coppiceproblem, sep = " / "),
          # dus opsomming van alle "problems" gescheiden door komma
          .data$problem
        ),
        .data$coppiceproblem
      ),
    coppiceproblem = NULL
  ) %>%
  filter(!is.na(problem))


table(incorrect_trees$problem)

# hoe best meerdere foutmeldingen weergeven?
"tree not in A3 / no coppice tree linked with shoots"
"tree not in A3, no coppice tree linked with shoots"
"tree not in A3/no coppice tree linked with shoots"


