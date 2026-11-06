# test load_data_shoots: extra var alive_dead_shoots
connect_to_database <-
  function(database) {
    
    if (grepl(".accdb$", database) || grepl(".mdb$", database)) {
      con <-
        DBI::dbConnect(
          odbc::odbc(),
          .connection_string =
            paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                   database)
        )
    } else if (grepl(".sqlite$", database)) {
      con <- DBI::dbConnect(SQLite(), database)
    } else if (grepl(".fdb$", database) || grepl(".gdb", database)) {
      con <-
        DBI::dbConnect(
          odbc::odbc(),
          .connection_string =
            paste0(
              "Driver={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", #nolint: line_length_linter
              database
            )
        )
    } else {
      stop(
        "This database type is not supported, please use .mdb, .accdb, .fdb, .gdb or .sqlite" #nolint: line_length_linter
      )
    }
    
    return(con)
  }


#" 
library("assertthat")


# 
compare_attributes <- function(x, y, name_x, name_y) {
  if (!is.null(attr(x, "database")) && !is.null(attr(y, "database"))) {
    if (attr(x, "database") == attr(y, "database")) {
      attr_database <- attr(x, "database")
    } else {
      stop(
        sprintf(
          "%1$s and %2$s are not from the same version of the database, please only provide datasets from the same database", #nolint: line_length_linter
          name_x, name_y
        )
      )
    }
  } else {
    attr_database <- NULL
  }
  if (!is.null(attr(x, "forrescalc")) && !is.null(attr(y, "forrescalc"))) {
    if (attr(x, "forrescalc") == attr(y, "forrescalc")) {
      attr_forrescalc <- attr(x, "forrescalc")
    } else {
      stop(
        sprintf(
          "%1$s and %2$s are not calculated with the same version of forrescalc, please use the same version of the package for all calculations", #nolint: line_length_linter
          name_x, name_y
        )
      )
    }
    if (attr_forrescalc != paste("forrescalc", packageVersion("forrescalc"))) {
      stop(
        "The given datasets are not calculated with the version of forrescalc that is used now, please use the same version of the package for all calculations" #nolint: line_length_linter
      )
    }
  } else {
    attr_forrescalc <- NULL
  }
  
  return(
    list(
      attr_database = attr_database, attr_forrescalc = attr_forrescalc
    )
  )
}


#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter inner_join mutate relocate select
#' @importFrom assertthat has_name
#'
compose_stem_data_AL <-
  function(data_dendro, data_shoots, extra_variables = FALSE) {
    extra_vars <- c("iufro_hght", "iufro_vital", "iufro_socia",
                    "remark", "common_remark")
    extra_vars_shoots <- c("iufro_hght_shoots", "iufro_vital_shoots",
                           "iufro_socia_shoots",
                           "remark_shoots", "common_remark_shoots")
    if (extra_variables) {
      assert_that(
        has_name(data_dendro, extra_vars),
        msg =
          "data_dendro should contain extra variables as iufroclasses and (common_)remark" #nolint: line_length_linter
      )
      assert_that(
        has_name(data_shoots, extra_vars_shoots),
        msg =
          "data_shoots should contain extra variables as iufroclasses and (common_)remark" #nolint: line_length_linter
      )
    } else {
      if (has_name(data_dendro, extra_vars)) {
        data_dendro <- data_dendro %>% select(-all_of(extra_vars))
      }
      if (has_name(data_shoots, extra_vars_shoots)) {
        data_shoots <- data_shoots %>% select(-all_of(extra_vars_shoots))
      }
    }
    attributes <-
      compare_attributes(
        data_dendro, data_shoots, "data_dendro", "data_shoots"
      )
    #omit data that could be misinterpreted if data on shoot level are added
    data_dendro_relevant <- data_dendro %>%
      select(
        -"nr_of_stems", -"dbh_class_5cm"
      )
    stem_data <- data_dendro_relevant %>%
      filter(.data$ind_sht_cop != 12) %>%
      bind_rows(
        data_dendro_relevant %>%
          select(-"dbh_mm", -"height_m",
                 -"intact_snag", -"decaystage") %>%
          filter(.data$ind_sht_cop == 12) %>%
          inner_join(data_shoots, by = c("plot_id", "tree_measure_id", "period"))
      ) %>%
      mutate(
        dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm),
        basal_area_m2 = pi * (.data$dbh_mm / 2000) ^ 2
      ) %>%
      relocate("shoot_measure_id", .after = "old_id") %>%
      relocate(all_of(c("dbh_class_5cm", "basal_area_m2")), .after = "decaystage")
    
    if (
      has_name(
        stem_data,
        c("iufro_hght", "iufro_vital", "iufro_socia", "iufro_hght_shoots",
          "iufro_vital_shoots", "iufro_socia_shoots",
          "remark_shoots", "common_remark_shoots")
      )
    ) {
      stem_data <- stem_data %>%
        mutate(
          alive_dead = 
            ifelse(is.na(.data$alive_dead_shoots),
                   .data$alive_dead, .data$alive_dead_shoots),
          iufro_hght =
            ifelse(is.na(.data$iufro_hght_shoots),
                   .data$iufro_hght, .data$iufro_hght_shoots),
          iufro_hght_shoots = NULL,
          iufro_vital =
            ifelse(is.na(.data$iufro_vital_shoots),
                   .data$iufro_vital, .data$iufro_vital_shoots),
          iufro_vital_shoots = NULL,
          iufro_socia =
            ifelse(is.na(.data$iufro_socia_shoots),
                   .data$iufro_socia, .data$iufro_socia_shoots),
          iufro_socia_shoots = NULL,
          remark =
            ifelse(is.na(.data$remark_shoots),
                   .data$remark, .data$remark_shoots),
          remark_shoots = NULL,
          common_remark =
            ifelse(is.na(.data$common_remark_shoots),
                   .data$common_remark, .data$common_remark_shoots),
          common_remark_shoots = NULL,
        )
    }
    
    attr(stem_data, "database") <- attributes[["attr_database"]]
    attr(stem_data, "forrescalc") <- attributes[["attr_forrescalc"]]
    
    return(stem_data)
  }
