---
title: "Controle vegetatiedata"
author: "Anja Leyman"
date: "14 mrt 2022"
output: 
  html_document:
    code_folding: hide
    fig_caption: yes
    number_sections: yes
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
editor_options: 
  chunk_output_type: inline
---

```{r Rm, eval = FALSE}
rm(list=ls())
```

```{r Setup, include = FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  eval = FALSE,
  message = FALSE,
  warning = FALSE, 
  fig.width = 9,
  fig.align = TRUE)

#rm(list = ls())

library(here)

# libraries & invoergegevens
source(here::here("scripts/Setup.R"))

# library(tidyr)
# library (rlang)
# library(dplyr)
# library(RODBC)
# library(lubridate)
# library(knitr)
# library(here)
# library(openssl)
# library(kableExtra)
# library(stringr)


```


# Vraagstelling

Doel van dit script is datacontrole vegetatie (vegetation & herblayer).


```{r LoadLookuplists}
con <- odbcConnectAccess2007(path_to_fieldmap_db)
  
  qLayer <- sqlFetch(con, "qLayer", stringsAsFactors = FALSE)
  qTotalCover <- sqlFetch(con, "qtotalCover", stringsAsFactors = FALSE)
  qHerbSpecies <- sqlFetch(con, "qHerbSpecies240810", stringsAsFactors = FALSE)
  qBrowseIndex <- sqlFetch(con, "qBrowsIndex", stringsAsFactors = FALSE)
  qCoverHerbs <- sqlFetch(con, "qCoverHerbs", stringsAsFactors = FALSE)

odbcClose(con)
```


# Data inladen

Eerst dienen de vegetatiegegevens ingeladen te worden.

- data_vegetation met gegevens op plotniveau (algemene bedekking, year, ... zonder soorten) 
- data_herblayer met soortspecifieke gegevens

Enkele verduidelijkingen:

<!-- Veg.Year AS year_main_survey -->
<!-- year = year(.data$date_vegetation), -->
<!--       year = ifelse(is.na(.data$year), .data$year_main_survey, .data$year), -->

* year: afgeleid van het veld `Datum`, of van `Opnamejaar` als `Datum` niet ingevuld is

* year_main_survey: overgenomen van veld `Opnamejaar` - zou zelfde moeten zijn als `year`

* date_vegetation: wordt op niveau van herblayer bij voorjaarsflora vervangen door de afwijkende datum

* totalplotarea_ha: area of total plot; in case of circular plot: area of A4 circle

* plotarea_ha: area of plot (ha): area of vegetation plot (16 x 16 m2 = 0.0256 ha)

* naast range van cover, wordt ook min, max en mid toegevoegd 

```{r LoadDatavegetation}
# TER CONTROLE - OOK NIET PROCESSED 

data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap_db, 
    processed=FALSE
  )
# 5012

data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap_db, 
    processed=FALSE
  )
# 67771
```


Als total_herb_cover > 1%, dan zou er ook een species genoteerd moeten worden in herblayer.

```{r veg_maar_no_herbs}
# plots mét vegetatie in herblayer
cover_veg <- data_vegetation %>% 
  filter(!is.na(total_herb_cover_id) & !total_herb_cover_id %in% c(1, 20)) %>% 
  group_by(plot_id, subplot_id, period) %>% 
  summarize() %>% 
  ungroup()

plots_with_herbs <- data_herblayer %>% 
  group_by(plot_id, subplot_id, period) %>% 
  summarize() %>% 
  ungroup()

nrow(plots_with_herbs)
nrow(cover_veg)

# plots waar géén herblayer 
no_herbs <-  cover_veg %>% 
  anti_join(plots_with_herbs)
```



