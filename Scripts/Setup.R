

library(forrescalc)
# https://github.com/inbo/forresdat


# Algemene documentatie
# vignette("overview_forrescalc", package = "forrescalc")


### Paths ----
source(here::here("Scripts/Paths.r"))

### Libraries ----
# library(ddply)
library(tidyr)
library (rlang)
# library(plyr)  # indien nodig dan voor dplyr plaatsen (cfr warning)
library(dplyr)
library(RODBC)
library(lubridate)
library(openssl)
library(kableExtra)
library(stringr)
library(here)
library(tibble)
library(tidyselect)
library(readxl)
library(git2rdata)
library(readr)
library(tidyverse)
library(inbodb)
library(glue)
library(forestmangr)
library(gridExtra)
library(sf)
library(xlsx)
library(googledrive)
library(googlesheets4)

library(usethis)
library(forrescalc)

# library(conflicted)  #ivm problemen unused argument bij "here", lukt ook door here::here te gebruiken

    # install.packages("writexl")
    # install.packages("usethis")
    # install.packages("ddply")

# bovenstaande libraries zitten allemaal vervat in package "forrescalc"
# !! toch best inladen, want soms nodig voor eigen code (buiten de functies van forrescalc)
library(forrescalc)



### Extra functies tov package forrescalc ----
source(here::here("Scripts/functions.r"))
source(here::here("Scripts/functions_volume.r"))
source(here::here("Scripts/functions_overall_statistics.r"))

### Load packages (na herinstallatie pc) -----
# install.packages("glue")
# install.packages("forestmangr")
# install.packages("googledrive")
# install.packages("googlesheets4")
