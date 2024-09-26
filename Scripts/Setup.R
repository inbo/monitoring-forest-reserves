## INSTALL PACKAGES ----------------

# install.packages("forrescalc") 
        # samen met INBO universe: update packages dan wordt deze ook geupdate
        # enkel als nieuwe versie uitkomt ...

# remotes::install_github("inbo/forrescalc@bugfix")   # ok  26/9/2024 (in aparte R)

    # tijdelijk om probleem met save to access op te lossen 
    # mail ELs 25/9/2024:
        # Ik heb dit probleempje opgelost met een commit in een nieuwe branch 'bugfix', 
        # dus je kan deze versie installeren met
        # 
        # remotes::install_github("inbo/forrescalc@bugfix")
        # 
        # Nadeel is wel dat dit geen officiële versie met versienummer is, dus voor het wegschrijven van 
        # gegevens in forresdat gebruik je beter de versie die je installeert met install.packages() omdat je er 
        # dan een proper versienummer bij hebt  (je kan gewoon telkens opnieuw met die commando's installeren wat 
        # je nodig hebt voor je code, misschien minder handig maar ik wil niet voor elk detail een nieuwe versie maken)
        # 
        # Ik zou zeggen: test alles goed uit (zeker met Access) en lees alles goed na, dan laat ik deze branch
        # eventjes open voor eventuele extra problemen, en als alles over enkele dagen of weken in orde lijkt,
        # zal ik deze als nieuwe versie publiceren.  (En ik zal meteen ook dat foutje in de documentatie
        # verbeteren, maar ga wachten met pushen tot vanavond of de volgende commit, kwestie om de
        # continuous integration op github niet te overladen.)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!! Bij export to forersdat terug andere installeren, met versienr !!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


## LIBRARIES ----------------

library(forrescalc)
# https://github.com/inbo/forresdat


# Algemene documentatie
# vignette("overview_forrescalc", package = "forrescalc")


### Paths ----
source(here::here("Scripts/Paths.r"))

### Libraries ----
# library(ddply)
library(tidyr)
# library (rlang)
# library(plyr)  # indien nodig dan voor dplyr plaatsen (cfr warning)
library(dplyr)
library(RODBC)
# library(lubridate)
# library(openssl)
library(kableExtra)
library(stringr)
library(here)
library(tibble)
# library(tidyselect)
library(readxl)
library(git2rdata)
library(readr)
library(tidyverse)
# library(inbodb)
library(glue)
# library(forestmangr)
library(gridExtra)
# library(sf)
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
