


### inbo-databanken -----
# Log in to INBO VPN

# If not done, install the required packages by following the instructions:
# inbodb installation instructions
install.packages("remotes")
remotes::install_github("inbo/inbodb", build_vignettes = TRUE)

# watina installation instructions
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # as a precaution
remotes::install_github("inbo/watina", upgrade = TRUE)


# Load the required packages:
library(tidyverse)
library(inbodb)
library(watina)

# Open connections to databases
watina <- connect_watina()
inboveg <- connect_inbo_dbase("D0010_00_Cydonia")
florabank <- connect_inbo_dbase("D0021_00_userFlora")