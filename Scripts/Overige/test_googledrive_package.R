### GDRIVE--------------

path_to_output_gdrive <- "G:/Gedeelde drives/Team_Boseco/00_projecten/PRJ_BR_AanvraagGegevens/"

# Tip van Damiano - enkel om te downloaden

# Zie 'https://googledrive.tidyverse.org/index.html#all-other-files"
# zelf gdrivelink nemen (is https) en dan via onderstaande code downloaden
googledrive::drive_download("https://drive.google.com/file/d/1V81pZQuauznDa4OoLiGFsQnxsl3bPa9C/view?usp=drive_link")
# => file van internet wordt lokaal opgeslagen
# lokaal = in hoofddir (01_BR_dataverwerking)


# Tip van Hans Van Calster
library(googledrive)
library(dplyr)
team <- team_drive_find("Team_Boseco")
team %>% 
  drive_reveal("permissions")

# oplijsten mappen
team_mappen <- drive_ls(team)

# of ook submappen - lukt niet
        # team_submappen <- drive_ls(team, recursive = TRUE)

# extra variabelen zoals volledig path toevoegen - lukt niet
      # team_submappen <- drive_reveal(team_submappen, what = "path")
      # team <- drive_reveal(team, what = "path")


# Combineren tot "path_to_output_gdrive"
path_to_output_gdrive <- paste0("G:/.shortcut-targets-by-id/", team$id, "/Team_Boseco/00_projecten/PRJ_BR_AanvraagGegevens/")
path_to_gdrive <- paste0("G:/.shortcut-targets-by-id/", team$id, "/Team_Boseco")

write.xlsx(tarieven1ing, paste0(path_to_output_gdrive, "/test_anja.xlsx"))
list.files(path = path_to_output_gdrive)







