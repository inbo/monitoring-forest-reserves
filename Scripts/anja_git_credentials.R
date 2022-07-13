
source(here::here("scripts/Setup_Forrescalc.R"))

qSpecies <- read_forresdat("qSpecies", repo_path = path_to_git_forresdat, join_plotinfo = FALSE)

## 

cred <- git2r::cred_ssh_key(
  publickey = "C:/Users/anja_leyman/.ssh/id_rsa.pub",
  privatekey = "C:/Users/anja_leyman/.ssh/id_rsa"
)

# use_github(credentials = cred)


list.files(
  Sys.getenv("USERPROFILE"),
  pattern = "ssh|git",
  include.dirs = TRUE,
  all.files = TRUE
)

# [1] ".git-credentials" ".gitconfig"       ".ssh"    



# ssh staat waar het zou moeten staan, onder userprofile, maar toch navigeert R naar onedrive
# oplossing: ssh-folder gekopieerd naar onedrive
# C:\Users\anja_leyman\OneDrive - Vlaamse overheid - Office 365
