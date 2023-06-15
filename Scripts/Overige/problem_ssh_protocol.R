
# git2r cred_ssh_key remote_url

library(git2r)
install.packages("remote_url")
library(remote_url)
install.packages("cred_ssh_key")
library(cred_ssh_key)

repo_path <- path_to_forresdat_data
tablename <- "regeneration_by_plot"
regeneration_by_plot <- forrescalc::read_forresdat("regeneration_by_plot", repo_path)

repo <- repository(repo_path)
pull(repo, credentials = get_cred(repo))
dataset <- read_vc(file = paste0("data/", tablename), root = repo)


libgit2_features()


# To fix this, I reinstalled libssh2 and libgit2. In my case, I had libssh2, but I was still missing libgit2. On a Mac, I did
# 
# brew install libssh2 and brew install libgit2.
# 
# After that, I re-installed git2r from source with
# 
install.packages("git2r", type="source", configure.vars="autobrew=yes")
# 
# Then, after reloading git2r with library(git2r) when I libgit2_features() I got ssh TRUE and could clone the repo


install.packages("libssh2")
install.packages("libgit2")

library(git2r)

libgit2_features()

