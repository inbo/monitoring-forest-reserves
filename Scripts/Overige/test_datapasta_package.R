
# PASTE ---------

install.packages("datapasta")
library('datapasta')

df_paste()

t <- data.frame(
  stringsAsFactors = FALSE,
  name_nl = c("Aalbes","Adelaarsvaren",
              "Amerikaanse eik","Amerikaanse vogelkers","Beuk",
              "Bleke zegge","Bosandoorn","Bosanemoon",
              "Bosereprijs"),
  variable = c("perc_plots_species_herblayer",
               "perc_plots_species_herblayer",
               "perc_plots_species_herblayer",
               "perc_plots_species_herblayer","perc_plots_species_herblayer",
               "perc_plots_species_herblayer",
               "perc_plots_species_herblayer",
               "perc_plots_species_herblayer","perc_plots_species_herblayer"),
  n_obs = c(50L,50L,50L,50L,50L,50L,50L,50L,
            50L),
  mean = c(6L, 2L, 12L, 2L, 38L, 6L, 12L, 100L, 16L)
)

