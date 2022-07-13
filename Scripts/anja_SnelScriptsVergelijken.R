
# install.packages("diffr")
library(diffr)


# folder en scripts definiëren Anja ----
subfolder <- "C:/1Bosinventarisatie_v5/Scripts/Analysedatabank/"
# file1 <- paste(subfolder, "Analysedatabank_meetproces_berekening_LG_hersteld.R", sep = "")
# file2 <- paste(subfolder, "Analysedatabank_meetproces_berekening.R", sep = "")
file2 <- paste(subfolder, "CopyOfAnalyseDatabank_meetproces_inlezen_Anja.Rmd", sep = "")
file1 <- paste(subfolder, "AnalyseDatabank_meetproces_inlezen.Rmd", sep = "")


file2 <- paste(subfolder, "CopyOfAnalyseDatabank_RepeatedMeasurementPlots_Anja.Rmd", sep = "")
file1 <- paste(subfolder, "AnalyseDatabank_RepeatedMeasurementPlots.Rmd", sep = "")



# subfolder <- "scripts/AnalyseMeetvragen/6DuurzaamBosbeheer/6_1b1c_Aanwas/"
# file1 <- paste(subfolder, "6_1b_AnalyseAanwasPerBosgroepBeheerregio.R", sep = "")
# file2 <- paste(subfolder, "6_1bc_AnalyseAanwas.R", sep = "")


# folder en scripts definiëren BR ----
subfolder <- "C:/3BR/2_VisualisatieDataBR/1Packages/"
file1 <- paste(subfolder, "calc_variables_tree_level_AL.R", sep = "")
file2 <- paste(subfolder, "forrescalc/R/calc_variables_tree_level.R", sep = "")

# 

# vergelijken ----
diffr(file1, file2, contextSize = 3, minJumpSize = 10, wordWrap = TRUE,
      before = file1, after = file2, width = NULL, height = NULL)



