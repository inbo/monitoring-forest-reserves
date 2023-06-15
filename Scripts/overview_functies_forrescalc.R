# OVERZICHT VAN FUNCTIES UIT FORRESCALC
#############################################################################

# !! functies zelf nooit laten lopen van hieruit,
# want dan worden dependencies (libraries) niet meegeladen)

# path_to_functions <- "C:/03_BR/2_Forrescalc_Forresdat/1_forrescalc/R"
path_to_functions <- str_sub(path_to_forrescalc, 1, -2)
path_to_functions


list_functions  <- list.files(path = path_to_functions)
write.csv2(list_functions, paste(here::here("Output"), "/overzicht_functies_forrescalc.csv", sep = ""))
# list_functions <- list_functions %>%
#   .[.!="voorbeeld_VBI_MyWgtParEstimation_functie.R"] %>%
#   .[.!="voorbeeld_VBI_MyWgtParEstimation_gebruik.R"]

# for (i in 1:length(list_functions)) {
#    source(paste(path_to_functions, "/", list_functions[i], sep = ""))
#    }

# functies die verschil tss periodes berekenen
list_functions_diff <- list_functions[str_detect(list_functions, pattern = "diff")]
list_functions_diff

# functies mbt dendro
list_functions_dendro <- list_functions[str_detect(list_functions, pattern = "dendro|diam|logs")]
list_functions_dendro

# functies mbt regeneration
list_functions_reg <- list_functions[str_detect(list_functions, pattern = "regeneration")]
list_functions_reg


# functies mbt vegetation
list_functions_veg <- list_functions[str_detect(list_functions, pattern = "vegetation")]
list_functions_veg


# functies om statistieken te genereren over volledig bosreservaat
list_functions_statistics <- list_functions[str_detect(list_functions, pattern = "statistics")]
list_functions_statistics

