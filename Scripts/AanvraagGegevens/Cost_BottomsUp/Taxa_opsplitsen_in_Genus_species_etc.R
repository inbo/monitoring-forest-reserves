# tip ivm rgbif en parsenames gekregen van Wouter Van Landuyt (5/2020)
# geïmplementeerd in script "CostBottomsUpData_vegetation.Rmd"

library("rgbif")

parsed_trees <- parsenames(scientificname = as.character(qspecies$Value2))


parsed_herbs_BR <- parsenames(scientificname = as.character(qherbspecies$Value2))
qherbspecies <- qherbspecies %>%
  left_join(parsed_herbs_BR, by = c("Value2" = "scientificname")) %>%
  unique()

parsedpartially <- qherbspecies %>%
  filter(parsedpartially ==TRUE)

possibleproblems <- data_herblayer %>%
  filter(species %in% parsedpartially$ID)


#♥ volgende soorten gaven problemen:
      #
      # Viola riviniana/reichenbachiana
      # Dryopteris carthusiana/dilatata
      # Betula tremula/alba
      # Rubus 'fruticosus' groep -> rubus groep


write.csv2(qherbspecies, paste(here::here("Output"), "/CostBottomUp_2020_05/", "qHerbSpecies_GenusSpecies", ".csv", sep = ""))


