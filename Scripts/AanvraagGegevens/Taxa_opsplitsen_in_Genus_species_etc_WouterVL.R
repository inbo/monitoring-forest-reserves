library(RODBC)
library("rgbif")

source('C:/Users/wouter_vanlanduyt/Google Drive/Scriptjes_R/connect_to_access.R')

db_file_name <- "Z:/Projects/PRJ_Florabank/UserFlorabank.mdb"
connection <- connect_to_access_rodbc(db_file_name)

sqlCode <-"SELECT tblTaxon.*  from tblTaxon;"
sqlCode <-"SELECT qryActueleNaamMossen.*  from qryActueleNaamMossen;"
tblTaxon <- sqlQuery(channel = connection, sqlCode)

parsed <- parsenames(scientificname = as.character(tblTaxon$NaamWetenschappelijk))
class(parsed)
str(parsed)
summary(parsed)
parsed$canonicalname

library(xlsx)
write.xlsx(parsed, "C:/Users/wouter_vanlanduyt/Google Drive/mossen en lichenen/parsed.xlsx")

# Anja ---------------
parsed_test <- parsenames(scientificname = as.character(qSpecies$Value2))

parsed_herbs_BR <- parsenames(scientificname = as.character(qHerbSpecies$Value2))
qHerbSpecies_2 <- qHerbSpecies %>%
  left_join(parsed_herbs_BR, by = c("Value2" = "scientificname")) %>%
  unique()

write.csv2(qHerbSpecies_2, paste(here::here("Output"), "/CostBottomUp_2020_05/", "qHerbSpecies_GenusSpecies", ".csv", sep = ""))


