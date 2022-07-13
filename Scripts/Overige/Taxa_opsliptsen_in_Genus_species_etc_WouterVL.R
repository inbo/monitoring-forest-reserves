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
