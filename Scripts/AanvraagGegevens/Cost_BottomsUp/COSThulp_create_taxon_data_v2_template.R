#05.05.2020
rm(list=ls())
library("readxl")
library("tidyverse")

#cols required for taxon raw dataframe
struc<-c("siteID","plotID","elemID","genus","species","genspe","taxon","layer","abucov","abuind")

#create empty output taxon raw dataframe 
output<-as.data.frame(matrix(NA,ncol=length(struc),nrow=0))
names(output)<-struc

#key columns IDs should be character type (important for append data:)
str(output)
output$siteID=as.character(output$siteID); output$plotID=as.character(output$plotID); output$elemID<-as.character(output$elemID)
str(output)

#set your inout data directory
setwd("/Users/francescochianucci/Dropbox/PersonaLI/Missioni_estero/COST/COST_CA18207_BOTTOMS-UP/database/AC_data/data_AC/originali/db_coppice")


#Tracheophyta data
#load original raw taxon data 
"temp_Tbl_B4_Vegetation.xls" %>% 
  read_excel() -> trach


#create siteID codes COST from  original site id codes from an existing column "ID_Sito"
trach<-trach %>% 
  mutate(siteID=as.character(
    case_when(ID_Sito == "Buca Zamponi"~"IT_AC1_CAT",
             ID_Sito == "Poggio Pievano"~"IT_AC2_MET",
             ID_Sito == "Is Cannoneris"~"IT_AC3_ISC")))

#check if the rename is ok
table(trach$siteID,trach$ID_Sito) # check ok

#rename some existing columns like genus, species, elemID, plotID based on existing original cols
trach<-trach %>% 
  dplyr::rename(genus=Genere,species=Specie) %>% 
  mutate(elemID=as.character(ID_osservazione), plotID=as.character(ID_Area))

# Create genspe by concatenating genus and species cols if available from original data
trach<-trach %>% 
  mutate(genspe=str_c(genus,species,sep=" "))


#create layer col (only for tracheophytes) from an existing original column
trach<-trach %>% 
  mutate(layer=
           case_when(Layer == "erbaceo"~"Herb",
                     Layer == "arbustivo"~"Shrub",
                     Layer == "arboreo"~"Tree"))

table(trach$Layer,trach$layer) #check ok


#In this case I have abundance  as original Braun-Blanquet categorical scale (i.e. :r,+,1:5)
# Braun-Blanquet scale recode
BBor<-c("r","\"+\"",1:5)
BBcl<-c(0.01,0.1,3,15,37.5,62.5,87.5)
bb<-data.frame(cbind(BBor,BBcl))

trach<-trach %>% 
  left_join(bb,by=c("Cover"="BBor"))

#In this case I need to put them in abucov column
trach<-trach %>% mutate(abucov=as.numeric(as.character(BBcl)))
# table(trach$Abuvalue,trach$Cover) #check ok

#Create Taxon col
trach$taxon<-"Tracheophyta"

#select only tracheop relevant for COST db
out.trach<-trach %>% select(one_of(struc))

#append to the output dataframe:
output<-bind_rows(output,out.trach)  

str(output)


#More or less the same for other taxon


##Wood_dec_mushrooms data

"temp_Tbl_B4_Wood_Dacaying_Mushrooms.xls" %>% 
  read_excel() -> fung

#create siteID
fung<-fung %>% 
  mutate(siteID=as.character(
           case_when(ID_Sito == "Buca Zamponi"~"IT_AC1_CAT",
                     ID_Sito == "Poggio Pievano"~"IT_AC2_MET",
                     ID_Sito == "Is Cannoneris"~"IT_AC3_ISC")))

table(fung$siteID,fung$ID_Sito) # check ok

fung<-fung %>% 
  dplyr::rename(genus=Genere_Fungo,species=Specie_Fungo) %>% 
  mutate(elemID=as.character(ID_osservazione), plotID=as.character(ID_Area)) %>% 
  # mutate(species=str_c(tolower(str_sub(Genus,1,3)),str_sub(Species,1,3),sep="."))
  mutate(genspe=str_c(genus,species,sep=" "))

fung$abuind<-NA #it has only P/A data
fung$taxon<-"Fungi"

#drop non-needed cols in the output file
out.fung<-fung %>% select(one_of(struc))

output<-bind_rows(output,out.fung)  


##Birds

"temp_Tbl_B4_Birds.xls" %>% 
  read_excel()-> bird

#create siteID by renaming original Site id from existing column
bird<-bird %>% 
  mutate(siteID=as.character(
           case_when(ID_Sito == "Buca Zamponi"~"IT_AC1_CAT",
                     ID_Sito == "Poggio Pievano"~"IT_AC2_MET",
                     ID_Sito == "Is Cannoneris"~"IT_AC3_ISC")))
#check
table(bird$siteID,bird$ID_Sito) # check ok


#rename cols and create abbr. species
bird<-bird %>% 
  dplyr::rename(genus=Genere,species=Specie,abuind=Contatti) %>% 
  mutate(elemID=as.character(ID_osservazione), plotID=as.character(ID_Area)) %>% 
  # mutate(species=str_c(tolower(str_sub(Genus,1,3)),str_sub(Species,1,3),sep="."))
  mutate(genspe=str_c(genus,species,sep=" ")) 

bird$taxon<-"Aves"


out.bird<-bird %>% select(one_of(struc))

output<-bind_rows(output,out.bird)  


write.csv(output,"raw_data_taxa_IT_AC_v2.csv",row.names=F)