#set directory
setwd("C:\\Users\\yannick_dewin\\Documents\\Bosreservaten\\LandClim\\_data_Yannick_Zonien")

library(tidyverse)
library(tidyr)
library(dplyr)
library(sf) #using shapefiles
library(readxl)

OPNAME_TABEL <- read.csv("OPNAME_TABEL_.csv", sep = ";") #overzicht alle opnames
#T4 : Hooghout: proefcirkel A3 + proefcirkel A4 Levende Bomen (per omtrekklasse in homogene bestanden)
#T7 : Hooghout: proefcirkel A3 + proefcirkel A4 (alle bomen met hoogte in heterogene bestanden)

OPNAME_TABEL_T4 <- read.csv("OPNAME_TABEL_T4_.csv", sep = ";") #overzicht T4 opnames
#opnae_tabel4_id is uniek voor elke rij, opname tabel ID uniek per opname (proefvlak), kunnen meerdere omtrekklasse en boomsoorten
#OPNAME_IDENTIFICATIE gebruiken voor link proefvlakken
OPNAME_TABEL_T7 <- read.csv("OPNAME_TABEL_T7_.csv", sep = ";") #overzicht T7 opnames
OPNAME_TABEL_T7 <- OPNAME_TABEL_T7[OPNAME_TABEL_T7$LEVEND.DOOD=='Levend',] #only living trees

T4<- read_excel("qry_T4.xlsx")# data T4
T7<- read_excel("qry_T7.xlsx")# data T7

best_tot <- st_read("best_tot.shp") #shapefile met age (more specific than stand level)
zonien_BHI <- st_read("zonien_BHI_20210201.shp") #shapefile with original stands
zonien_proefvlakken <- st_read("zonien_proefvlakken.shp") #location sampling points

overzicht_BHI<- read_excel("qry_overzicht_BHI.xlsx")
GISLINK<- read_excel("qry_GISlink_proefvlaknr_zonien.xlsx") #link proefvlakken en opnamen


BOSBESTAND <- read.csv("BOSBESTAND_.csv", sep = ";")#bestandskarakteristieken




#Verwerking


# We proberen eerst proefvlak niveau (lukt niet, ga verder naar bestandsniveau) ####
#OPNAME_TABEL_T4 en T7, zonien_proefvlakken

#ik kan dus OPNAME_TABEL_T4 en T7 rechtstreeks linken met proefvlakken met behulp van OPNAME_IDENTIFICATIE
#opname identificatie zou moeten matchen met Opname punt maar zonien_proefvlakken$OPNAMEPUNT we herwerken dit eerst naar OPNAME_IDENTIFICATIE

zonien_proefvlakken$OPNAME_IDENTIFICATIE <- 
  toupper(
    ifelse(
      grepl("^z", zonien_proefvlakken$OPNAMEPUNT, ignore.case = TRUE),
      zonien_proefvlakken$OPNAMEPUNT,
      paste0("Z", zonien_proefvlakken$OPNAMEPUNT)
    )
  )

zonien_proefvlakken <- zonien_proefvlakken[,c(10,11,13,14)]


OPNAME_TABEL_T7 <- left_join(OPNAME_TABEL_T7, zonien_proefvlakken, by ="OPNAME_IDENTIFICATIE")
#zeer weinig overlap
#beter werken met qry op bestands niveau


#Bestandsniveau ####
#T4, T7 en zonien_BHI



#T7
T7[T7$Bosplaats_BHI=='Grotehut',29] <- 'Grote Hut'
T7[T7$Bosplaats_BHI=='RoodKlooster',29] <- 'Rood Klooster'
T7[T7$Bosplaats_BHI=='Smeyberg',28] <- 'Smeyberg'


#T7[T7$Bosplaats_BHI=='ParkvanTervuren',28] <- 'Park Tervuren'
#T7[T7$Bosplaats_BHI=='ParkvanTervuren',29] <- 'Hooghout'


T7$Langenaam <-  paste(T7$BOS_BHI, T7$Bosplaats_BHI, T7$Perceel_BHI, T7$Bestand_BHI, sep =", ")
T7[T7$Langenaam == 'Zoniënwoud, Ticton, 1, V',32] <- 'Zoniënwoud, Ticton, 1, V1'
zonien_BHI_simpel <- zonien_BHI[,c(2,8,11)]

T7_shape <- left_join(T7,zonien_BHI_simpel, by = 'Langenaam')
A <- T7[is.na(T7_shape$Hectare),] #enkel Park Tervuren niet gelinkt


#T4
T4[T4$Bosplaats_BHI=='Grotehut',29] <- 'Grote Hut'
T4[T4$Bosplaats_BHI=='RoodKlooster',29] <- 'Rood Klooster'
T4[T4$Bosplaats_BHI=='Smeyberg',28] <- 'Smeyberg'
T4[T4$Bosplaats_BHI=='Sint-Hubertus',29] <- 'St Hubertus'



T4$Langenaam <-  paste(T4$BOS_BHI, T4$Bosplaats_BHI, T4$Perceel_BHI, T4$Bestand_BHI, sep =", ")
T4[T4$Langenaam == 'Zoniënwoud, Ticton, 1, V',32] <- 'Zoniënwoud, Ticton, 1, V1'
T4_shape <- left_join(T4,zonien_BHI_simpel, by = 'Langenaam')
B <- T4[is.na(T4_shape$Hectare),] #enkel Park Tervuren niet gelinkt

T7_shape_simple <- T7_shape[,c(4:7,16,19:22,28:34)]
T4_shape_simple <- T4_shape[,c(4:7,16,18:23,28:34)]


T7_shape_simple$BMS <- as.factor(T7_shape_simple$BMS)
summary(T7_shape_simple$BMS)
T7_shape_simple$BMS <- as.character(T7_shape_simple$BMS)

T7_shape_simple[T7_shape_simple$BMS == 'Beuk',5] <- 'Fagus sylvatica'
T7_shape_simple[T7_shape_simple$BMS == 'Zomereik',5] <- 'Quercus robur'
T7_shape_simple[T7_shape_simple$BMS == 'Amerikaanse eik',5] <- 'Quercus rubra'
T7_shape_simple[T7_shape_simple$BMS == 'Amerikaanse vogelkers',5] <- 'Prunus serotina'
T7_shape_simple[T7_shape_simple$BMS == 'esdoorn (G)',5] <- 'Acer spec.'
T7_shape_simple[T7_shape_simple$BMS == 'berk (G)',5] <- 'Betula spec.'
T7_shape_simple[T7_shape_simple$BMS == 'Haagbeuk',5] <- 'Carpinus betulus'
T7_shape_simple[T7_shape_simple$BMS == 'Gewone es',5] <- 'Fraxinus excelsior'
T7_shape_simple[T7_shape_simple$BMS == 'cultuurpopulier',5] <- 'Populus spec.'
T7_shape_simple[T7_shape_simple$BMS == 'Eenstijlige meidoorn',5] <- 'Crateagus monogyna'
T7_shape_simple[T7_shape_simple$BMS == 'Gladde iep (Gewone iep, Veldiep)',5] <- 'Ulmus minor'
T7_shape_simple[T7_shape_simple$BMS == 'Grove den (Gewone den)',5] <- 'Pinus sylvestris'
T7_shape_simple[T7_shape_simple$BMS == 'Japanse lork',5] <- 'Larix kaempferi'
T7_shape_simple[T7_shape_simple$BMS == 'paardekastanje (G)',5] <- 'Aesculus'
T7_shape_simple[T7_shape_simple$BMS == 'Ruwe berk',5] <- 'Betula pendula'
T7_shape_simple[T7_shape_simple$BMS == 'Sitkaspar',5] <- 'Picea sitchensis'
T7_shape_simple[T7_shape_simple$BMS == 'Tulpeboom',5] <- 'Liriodendron tulipifera'
T7_shape_simple[T7_shape_simple$BMS == 'Wintereik',5] <- 'Quercus petraea'
T7_shape_simple[T7_shape_simple$BMS == 'Zachte berk',5] <- 'Betula pubescens'
T7_shape_simple[T7_shape_simple$BMS == 'Zomerlinde (Grootbladige linde)',5] <- 'Tillia platyphyllos'
T7_shape_simple[T7_shape_simple$BMS == 'chamaecyparis (schijncipres) (G)',5] <- 'Chamaecyparis'
T7_shape_simple[T7_shape_simple$BMS == 'Gewone esdoorn',5] <- 'Acer pseudoplatanus'
T7_shape_simple[T7_shape_simple$BMS == 'Grauwe wilg',5] <- 'Salix cinerea'
T7_shape_simple[T7_shape_simple$BMS == 'lork (G)',5] <- 'Larix spec.'
T7_shape_simple[T7_shape_simple$BMS == 'Plataan',5] <- 'Platanus spec.'
T7_shape_simple[T7_shape_simple$BMS == 'Ratelpopulier (Trilpopulier, Esp)',5] <- 'Populus tremula'
T7_shape_simple[T7_shape_simple$BMS == 'Spaanse aak (Veldesdoorn)',5] <- 'Acer campestre'
T7_shape_simple[T7_shape_simple$BMS == 'Witte els (Grauwe els)',5] <- 'Alnus incana'
T7_shape_simple[T7_shape_simple$BMS == 'Zoete kers (Boskers)',5] <- 'Prunus avium'
T7_shape_simple[T7_shape_simple$BMS == 'Douglasspar',5] <- 'Pseudotsuga menziesii'
T7_shape_simple[T7_shape_simple$BMS == 'Fijnspar',5] <- 'Picea abies'
T7_shape_simple[T7_shape_simple$BMS == 'Gewone vlier (zwarte vlier)',5] <- 'Sambucus nigra'
T7_shape_simple[T7_shape_simple$BMS == 'Hazelaar',5] <- 'Corylus avellana'
T7_shape_simple[T7_shape_simple$BMS == 'linde (G)',5] <- 'Tilia spec.'
T7_shape_simple[T7_shape_simple$BMS == 'Ruwe iep',5] <- 'Ulmus glabra'
T7_shape_simple[T7_shape_simple$BMS == 'Tamme kastanje',5] <- 'Castanea sativa'
T7_shape_simple[T7_shape_simple$BMS == 'hemlockspar (G)',5] <- 'Tsuga'
T7_shape_simple[T7_shape_simple$BMS == 'Wilde lijsterbes (Gewone lijsterbes)',5] <- 'Sorbus aucuparia'
T7_shape_simple[T7_shape_simple$BMS == 'Boswilg',5] <- 'Salix caprea'
T7_shape_simple[T7_shape_simple$BMS == 'Witte paardekastanje',5] <- 'Aesculus hippocastanum'
T7_shape_simple[T7_shape_simple$BMS == 'Zwarte els',5] <- 'Alnus glutinosa'
T7_shape_simple[T7_shape_simple$BMS == 'Corsicaanse den',5] <- 'Pinus nigra'
T7_shape_simple[T7_shape_simple$BMS == 'Draaiden',5] <- 'Pinus contorta'
T7_shape_simple[T7_shape_simple$BMS == 'Grauwe abeel',5] <- 'Populus xcanescens'
T7_shape_simple[T7_shape_simple$BMS == 'Schietwilg',5] <- 'Salix alba'
T7_shape_simple[T7_shape_simple$BMS == 'Vogelkers',5] <- 'Prunus padus'
T7_shape_simple[T7_shape_simple$BMS == 'Zwarte walnoot',5] <- 'Juglans nigra'
T7_shape_simple[T7_shape_simple$BMS == 'zilverspar (G)',5] <- 'Abies alba'


T4_shape_simple$BMS <- as.factor(T4_shape_simple$BMS)
summary(T4_shape_simple$BMS)
T4_shape_simple$BMS <- as.character(T4_shape_simple$BMS)

T4_shape_simple[T4_shape_simple$BMS == 'Amerikaanse eik',5] <- 'Quercus rubra'
T4_shape_simple[T4_shape_simple$BMS == 'Amerikaanse vogelkers',5] <- 'Prunus serotina'
T4_shape_simple[T4_shape_simple$BMS == 'berk (G)',5] <- 'Betula spec.'
T4_shape_simple[T4_shape_simple$BMS == 'Beuk',5] <- 'Fagus sylvatica'
T4_shape_simple[T4_shape_simple$BMS == 'Boswilg',5] <- 'Salix caprea'
T4_shape_simple[T4_shape_simple$BMS == 'Corsikaanse den',5] <- 'Pinus nigra'
T4_shape_simple[T4_shape_simple$BMS == 'Douglasspar',5] <- 'Pseudotsuga menziesii'
T4_shape_simple[T4_shape_simple$BMS == 'Europese lork',5] <- 'Larix decidua'
T4_shape_simple[T4_shape_simple$BMS == 'Fijnspar',5] <- 'Picea abies'
T4_shape_simple[T4_shape_simple$BMS == 'Gewone es',5] <- 'Fraxinus excelsior'
T4_shape_simple[T4_shape_simple$BMS == 'Gewone esdoorn',5] <- 'Acer pseudoplatanus'
T4_shape_simple[T4_shape_simple$BMS == 'Gewone vlier (zwarte vlier)',5] <- 'Sambucus nigra'
T4_shape_simple[T4_shape_simple$BMS == 'Grauwe wilg',5] <- 'Salix cinerea'
T4_shape_simple[T4_shape_simple$BMS == 'Grove den (Gewone den)',5] <- 'Pinus sylvestris'
T4_shape_simple[T4_shape_simple$BMS == 'Haagbeuk',5] <- 'Carpinus betulus'
T4_shape_simple[T4_shape_simple$BMS == 'Hazelaar',5] <- 'Corylus avellana'
T4_shape_simple[T4_shape_simple$BMS == 'hemlockspar (G)',5] <- 'Tsuga'
T4_shape_simple[T4_shape_simple$BMS == 'Japanse lork',5] <- 'Larix kaempferi'
T4_shape_simple[T4_shape_simple$BMS == 'linde (G)',5] <- 'Tilia spec.'
T4_shape_simple[T4_shape_simple$BMS == 'lork (G)',5] <- 'Larix spec.'
T4_shape_simple[T4_shape_simple$BMS == 'meidoorn (G)',5] <- 'Crateagus spec.'
T4_shape_simple[T4_shape_simple$BMS == 'Noorse esdoorn',5] <- 'Acer platanoides'
T4_shape_simple[T4_shape_simple$BMS == 'Robinia (Valse Acacia)',5] <- 'Robinia pseudoacacia'
T4_shape_simple[T4_shape_simple$BMS == 'Ruwe berk',5] <- 'Betula pendula'
T4_shape_simple[T4_shape_simple$BMS == 'Spaanse aak (Veldesdoorn)',5] <- 'Acer campestre'
T4_shape_simple[T4_shape_simple$BMS == 'Tamme kastanje',5] <- 'Castanea sativa'
T4_shape_simple[T4_shape_simple$BMS == 'Vogelkers',5] <- 'Prunus padus'
T4_shape_simple[T4_shape_simple$BMS == 'Wilde lijsterbes (Gewone lijsterbes)',5] <- 'Sorbus aucuparia'
T4_shape_simple[T4_shape_simple$BMS == 'Wintereik',5] <- 'Quercus petraea'
T4_shape_simple[T4_shape_simple$BMS == 'Witte els (Grauwe els)',5] <- 'Alnus incana'
T4_shape_simple[T4_shape_simple$BMS == 'Zachte berk',5] <- 'Betula pubescens'
T4_shape_simple[T4_shape_simple$BMS == 'Zoete kers (Boskers)',5] <- 'Prunus avium'
T4_shape_simple[T4_shape_simple$BMS == 'Zomereik',5] <- 'Quercus robur'
T4_shape_simple[T4_shape_simple$BMS == 'Zwarte els',5] <- 'Alnus glutinosa'


colnames(T4_shape_simple) <- c('BOSDOMEIN','BOS_BOSPLAATS_OMSCH','NR','BOSBESTAND_OMSCH','Species','circumference class','Circumference at 1.3m (cm)', 'Height1','Height2','Height3','Amount trees','BOS_BHI','Bosplaats_BHI','Perceel_BHI','Bestand_BHI', 'Langenaam', 'Stand area (ha)', 'geometry')


colnames(T7_shape_simple) <- c('BOSDOMEIN','BOS_BOSPLAATS_OMSCH','NR','BOSBESTAND_OMSCH','Species','Height (m)','Circumference at 1.3m (cm)','BA m²','Volume (m³)','BOS_BHI','Bosplaats_BHI','Perceel_BHI','Bestand_BHI', 'Langenaam', 'Stand area (ha)', 'geometry')

#remove tervuren
T7_shape_simple <- T7_shape_simple[!is.na(T7_shape_simple$`Stand area (ha)`),]
T4_shape_simple <- T4_shape_simple[!is.na(T4_shape_simple$`Stand area (ha)`),]


st_write(T7_shape_simple, dsn =  "C:/Users/yannick_dewin/Documents/Bosreservaten/LandClim",layer="T7_shape_simple.shp", driver="ESRI Shapefile")

st_write(T4_shape_simple, dsn =  "C:/Users/yannick_dewin/Documents/Bosreservaten/LandClim",layer="T4_shape_simple.shp", driver="ESRI Shapefile")
