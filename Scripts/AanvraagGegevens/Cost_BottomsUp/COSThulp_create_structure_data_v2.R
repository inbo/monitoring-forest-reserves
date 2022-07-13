#15/04/2020
rm(list=ls())
library("readxl")
library("tidyverse")

#names of target variables
struc<-c("siteID","plotID","treeID","genus","species","treesp","treedb","treeht","treevol","cenver","azimuth","distan","coordx","coordy","crowndep","crownrad1","crownrad2","crownrad3","crownrad4","treevit","alive","decsta","plosiz","weisiz","TreMn1","TreMn2","TreMn3")

###original structure data (input)

#set the directory where the files is
setwd("/Users/francescochianucci/Dropbox/PersonaLI/Missioni_estero/COST/COST_CA18207_BOTTOMS-UP/database/AC_data/data_AC/originali/db_coppice")
dir()

#load input data based on file name
"temp_Tbl_Piedilista_Macroplot.xls" %>% 
  read_excel() -> data

#eliminates other plots not considered in COST using a column ID (in the example, column "ID_Area")
data<-data %>% 
  filter(str_detect(ID_Area, 'cate|isc|mass')) %>% 
  filter(!ID_Area %in%c('fsCONTRcate','fsTScate'))

#check you are retaining your desired plots
unique(data$ID_Area)#ok

#create siteid column from a original column with different name
data<-data %>% 
  mutate(siteID=as.character(case_when(
    ID_Sito == "Buca Zamponi"~"IT_AC1_CAT",
    ID_Sito == "Poggio Pievano"~"IT_AC2_MET",
    ID_Sito == "Is_Cannoneris"~"IT_AC3_ISC"
  )))

# compare original siteid and COST siteid
table(data$siteID,data$ID_Sito) # check ok

#rename some columns to the target columns
data<-data %>% 
  dplyr::rename(genus=Genere,species=Specie,treedb=DBH,plosiz=Superficie_Macroplot) %>% 
  mutate(plotID=as.character(ID_Area),treeID=as.character(Num_Pianta))

#create treesp columns by pasting genus and spp
data<-data %>% 
  # mutate(treesp=str_c(tolower(str_sub(Genus,1,3)),str_sub(Species,1,3),sep="."))
  mutate(treesp=str_c(genus, species,sep=" "))

#create "weisiz" column which is 1 hectare divided by the size of the plot
data<-data %>% mutate (weisiz=10000/plosiz) 

#in this case all data are alive so I created "alive" column with all values = 1
data<- data  %>% mutate(alive=1)


#Now I created an empty output dataframe which will be used to append the data
output<-data.frame(matrix(NA,ncol=length(struc),nrow=0))
names(output)<-struc
#it is important that IDs cols must be character type
output$siteID<-as.character(output$siteID);output$plotID<-as.character(output$plotID);output$treeID<-as.character(output$treeID)

#Now I checked which columns are missing from my original dataset compared to target columns
out<-data %>% select(one_of(struc))

#The missed columns from the original data now are filled with NA assuming I have not information on them:
output<-bind_rows(output,out)  

#save output
write.csv(output,"rawdata_structure_IT_AC_v2.csv",row.names = F)

#check on basal area
output %>% 
  mutate(g=pi/4*((treedb/100)^2)) %>% 
  group_by(plotID) %>% 
  summarise(G=sum(g),w=mean(weisiz),Gha=G*w)

