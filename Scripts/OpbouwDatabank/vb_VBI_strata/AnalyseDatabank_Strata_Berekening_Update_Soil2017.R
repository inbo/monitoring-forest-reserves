############################################################################
###### BEREKENEN ANALYSEVARIABELEN
############################################################################

str(Plots)    

##### Statische strata - overzicht ----
str(StrataStatic_v3)  # niet echt versie3, versie die nu in db zit, dus ondertss is dat versie4 :-)!!
str(ManagementRechtenANB2013)
str(ManagementRechtenANB2019)
str(Natura2000) 

str(Soil2017)
str(plotSoilGroup)  # input Wim Buysse


####----------------------------------------------------------------------
#### STATISCHE STRATA : provincie, ecodistrict, bodemtype, beheerregio ----
####----------------------------------------------------------------------

# v4: obv eerder aangemaakte strata -----
plotStrataUnchanged<-StrataStatic_v3 

str(plotStrataUnchanged)
summary(plotStrataUnchanged)

# Ecoregio-groepen
  # unique(plotStrataUnchanged$EcoRegio)
  # EcoRegio_Group2: "Ecoregio van de Kempen" <-> "Ecoregio buiten de Kempen"
  # EcoRegio_Group1: "zandige ecoregio" <-> "niet-zandige ecoregio"
  # EcoRegio_zandig <- c("Ecoregio van de cuesta's","Ecoregio van de Kempen") # op vraag van Bart Roelandt en Wim Buysse   (!! Ecoregio van de kustduinen is bijgevolgd "niet-zandig"; ook bijna geen bos!)


# v4: bijwerken bodem obv kaart 2017 -----
str(Soil2017)

plotStrataUnchanged <- plotStrataUnchanged %>% 
  select(-SoilCode, - SoilType, -SoilTexture, -SoilGroup) %>%  # oude info verwijderen
  left_join (Soil2017, by = c("IDPlots"))
  
str(plotSoilGroup) # aangemaakt obv oude bodemkaart => enkel geïntersseerd in link bodemserie en bodemgroep
SoilGroup <- plotSoilGroup %>% 
  select(-PLOTNR, -OBJECTID, -LAYER) %>% 
  unique()

plotStrataUnchanged <- plotStrataUnchanged %>% 
  left_join(SoilGroup, by = c("SoilCode" = "CODE_1")) %>% 
  select(-temp, -Textuur) %>% 
  rename(SoilGroup = "Bodemassociatieklasse", SoilDrainageGroup = "Vocht") %>% 
  unique()
str(plotStrataUnchanged)


# welke nog niet gekoppeld? - heel ambachtelijk :-)
t5 <- plotStrataUnchanged %>% 
  filter(is.na(SoilGroup) & SoilType != "Antropogeen") %>% 
  filter(str_detect(SoilType, "zand") | str_detect(SoilType, "leem")) 

table(t5$SoilType)
# 9 nat zand en 1 droog zand en 2 vochtigzand => Zdroog_nat_vochtig
# 2 droge leem => Adroog
# 1 vochtig zandleem => Lvochtig
unique(t5$SoilType)
unique(SoilGroup$Bodemassociatieklasse)

plotStrataUnchanged <- plotStrataUnchanged %>% 
  mutate(SoilGroup =  ifelse(is.na(SoilGroup) & SoilType == "Nat zand", 
                                            "Znat", SoilGroup)) %>% 
  mutate(SoilGroup =  ifelse(is.na(SoilGroup) & SoilType == "Droog zand", 
                                     "Zdroog", SoilGroup)) %>% 
  mutate(SoilGroup =  ifelse(is.na(SoilGroup) & SoilType == "Vochtig zand", 
                                     "Zvochtig", SoilGroup)) %>% 
  mutate(SoilGroup =  ifelse(is.na(SoilGroup) & SoilType == "Vochtig zandleem", 
                                     "Lvochtig", SoilGroup)) %>% 
  mutate(SoilGroup =  ifelse(is.na(SoilGroup) & SoilType == "Droge leem", 
                                     "Adroog", SoilGroup)) 

plotStrataUnchanged <- plotStrataUnchanged %>% 
  mutate(SoilDrainageGroup =  ifelse(is.na(SoilDrainageGroup) & SoilType == "Nat zand", 
                                     "nat", SoilDrainageGroup)) %>% 
  mutate(SoilDrainageGroup =  ifelse(is.na(SoilDrainageGroup) & SoilType == "Droog zand", 
                                     "droog", SoilDrainageGroup)) %>% 
  mutate(SoilDrainageGroup =  ifelse(is.na(SoilDrainageGroup) & SoilType == "Vochtig zand", 
                                     "vochtig", SoilDrainageGroup)) %>% 
  mutate(SoilDrainageGroup =  ifelse(is.na(SoilDrainageGroup) & SoilType == "Vochtig zandleem", 
                                     "vochtig", SoilDrainageGroup)) %>% 
  mutate(SoilDrainageGroup =  ifelse(is.na(SoilDrainageGroup) & SoilType == "Droge leem", 
                                     "droog", SoilDrainageGroup)) 



str(plotStrataUnchanged)
# Opgepast: 15 plots zonder bodemtype, waaronder één bosplot (188051: deze bosplot kreeg ook obv oude bodemkaart geen bodemtype toegekend)
