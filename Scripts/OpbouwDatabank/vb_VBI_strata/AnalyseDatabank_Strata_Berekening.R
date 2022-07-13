############################################################################
###### BEREKENEN ANALYSEVARIABELEN
############################################################################

str(Plots)    

##### Statische strata - overzicht ----
str(StrataStatic_v3)  # niet echt versie3, versie die nu in db zit, dus ondertss is dat versie4 :-)!!
str(ManagementRechtenANB2013)
str(ManagementRechtenANB2019)
str(Natura2000) 
str(BOSW12)
str(BOSW15)


##### Dynamische strata - overzicht -----
str(Ownertype_VBI2)
str(UBBP_BBBP)
str(ReservaatStatuut)
str(plotStandDescriptionVBI2Orig)
str(plotStandDescriptionVBI1Orig)
str(veglayerVBI1Orig)



####----------------------------------------------------------------------
#### STATISCHE STRATA : provincie, ecodistrict, bodemtype, beheerregio ----
####----------------------------------------------------------------------
# versie3 - obv shapefiles ----
    # plotStrataUnchanged<-data.frame(IDPlots=plotGeneralDescriptionVBI2$PLOTNR,
    #                                 Province=plotGeneralDescriptionVBI2$PROVINCIE,
    #                                 EcoRegio=plotGeneralDescriptionVBI2$REGIO,
    #                                 SoilCode=plotGeneralDescriptionVBI2$CODE_1,
    #                                 SoilType=plotGeneralDescriptionVBI2$LAYER,
    #                                 ANBRegion=plotGeneralDescriptionVBI2$Regio_12,
    #                                 Gewestplan=plotGeneralDescriptionVBI2$HOOFDCODE,
    #                                 X=plotGeneralDescriptionVBI2$X,
    #                                 Y=plotGeneralDescriptionVBI2$Y)
    #
    # plotStrataUnchangedExtra <- data.frame(IDPlots = plotExtraStrata$PLOTNR,
    #                                        ForestAgeClass = plotExtraStrata$Bosleeftij,
    #                                        SoilTexture = plotExtraStrata$BodemTextu)
    #
    # plotStrataUnchangedBodemClass <- data.frame(IDPlots = plotBodemClass$PLOTNR,
    #                                        SoilGroup = plotBodemClass$Bodemassociatieklasse)
    #
    # plotStrataUnchanged <- merge (plotStrataUnchanged,plotStrataUnchangedExtra,by = "IDPlots")
    # plotStrataUnchanged <- merge (plotStrataUnchanged,plotStrataUnchangedBodemClass,by = "IDPlots",all.x=TRUE )

    # # groeperen van ecodistricten:
    # unique(plotStrataUnchanged$EcoRegio)
    # EcoRegio_zandig <- c("Ecoregio van de cuesta's","Ecoregio van de Kempen") # op vraag van Bart Roelandt en Wim Buysse
    #
    # plotStrataUnchanged$EcoRegio_Group1 <- ifelse(is.na(plotStrataUnchanged$EcoRegio), NA,
    #                                               ifelse(plotStrataUnchanged$EcoRegio %in% EcoRegio_zandig, "zandige ecoregio", "niet-zandige ecoregio"))
    #
    # t <- table(plotStrataUnchanged$EcoRegio, plotStrataUnchanged$SoilTexture)
    # write.csv2(t, "Output/Tabellen_tmp/VerdelingTextuurEcoregio")
    #
    # plotStrataUnchanged$EcoRegio_Group2 <- ifelse(is.na(plotStrataUnchanged$EcoRegio), NA,
    #                                               ifelse(plotStrataUnchanged$EcoRegio == "Ecoregio van de Kempen", "Ecoregio van de Kempen", "Ecoregio buiten de Kempen"))
    #
    # table(plotStrataUnchanged$EcoRegio_Group2, plotStrataUnchanged$SoilTexture)


# v4: obv eerder aangemaakte strata -----
plotStrataUnchanged<-StrataStatic_v3 %>%
  select (IDPlots,Province,Gewestplan,EcoRegio,EcoRegio_Group1,EcoRegio_Group2,SoilCode,SoilType,SoilTexture,SoilGroup,ForestAgeClass,Regio,RegioCode,Entiteit,Bosgroep)

str(plotStrataUnchanged)
summary(plotStrataUnchanged)

### ! nog NA's: gecheckt in basis-shapefile en aangevuld (invb2_ecod_bodem_BHR_ANB_GW_correctie20191015.shp + in  db)
  # Provincie: 31 - OK
  # Gewestplan: 8 - ok
  # EcoRegio: 2 - OK
  # SoilCode: 31 - OK
  # RegioCode: 1 - OK
  # Entiteit: 2 - OK
  # Bosgroep: 4 - OK

t <- plotStrataUnchanged %>%
  filter(is.na(Bosgroep))
t$IDPlots

# Ecoregio-groepen
  # unique(plotStrataUnchanged$EcoRegio)
  # EcoRegio_Group2: "Ecoregio van de Kempen" <-> "Ecoregio buiten de Kempen"
  # EcoRegio_Group1: "zandige ecoregio" <-> "niet-zandige ecoregio"
  # EcoRegio_zandig <- c("Ecoregio van de cuesta's","Ecoregio van de Kempen") # op vraag van Bart Roelandt en Wim Buysse   (!! Ecoregio van de kustduinen is bijgevolgd "niet-zandig"; ook bijna geen bos!)


# Natura2000 ----
str(Natura2000)

plotStrataUnchanged <- merge (plotStrataUnchanged, Natura2000, by="IDPlots",all.x=TRUE)


# 10 extra plots uit Voeren: NA => geen SBZ (bekeken op kaart) 
plotStrataUnchanged$HRL <- ifelse(is.na(plotStrataUnchanged$HRL), FALSE, plotStrataUnchanged$HRL)
plotStrataUnchanged$VRL <- ifelse(is.na(plotStrataUnchanged$VRL), FALSE, plotStrataUnchanged$VRL)
plotStrataUnchanged$SBZ <- ifelse(is.na(plotStrataUnchanged$SBZ), FALSE, plotStrataUnchanged$SBZ)




# RechtenANB en Management ----
# aangezien hier een datum opgenomen is in de variabele, gaat het om een statisch stratum 
# (periode dient niet gespecifieerd te worden)

str(ManagementRechtenANB2013)
str(ManagementRechtenANB2019)

plotStrataUnchanged <- plotStrataUnchanged %>%
  merge (ManagementRechtenANB2013, by="IDPlots",all.x=TRUE) %>%
  merge (ManagementRechtenANB2019, by="IDPlots",all.x=TRUE)


str(plotStrataUnchanged)



# BOSWIJZER2012----(later toegevoegd, rechtstreeks aan strata zoals in v4 2019-15-10)
plotStrataUnchanged<-StrataStatic_v3 
plotStrataUnchanged <- plotStrataUnchanged %>%
  merge (BOSW12, by="IDPlots",all.x=TRUE) 

# BOSWIJZER2012----(later toegevoegd, rechtstreeks aan strata zoals in v4 2019-15-10)
plotStrataUnchanged<-StrataStatic_v3 
plotStrataUnchanged <- plotStrataUnchanged %>%
  merge (BOSW15, by="IDPlots",all.x=TRUE) 

####---------------------------------------------------------------------------------
#### DYNAMISCHE STRATA : eigenaarscat, beheer, type BHPlan... ----
####--------------------------------------------------------------------------------

str(Ownertype_VBI2)
str(UBBP_BBBP)
str(ReservaatStatuut)
str(plotStandDescriptionVBI2Orig)
str(plotStandDescriptionVBI1Orig)
str(veglayerVBI1Orig)

# DYNAMISCH => per periode!!


########################
#### VBI2 -----
########################

##### standdescription (landuse, standtype) ----

str(plotStandDescriptionVBI2Orig)  # < Standdescription_segments, in FieldMapDataVal_Bosinventaris_v4.accdb (= "dbVBI2")
plotStrataDynVBI2<-data.frame(IDPlots=plotStandDescriptionVBI2Orig$IDPlots, 
                          IDSegments=plotStandDescriptionVBI2Orig$ID,
                          LandUse=plotStandDescriptionVBI2Orig$Value1a,
                          StandType=plotStandDescriptionVBI2Orig$Value1b)
nrow(plotStrataDynVBI2)
table(plotStrataDynVBI2$LandUse)
str(plotStrataDynVBI2)
summary(plotStrataDynVBI2)


##### !!alle segmenten ------

# nu enkel de segmenten met info over landuse of standtype (wegens gebruik van query in "strata_inlezen.R")
# toch belangrijk dat we alle segmenten mee hebben (ook deze zonder info over bv landuse), dan hebben we wel nog info over eig en beheer
str(segments)   # = tblPlotWeights < dbMeetproces

nrow(unique(segments[,c("IDPlots","IDSegments")]))
# 3008 vs 3191
# !! niet uniek: dubbele segmenten? 
nrow(unique(segments))
# 3008 => compleet dubbele records (alle velde identiek)
    # tst2 <- filter(segments,duplicated(segments))
    # tst3 <- segments[segments$IDPlots%in%tst2$IDPlots, ]
# (c)A: cfr. Leen: heeft te maken met feit dat sommige segmenten uit 2 ruimtelijk gescheiden delen bestaan
segments <- unique(segments)


plotStrataDynVBI2<-merge(plotStrataDynVBI2,segments[,c("IDPlots","IDSegments")],
                         by=c("IDPlots","IDSegments"),all=TRUE)
nrow(plotStrataDynVBI2)
# 5 extra records


#####  Ownertype ------
str(Ownertype_VBI2)

Ownertype_VBI2$OwnerType<-ifelse(Ownertype_VBI2$OwnerType=="Openbaar","Public","Private")


# mergen met rest van de dynamische strata
plotStrataDynVBI2<-merge(plotStrataDynVBI2,Ownertype_VBI2, by="IDPlots",all=TRUE)
length(unique(plotStrataDynVBI2$IDPlots))
# [1] 27153
# => op deze manier alle plots opgenomen in plotStrataDynVBI2

# ! IDSegments = NA voor niet-opgemeten plots: 
# om verwarring achteraf te vermijden, toch beter om daar "1" van te maken (zie verder)


###### Alle plots (27163) ------

# Alle plots in tabel opnemen (in versie3 ontbraken nog 10 niet-bosplots uit Voeren)
# => merge met tbl "Plots"
str(Plots)

plotStrataDynVBI2<-merge(plotStrataDynVBI2,Plots, by="IDPlots",all=TRUE)
length(unique(plotStrataDynVBI2$IDPlots))
# [1] 27163

# ! IDSegments = NA voor niet-opgemeten plots: beter om daar "1" van te maken
plotStrataDynVBI2 <- plotStrataDynVBI2 %>% 
  mutate(IDSegments = ifelse(is.na(IDSegments), 1, IDSegments ))

# df <- plotStrataDynVBI2[c("IDPlots", "IDSegments")]
# df[duplicated(df),]
# => geen duplicates



###### Reservaat statuut ------

str(ReservaatStatuut)

# mergen met rest van de dynamische strata
plotStrataDynVBI2<-merge(plotStrataDynVBI2,ReservaatStatuut, by="IDPlots",all.x=TRUE)
length(unique(plotStrataDynVBI2$IDPlots))
# [1] 27163
# 10 extra plots uit Voeren: NA => geen reservaat (bekeken op kaart) 
plotStrataDynVBI2$VNR <- ifelse(is.na(plotStrataDynVBI2$VNR), FALSE, plotStrataDynVBI2$VNR)
plotStrataDynVBI2$ENR <- ifelse(is.na(plotStrataDynVBI2$ENR), FALSE, plotStrataDynVBI2$ENR)
plotStrataDynVBI2$BR <- ifelse(is.na(plotStrataDynVBI2$BR), FALSE, plotStrataDynVBI2$BR)
plotStrataDynVBI2$reservaat <- ifelse(is.na(plotStrataDynVBI2$reservaat), FALSE, plotStrataDynVBI2$reservaat)



#### Al dan niet UBBP (uitgebreid bosbeheerplan) -----

# (c)A: invb2_UBBPvsBBBP.shp < Asbet (dd 22/5/2017)
str(UBBP_BBBP)
table(UBBP_BBBP$TypeBHP)

# (c)A: beslist om enkel te focussen op UBBP, geen zin om te weten of er al dan niet een beperkt beheerplan is, is niet veel meer waard dan geen beheerplan (én maak alles veel complexer)

# volgende variabelen te behouden:
      # - UBBP: T/F
      # - UBBP_MeerDan10Jr: T/F
      # - JrGoedkUBBP: integer
      # - DuurUBBP: integer (vgl met DateDendro)


# dubbels wegens 2 beheerplannen op zelfde plot => tijdelijk verder gaan met enkel UBBP
UBBP <- UBBP_BBBP %>%
  filter (TypeBHP != "BBBP")
UBBP$TypeBHP <- "UBBP"
summary(UBBP)

# groeperen per plot
UBBP <- UBBP %>%
  group_by(IDPlots, TypeBHP) %>%
  summarise(JrGoedkUBBP = min(JrGoedkBHP)) %>%    # beheerplan dat er al het langste is
  as_tibble()
UBBP

length(unique(UBBP$IDPlots))
nrow(UBBP)


# alle plots
UBBP<-merge(UBBP,Plots, by=c("IDPlots"),all.y=TRUE) 
str(UBBP)
UBBP$TypeBHP <- ifelse(is.na(UBBP$TypeBHP), "geen of beperkt beheerplan", UBBP$TypeBHP)   # onafhankelijk van DateDendro, gerelateerd aan JrGoedkUBBP


# aftoetsen aan DateDendro of UBBP er reeds was ten tijde van dendro-opname

# hier specifiek voor VBI2 -----
UBBP_VBI2 <- UBBP
UBBP_VBI2$Periode<-2

# vergelijken met date dendro VBI2 (was er al een beheerplan bij bosbouwopname?)
# daarom ook "Periode" toegevoegd
str(date_dendro) # <tblPlotDetails
str(UBBP_VBI2)

UBBP_VBI2<-merge(UBBP_VBI2,date_dendro[,c("IDPlots","Periode","DateDendro")],
                      by=c("IDPlots","Periode"),all.x=TRUE) 
length(unique(UBBP_VBI2$IDPlots))
nrow(UBBP_VBI2)

summary(UBBP_VBI2)

UBBP_VBI2$DuurUBBP <- UBBP_VBI2$DateDendro - UBBP_VBI2$JrGoedkUBBP
unique(UBBP_VBI2$DuurUBBP)

str(UBBP_VBI2)
UBBP_VBI2$UBBP <- ifelse(UBBP_VBI2$TypeBHP == "geen of beperkt beheerplan", F,
                         ifelse(UBBP_VBI2$DuurUBBP < 0, F, T))
# ook "F" als er sowieso geen BHP is
# enkel NA als DateDendro niet gekend is én er een uitgebreid beheerplan is

UBBP_VBI2$UBBP_MeerDan10Jr <- ifelse(UBBP_VBI2$TypeBHP == "geen of beperkt beheerplan", F,
                         ifelse(UBBP_VBI2$DuurUBBP < 10, F, T))


# mergen met rest van de dynamische strata ----
plotStrataDynVBI2<-merge(plotStrataDynVBI2,UBBP_VBI2[,c("IDPlots","JrGoedkUBBP", "DuurUBBP", "UBBP", "UBBP_MeerDan10Jr")],
                     by="IDPlots",all.x=TRUE)


##### RESULT VBI2 ----
str(plotStrataDynVBI2)
plotStrataDynVBI2$Periode<-2



#### VBI1 ----
########################
plotStrataDynVBI1<-data.frame(IDPlots=plotStandDescriptionVBI1Orig$PLOTNR,
                           IDSegments=1,
                           Periode=1,
                           StandType=plotStandDescriptionVBI1Orig$NAAM,
                           OwnerType=ifelse(plotStandDescriptionVBI1Orig$EIGENCAT==0,"Unknown",
                                            ifelse(plotStandDescriptionVBI1Orig$EIGENCAT==6,"Private","Public")))

# plotStrataDynVBI1$LandUse<-ifelse(plotStrataDynVBI1$StandType=="te herbebossen", "bos - kapvlakte",
#                                ifelse(plotStrataDynVBI1$StandType=="open ruimte binnen bos","open ruimte binnen bos",
#                                       "bos"))
plotStrataDynVBI1$LandUse<-ifelse(plotStrataDynVBI1$StandType=="te herbebossen", "BOS - kaalkap",
                               ifelse(plotStrataDynVBI1$StandType=="open ruimte binnen bos","FOA",
                                      "BOS"))

plotStrataDynVBI1$StandType <- plyr::revalue(plotStrataDynVBI1$StandType,c("te herbebossen"="kapvlakte"))

table(plotStrataDynVBI1$StandType)
table(plotStrataDynVBI2$StandType)
table(plotStrataDynVBI1$LandUse)
table(plotStrataDynVBI2$LandUse)


summary(plotStrataDynVBI1)
plotStrataDynVBI1$LandUse <- as.factor(plotStrataDynVBI1$LandUse)


####### Alle plots (27163) ------

# Alle plots in tabel opnemen (in versie3 ontbraken nog 10 niet-bosplots uit Voeren)
# => merge met tbl "Plots"
str(Plots)

plotStrataDynVBI1<-merge(plotStrataDynVBI1,Plots, by="IDPlots",all=TRUE)
length(unique(plotStrataDynVBI2$IDPlots))
# [1] 27163
plotStrataDynVBI1$IDSegments <- 1
plotStrataDynVBI1$Periode <- 1


#### Al dan niet UBBP (uitgebreid bosbeheerplan) -----

# hier specifiek voor VBI1 -----
UBBP_VBI1 <- UBBP
UBBP_VBI1$Periode<-1

# vergelijken met date dendro VBI1 (was er al een beheerplan bij bosbouwopname?)
# daarom ook "Periode" toegevoegd
str(date_dendro) # <tblPlotDetails
str(UBBP_VBI1)

UBBP_VBI1<-merge(UBBP_VBI1,date_dendro[,c("IDPlots","Periode","DateDendro")],
                 by=c("IDPlots","Periode"),all.x=TRUE) 
length(unique(UBBP_VBI1$IDPlots))
nrow(UBBP_VBI1)

summary(UBBP_VBI1)

UBBP_VBI1$DuurUBBP <- UBBP_VBI1$DateDendro - UBBP_VBI1$JrGoedkUBBP
unique(UBBP_VBI1$DuurUBBP)

UBBP_VBI1$UBBP <- ifelse(UBBP_VBI1$TypeBHP == "geen of beperkt beheerplan", F,
                         ifelse(UBBP_VBI1$DuurUBBP < 0, F, T))
# ook "F" als er sowieso geen BHP is
# enkel NA als DateDendro niet gekend is én er een uitgebreid beheerplan is

UBBP_VBI1$UBBP_MeerDan10Jr <- ifelse(UBBP_VBI1$TypeBHP == "geen of beperkt beheerplan", F,
                                     ifelse(UBBP_VBI1$DuurUBBP < 10, F, T))


# mergen met rest van de dynamische strata ----
plotStrataDynVBI1<-merge(plotStrataDynVBI1,UBBP_VBI1[,c("IDPlots","JrGoedkUBBP", "DuurUBBP", "UBBP", "UBBP_MeerDan10Jr")],
                         by="IDPlots",all.x=TRUE)



##### RESULT VBI1 ----
str(plotStrataDynVBI1)


# MERGE VAN VBI1 en VBI2 ----
colnames(plotStrataDynVBI1)
colnames(plotStrataDynVBI2)

# ENR, VNR, BR, reseervaat toevoegen aan plotStrataDynVBI1
plotStrataDynVBI1$VNR <- NA
plotStrataDynVBI1$ENR <- NA
plotStrataDynVBI1$BR <- NA
plotStrataDynVBI1$reservaat <- NA

plotStrataVBIComb<-rbind(plotStrataDynVBI2,plotStrataDynVBI1)
nrow(plotStrataVBIComb)
length(unique(plotStrataVBIComb$IDPlots))



####----------------------------------------------------------------------
#### Bostypologie via R-script ----
####----------------------------------------------------------------------

#### VBI2 ----

herblayerVBI2<-herblayerVBI2Orig
herblayerVBI2$Coverage<-pmax(herblayerVBI2$Coverage_date1,herblayerVBI2$Coverage_date2,na.rm=TRUE)
# (c)A: Coverage_date1 = coverage op date 1; soms immers een 2de opname in het voorjaar met andere bedekkingen)

scale<-data.frame(Coverage=1:9,
                  CoverageClass=c("r","+","1","2m","2a","2b","3","4","5"))

herblayerVBI2<-merge(herblayerVBI2,scale,by="Coverage",all.x=TRUE)

herblayerVBI2$IDPlots<-factor(herblayerVBI2$IDPlots)

speciesList$NaamWet<-paste(speciesList$Genus,speciesList$Spec,sep=" ")

herblayerVBI2<-merge(herblayerVBI2,speciesList[,c("IDSpVBI2","NaamWet")],by="IDSpVBI2",all.x=TRUE)

herblayerVBI2<-herblayerVBI2[order(herblayerVBI2$IDPlots),]


# Naamgeving conform R-functie voor bepalen bostypologie (afgeleid van softwareprogramma - wordt nu niet meer gebruikt)
opnameVBI2<-data.frame(OPNAMECODE=herblayerVBI2$IDPlots,
                       SOORT=herblayerVBI2$NaamWet,
                       BEDEKKING=herblayerVBI2$CoverageClass)

Opnames <- opnameVBI2
Opnames$chBedekking <- as.character(Opnames$BEDEKKING)
Opnames$Code <- "9"  # code van de gebruikte schaal; "9" staat voor Braun-Blanquet ("38" voor Tansley)

Opnames <- merge(Opnames, subset(Schaal, as.character(SchaalCode) == 9, c("Code", "PctWaarde")),
                 all.x = TRUE, by.x = "BEDEKKING", by.y = "Code")
unique(Opnames$chBedekking)

Opnames <- Opnames[c("OPNAMECODE","SOORT","PctWaarde")]
Opnames <- merge(Opnames, Soortenlijst[c("SPECIES_NR", "SPECIESNAME")], by.x = "SOORT", by.y = "SPECIESNAME", all.x = TRUE)
Opnames <- reshape::rename(Opnames, c("PctWaarde" = "BEDEKKING"))


# berekening verwantschap + bepaling bostype(groep)

## T1 = hoofdgroepen cfr. A, B, C,...
verwantschapT1 <- berekenVerwantschap(Opnames, Referentie, correctie = TRUE, TypologieCode = "T1")
str(verwantschapT1)
summary(verwantschapT1)
    # (c) A: verwantschapT1 bevat per plot een verwantschap (S_aj) met elk van de 11 bostypes
    # als er echter ergens een bedekking ontbreekt van een bepaalde ("kritische") soort, dan kan voor bepaalde bostypes de verwantschap niet berekend worden en ontstaat er een NA voor dat bostype (is niet het geval bij VBI1, daar zijn alle bedekkingen ingevuld)

    # => niet zomaar wegfilteren, volledige plot eigenlijk een NA, want niet genoeg info om te weten welk bostype
BostypesMissingInfo <- verwantschapT1[!is.na(verwantschapT1$BostypeCode) & is.na (verwantschapT1$S_aj),]
    # test <- Opnames[Opnames$OPNAMECODE %in% BostypesMissingInfo$OPNAMECODE, ]
    # enkel plot 144157: bij geen enkele soort bedekkingen
    # andere plots: 1 soort geen bedekking => suggestie Johnny: minimale bedekking toekennen
    # (c) A: soms ook soorten die per ongeluk aangevinkt zijn, denk ik => zo laten (wegfilteren), gaat niet om veel (1.2%)

    # > length(unique(BostypesMissingInfo$OPNAMECODE))
    # [1] 33
    # > length(unique(verwantschapT1$OPNAMECODE))
    # [1] 2750
    # slechts voor 33 plots (van de 2750 = 1.2%) het geval => deze plots wegfilteren eerder dan verwantschap te berekenen met onvolledige info (bedekking = NA, maar we weten niet of soort wel of niet voorkomt, noch in welke bedekking)
verwantschapT1 <- verwantschapT1[!(verwantschapT1$OPNAMECODE %in% unique(BostypesMissingInfo$OPNAMECODE)),]
summary(verwantschapT1)
    # het blijkt ook nog nodig om de resterende NA's weg te filteren, zoniet wordt MaxScore een NA (blijkbaar bij elke plot een lege regel: zowel BostypeCode, als S_aj = NA)
verwantschapT1 <- verwantschapT1[!is.na(verwantschapT1$S_aj),]


forestTypeGroupCodeVBI2<-ddply(verwantschapT1,.(OPNAMECODE),summarise,
                               MaxScore=max(S_aj),
                               ForestTypeGroupCode=BostypeCode[which.max(S_aj)])


## T2 = subgroepen cfr. C1, C2, C3, ...
verwantschapT2 <- berekenVerwantschap(Opnames, Referentie, correctie = TRUE, TypologieCode = "T2")
BostypesMissingInfo <- verwantschapT2[!is.na(verwantschapT2$BostypeCode) & is.na (verwantschapT2$S_aj),]
    # > length(unique(BostypesMissingInfo$OPNAMECODE))
    # [1] 33
    # > length(unique(verwantschapT2$OPNAMECODE))
    # [1] 2750
    # slechts voor 33 plots (van de 2750) het geval => deze plots wegfilteren
verwantschapT2 <- verwantschapT2[!(verwantschapT2$OPNAMECODE %in% unique(BostypesMissingInfo$OPNAMECODE)),]
    # ook nog nodig om NA's weg te filteren, zoniet wordt MaxScore een NA
verwantschapT2 <- verwantschapT2[!is.na(verwantschapT2$S_aj),]

forestTypeCodeVBI2<-ddply(verwantschapT2,.(OPNAMECODE),summarize,
                          MaxScore=max(S_aj),
                          ForestTypeCode=BostypeCode[which.max(S_aj)])


## Overzicht aanmaken van plot met bijhorende ForestTypeGroupCode T1 en ForestTypeCode T2,
        # mét MaxScore van elk => naam veranderen
forestTypeGroupCodeVBI2 <- reshape::rename (forestTypeGroupCodeVBI2, c(MaxScore = "VerwantschForestTypeGr"))
forestTypeCodeVBI2 <- reshape::rename (forestTypeCodeVBI2, c(MaxScore = "VerwantschForestType_Subniv"))
forestTypeVBI2<-merge(forestTypeGroupCodeVBI2,forestTypeCodeVBI2)
forestTypeVBI2 <-reshape::rename(forestTypeVBI2,c(OPNAMECODE="IDPlots"))

# controle
      # verwantschapT1[verwantschapT1$OPNAMECODE==77016,]
      # forestTypeCodeVBI2[forestTypeCodeVBI2$OPNAMECODE==17144,]
      # forestTypeVBI2[forestTypeVBI2$IDPlots==17144,]
      verwantschapT1[verwantschapT1$OPNAMECODE==35106,]
      verwantschapT2[verwantschapT2$OPNAMECODE==35106,]
      forestTypeVBI2[forestTypeVBI2$IDPlots==35106,]



####VBI1 ----

veglayerVBI1<-veglayerVBI1Orig

herblayerVBI1 <- veglayerVBI1[veglayerVBI1$Layer=="k",]

herblayerVBI1<-merge(herblayerVBI1,scale,by="Coverage",all.x=TRUE)

herblayerVBI1$IDPlots<-factor(herblayerVBI1$IDPlots)

herblayerVBI1<-merge(herblayerVBI1,speciesList[,c("IDSpVBI1","NaamWet")],by="IDSpVBI1",all.x=TRUE)

herblayerVBI1<-herblayerVBI1[order(herblayerVBI1$IDPlots),]


# Naamgeving conform R-functie voor bepalen bostypologie
opnameVBI1<-data.frame(OPNAMECODE=herblayerVBI1$IDPlots,
                       SOORT=herblayerVBI1$NaamWet,
                       BEDEKKING=herblayerVBI1$CoverageClass)

Opnames <- opnameVBI1
Opnames$chBedekking <- as.character(Opnames$BEDEKKING)
Opnames$Code <- "9"

Opnames <- merge(Opnames, subset(Schaal, as.character(SchaalCode) == 9, c("Code", "PctWaarde")),
                 all.x = TRUE, by.x = "BEDEKKING", by.y = "Code")
Opnames <- Opnames[c("OPNAMECODE","SOORT","PctWaarde")]
Opnames <- merge(Opnames, Soortenlijst[c("SPECIES_NR", "SPECIESNAME")], by.x = "SOORT", by.y = "SPECIESNAME", all.x = TRUE)
Opnames <- reshape::rename(Opnames, c("PctWaarde" = "BEDEKKING"))


# berekening verwantschap + bepaling bostype(groep)

## T1 = hoofdgroepen cfr. A, B, C, ...
verwantschapT1 <- berekenVerwantschap(Opnames, Referentie, correctie = TRUE, TypologieCode = "T1")
BostypesMissingInfo <- verwantschapT1[!is.na(verwantschapT1$BostypeCode) & is.na (verwantschapT1$S_aj),]
    # > length(unique(BostypesMissingInfo$OPNAMECODE))
    # [1] 0
    # geen ontbrekende bedekkingen in VBI1
    # wel nog aangeraden om NA's (lege regels, zie hoger bij VBI2) te verwijderen
verwantschapT1 <- verwantschapT1[!is.na(verwantschapT1$S_aj),]
forestTypeGroupCodeVBI1<-ddply(verwantschapT1,.(OPNAMECODE),summarize,
                               MaxScore=max(S_aj),
                               ForestTypeGroupCode=BostypeCode[which.max(S_aj)])


## T2 = subgroepen cfr. C1, C2, C3, ...
verwantschapT2 <- berekenVerwantschap(Opnames, Referentie, correctie = TRUE, TypologieCode = "T2")
BostypesMissingInfo <- verwantschapT2[!is.na(verwantschapT2$BostypeCode) & is.na (verwantschapT2$S_aj),]
    # > length(unique(BostypesMissingInfo$OPNAMECODE))
    # [1] 0
    # geen ontbrekende bedekkingen in VBI1
    # wel nog aangeraden om NA's (lege regels, zie hoger bij VBI2) te verwijderen
verwantschapT2 <- verwantschapT2[!is.na(verwantschapT2$S_aj),]
forestTypeCodeVBI1<-ddply(verwantschapT2,.(OPNAMECODE),summarize,
                          MaxScore=max(S_aj),
                          ForestTypeCode=BostypeCode[which.max(S_aj)])

# test_totale_verwantschap <-ddply(verwantschapT2,.(OPNAMECODE),summarize,
#                           verwantschap_tot=sum(S_aj))

## Overzicht aanmaken van plot met bijhorende ForestTypeGroupCode T1 en ForestTypeCode T2,
        # mét MaxScore van elk => naam veranderen
forestTypeGroupCodeVBI1 <- reshape::rename (forestTypeGroupCodeVBI1, c(MaxScore = "VerwantschForestTypeGr"))
forestTypeCodeVBI1 <- reshape::rename (forestTypeCodeVBI1, c(MaxScore = "VerwantschForestType_Subniv"))
forestTypeVBI1<-merge(forestTypeGroupCodeVBI1,forestTypeCodeVBI1)
forestTypeVBI1 <-reshape::rename(forestTypeVBI1,c(OPNAMECODE="IDPlots"))

# controle
      # verwantschapT1[verwantschapT1$OPNAMECODE==77016,]
      # forestTypeVBI1[forestTypeVBI1$IDPlots==347088,]
      # forestTypeVBI2[forestTypeVBI2$IDPlots==17144,]



#### data samenvoegen ----
forestTypeVBI1$Periode<-1
forestTypeVBI2$Periode<-2

forestType<-rbind(forestTypeVBI1,forestTypeVBI2)



