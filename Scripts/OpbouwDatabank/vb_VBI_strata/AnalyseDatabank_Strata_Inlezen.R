####  ALGEMEEN ----

# (c)A: doel van scripts "AnalyseDatabank_Strata_Inlezen.R" en "AnalyseDatabank_Strata_Berekening.R" is de aanmaak van
# VBI_Strata_v2019-04-30.accdb, met daarin volgende tabellen:
#   - tblPlotStrataDynamic
#   - tblPlotStrataStatic
#   - tblN2000Habitat_versie20140324 (iets van Toon Westra)

# bij de eerste aanmaak van deze tabellen werd info uit verschillende bronnen bij elkaar geplaatst (zie versie3)
# bij versie 4 is het eenvoudiger om (deels) te vertrekken van de tabellen die reeds in VBI_Strata_v2018-04-09.mdb zitten (m.u.v. de dynamische strata)

# bij versie3 werd tblPlotStrataDynamic enkel aangemaakt voor de bosplots, bij versie4 werd beslist alle plots (27.163 records) in tblPlotStrataDynamic te behouden - ook de niet-bosplots 

# XY-coordinaten horen niet thuis in strata!!



############################################################################
##### DATA INLEZEN ----
############################################################################

####---------------------
#### Info mbt meetproces ----
####---------------------

# lijst van alle plots < tbl MeetnetDetails (27163 plots)
# lijst van alle segmenten (berekend in "Analysedb_meetproces_xxxxx": < "tblPlotWeights" : enkel periode 2 )
# lijst met date_dendro (VBI1+2)
connectieMeetproces <- odbcConnectAccess2007(dbMeetproces) #dit is een accdb file

MeetnetDetails <- sqlFetch(connectieMeetproces, "MeetnetDetails", stringsAsFactors = TRUE);str(MeetnetDetails)
segments <- sqlFetch(connectieMeetproces,"tblPlotWeights");str(segments)
date_dendro <- sqlFetch(connectieMeetproces,"tblPlotDetails");str(date_dendro)
tblRecordsVBI23 <- sqlFetch(connectieMeetproces, "tblRecordsVBI23")
odbcClose(connectieMeetproces)


Plots <- MeetnetDetails %>% 
  dplyr::select(IDPLOTS) %>% 
  unique() %>%
  reshape::rename (c(IDPLOTS = "IDPlots"))


####---------------------
#### Gegevens VBI_Strata_v3.accdb ----
####---------------------
# !! 10 plots van Voeren zaten her initieel nog niet in => statische strata aanvullen in access-db (OK - gebeurd)
# dynamic strata OK voor VBI1 ! verder aanvullen voor VBI2
# dynamic strata aanmaken voor alle plots, niet enkel voor bosplots => 54.326 records

connectieStrata <- odbcConnectAccess2007(dbStrata)

StrataStatic_v3 <- sqlFetch(connectieStrata,"tblPlotStrataStatic"); str(StrataStatic_v3)
      # provincie, ecodistrict, bodemtype (alle gridpunten)
# StrataDynamic_v3 <- sqlFetch(connectieStrata,"tblPlotStrataDynamic"); str(StrataDynamic_v3)
      # ! bestaat enkel voor de reeds opgemeten bosplots => geen zin om dit opnieuw in te laden

odbcClose(connectieStrata)


####---------------------
#### Soiltype obv bodemkaart 2017 ----
####---------------------

Soil2017<-sf::st_read(shpSoil2017)   # aangemaakt door M.Stevens obv vernieuwe bodemkaart
class(Soil2017)
# XY_corrected: obv XY theoretisch en ingemeten 2eSET
# str(Soil2017)

Soil2017 <- Soil2017 %>%
  st_drop_geometry() %>%
  dplyr::select(PLOTNR,Bodemtype,Gegenerali,Textuurkla,Drainagekl) %>%
  reshape::rename(c(PLOTNR="IDPlots",Bodemtype="SoilCode",Gegenerali= "SoilType", Textuurkla="SoilTexture", Drainagekl="SoilDrainage")) %>%
  as_tibble() 
Soil2017


# in bepaalde gevallen ontbreekt textuurklasse, terwijl de bodem toch als zandbodem/duin getypeerd werd
t <- Soil2017 %>% 
  filter(X_M != 0) %>% # ingemeten plots
  filter(is.na(Textuurkla) & Gegenerali != "Antropogeen")
table(t$Gegenerali)
nrow(t)
# [1] 30


t2 <- Soil2017 %>% 
  # filter(X_M != 0) %>% 
  filter(is.na(Textuurkla) & Gegenerali != "Antropogeen")
nrow(t2)
# [1] 1576

# table(t2$Gegenerali)


# => toevoegen van textuurklasse waar duidelijk vergeten bij
    # - Z indien veld "SoilCode" hoofdletter "Z" bevat
    # - X indien veld "SoilType" "landduin" bevat
    # - dekklei: geen textuurklasse toegekend, want krijgt nergens een textuurklasse toegekend
Soil2017 <- Soil2017 %>% 
  mutate(SoilTexture = as.character(SoilTexture), SoilType = as.character(SoilType)) %>% 
  mutate(SoilTexture = ifelse(is.na(SoilTexture), 
                              ifelse(str_detect(SoilType, "landduin"), 
                                     "X", 
                                     ifelse(str_detect(SoilCode, "Z") & SoilCode != "OZ", 
                                            "Z",
                                            SoilTexture)),
                              SoilTexture))

# controle
t3 <- Soil2017 %>% 
  filter(str_detect(SoilType, "zand") & SoilTexture != "Z")
t4 <- Soil2017 %>% 
  filter(SoilTexture == "Z")
table(t4$SoilType)


# Moet opnieuw gekoppeld worden aan de grote bodemgroepen die W. Buysse voorgesteld heeft
plotSoilGroup <- read.csv2(soil_group, stringsAsFactors = FALSE)  # afgeleide bodemgroepen (< Wim Buysse: 13 klasses, ikv QD)




####---------------------
#### Ownertype & Management2013 ----
####---------------------
OwnerManagement2013<-sf::st_read(shpStrata)   # < invb2_ecod_bodem_BHR_ANB_GW.shp
class(OwnerManagement2013)
# = "invb2_ecod_bodem_BHR_ANB_GW.shp" - bevat theoretische XY
# str(OwnerManagement2013_VBI2)
# vooral nodig wegens info over eigenaarscategorie en beheerder (= dynamisch)
str(OwnerManagement2013)
# slechts 21753 plots => 10 NA's (10 plots in Voeren zonder bos)
OwnerManagement2013 <- OwnerManagement2013 %>%
  dplyr::select(PLOTNR,EigenaarsC,RechtenANB,Beheerder) %>%
  reshape::rename(c(PLOTNR="IDPlots",EigenaarsC="OwnerType",RechtenANB= "RechtenANB_2013", Beheerder="Management2013")) %>%
  st_drop_geometry() %>%
  as_tibble() 
OwnerManagement2013

Ownertype_VBI2 <- OwnerManagement2013 %>%
  select (IDPlots,OwnerType)

ManagementRechtenANB2013 <- OwnerManagement2013 %>%
  select (IDPlots,RechtenANB_2013,Management2013)



####---------------------
### RechtenANB - Management2019 ----
####---------------------
ManagementRechtenANB2019 <- sf::st_read(shpStrata4)   # invb3_RechtenANB.shp
str(ManagementRechtenANB2019)
ManagementRechtenANB2019$RechtenANB <- as.character(ManagementRechtenANB2019$RechtenANB)

ManagementRechtenANB2019 <-  ManagementRechtenANB2019 %>%
  st_drop_geometry() %>%
  dplyr::select(PLOTNR, RechtenANB) %>%
  reshape::rename(c(PLOTNR="IDPlots", RechtenANB="RechtenANB_2019")) %>%
  dplyr::mutate(RechtenANB_2019 = ifelse (is.na(RechtenANB_2019), "geen specifieke rechten", RechtenANB_2019)) %>%
  dplyr::mutate(Management2019 = ifelse (RechtenANB_2019 %in% c("geen specifieke rechten","Technisch beheer conform bosdecreet"), NA, "ANB")) %>%
  as.tibble()
ManagementRechtenANB2019

ManagementRechtenANB2019$RechtenANB_2019 <- as.factor(ManagementRechtenANB2019$RechtenANB_2019)
ManagementRechtenANB2019$Management2019 <- as.factor(ManagementRechtenANB2019$Management2019)

unique(ManagementRechtenANB2019$RechtenANB_2019)

# ? dubbele records
length(unique(ManagementRechtenANB2019$IDPlots))
nrow(ManagementRechtenANB2019)
# !! niet uniek: 3 dubbele plotID's

# in detail bekeken, en zonder FID's is de info uniek per plot 
# tst2 <- ManagementRechtenANB2019[duplicated(ManagementRechtenANB2019$IDPlots),]
# ManagementRechtenANB2019[ManagementRechtenANB2019$IDPlots%in%tst2$IDPlots, ]
ManagementRechtenANB2019 <- unique(ManagementRechtenANB2019)



####---------------------
#### UBBP - BBBP ----
####---------------------

# VBI1 & VBI2
UBBP_BBBP <-sf::st_read(shpStrata2)   # = "invb2_UBBPvsBBBP.shp"
str(UBBP_BBBP)
# datums van géén beheerplan worden fout ingelezen (als 1899)
UBBP_BBBP$JrGoedkBHP <- ifelse(!is.na(UBBP_BBBP$dossiernum), format(UBBP_BBBP$goedkeurin, "%Y"), NA)
UBBP_BBBP$JrGoedkBHP <- as.integer(UBBP_BBBP$JrGoedkBHP)
unique(UBBP_BBBP$JrGoedkBHP)

UBBP_BBBP <- UBBP_BBBP %>%
  st_drop_geometry() %>%
  dplyr::select(PLOTNR,TypeBHP,JrGoedkBHP) %>%
  reshape::rename(c(PLOTNR="IDPlots")) %>% 
  filter(!is.na (JrGoedkBHP)) %>%     # als JrGoedkBHP NA, dan nog niet goedgekeurd
  as.tibble()
UBBP_BBBP


####---------------------
### Natura 2000 & reservaatstatuut----
####---------------------
Natura2000_Reservaat <-st_read(shpStrata3)   # invb2_VNR_ENR_BR_HRL_VRL.shp
str(Natura2000_Reservaat)
class(Natura2000_Reservaat)
# (c)A: op vraag van Leen datum erkenning toevoegen: NIET GEDAAN
    # WANT 
    # !! in shapefile zit géén datum voor Vlaamse NR, voor erkende NR ontbreken er datums ("DatumMB"), 
        # enkel voor bosreservaat is "erk_dat" consequent ingevuld 
    # !! datums van géén reservaat worden fout ingelezen (als 1899): cfr. UBBP_BBBP
# => enkel te gebruiken als stratum mbt VBI2 (wat nu reservaat is en wat niet)

Natura2000_Reservaat <-  Natura2000_Reservaat %>%
  st_drop_geometry() %>%
  dplyr::select(X,Y,PLOTNR, FID_Vlaams, FID_ENR_Pe, FID_bosres, FID_Habita, GEBCODE, DEELGEBI_1, FID_Vogelr, NA2000CODE,GEBNAAM) %>%
  reshape::rename(c(PLOTNR="IDPlots", GEBCODE="SBZ_H_code", DEELGEBI_1="SBZ_H_naam", NA2000CODE="SBZ_V_code", GEBNAAM= "SBZ_V_naam" )) %>%
  as.tibble()
Natura2000_Reservaat

# reservaatstatuut: ENR, VNR, BR - dynamisch ----
Natura2000_Reservaat$VNR <- ifelse(Natura2000_Reservaat$FID_Vlaams != -1, T, F)
Natura2000_Reservaat$ENR <- ifelse(Natura2000_Reservaat$FID_ENR_Pe != -1, T, F)
Natura2000_Reservaat$BR <- ifelse(Natura2000_Reservaat$FID_bosres != -1, T, F)

Natura2000_Reservaat <- Natura2000_Reservaat %>% 
  dplyr::mutate (reservaat = ifelse(VNR | ENR | BR, T, F))


# Natura 2000: HRL, VRL, SBZ - statisch ----
Natura2000_Reservaat$HRL <- ifelse(Natura2000_Reservaat$FID_Habita != -1, T, F)
Natura2000_Reservaat$VRL <- ifelse(Natura2000_Reservaat$FID_Vogelr != -1, T, F)
Natura2000_Reservaat$SBZ <- ifelse((Natura2000_Reservaat$FID_Habita != -1) | (Natura2000_Reservaat$FID_Vogelr != -1), T, F)

# ? dubbele records
length(unique(Natura2000_Reservaat$IDPlots))
nrow(Natura2000_Reservaat)
# !! niet uniek: dubbele plotID's

# dubbele records wegens o.a. 2 erkenningsdata (IDPlots = 384032)
# in detail bekeken, en zodra FID's weggeselecteerd worden, is de info uniek per plot 
    # tst2 <- Natura2000_Reservaat[duplicated(Natura2000_Reservaat$IDPlots),]
    # Natura2000_Reservaat[Natura2000_Reservaat$PLOTNR==384032, ]
    # Natura2000_Reservaat[Natura2000_Reservaat$IDPlots%in%tst2$IDPlots, ]

Natura2000_Reservaat <- Natura2000_Reservaat %>%
  select (- FID_Vlaams, -FID_ENR_Pe, -FID_bosres, -FID_Habita,-FID_Vogelr)

Natura2000_Reservaat <- unique(Natura2000_Reservaat)

Natura2000 <- Natura2000_Reservaat %>% 
  select(IDPlots,SBZ_H_code,SBZ_H_naam,SBZ_V_code,SBZ_V_naam,HRL,VRL,SBZ)

ReservaatStatuut <- Natura2000_Reservaat %>% 
  select(IDPlots,VNR,ENR,BR,reservaat)


# BOSWIJZER2012----
Boswijzer2012 <-sf:: st_read(shpStrata5)   # invb2_VNR_ENR_BR_HRL_VRL.shp
str(Boswijzer2012)
class(Boswijzer2012)

BOSW12 <- Boswijzer2012 %>%
    dplyr::select(IDPlots = PLOTNR, BOSW12 = Klasse) %>%
    data.frame() %>% 
    dplyr::select(-geometry)

# BOSWIJZER2015-v2.0----
Boswijzer2015 <-sf:: st_read(shpStrata6)   # invb2_VNR_ENR_BR_HRL_VRL.shp
str(Boswijzer2015)
class(Boswijzer2015)

BOSW15 <- Boswijzer2015 %>%
  dplyr::select(IDPlots = PLOTNR, BOSW15 = Klasse) %>%
  data.frame() %>% 
  dplyr::select(-geometry)
      
####---------------------
#### gegevens VBI2 => dynamic strata ----
####---------------------

    # query_StandDescription <-"
    # SELECT Standdescription_segments.IDPlots
    # , Standdescription_segments.ID
    # , Standdescription_segments.Landuse
    # , QSTANDDESCR_LANDUSE.Value1
    # , Standdescription_segments.StandType
    # , QSTANDTYPE.Value1
    # , Standdescription_segments.HarvestType
    # , QHARVESTTYPE.Value1
    # FROM ((Standdescription_segments INNER JOIN QSTANDDESCR_LANDUSE ON Standdescription_segments.Landuse = QSTANDDESCR_LANDUSE.ID)
    # INNER JOIN QSTANDTYPE ON Standdescription_segments.StandType = QSTANDTYPE.ID)
    # INNER JOIN QHARVESTTYPE ON Standdescription_segments.HarvestType = QHARVESTTYPE.ID
    # GROUP BY Standdescription_segments.IDPlots, Standdescription_segments.ID, Standdescription_segments.Landuse, QSTANDDESCR_LANDUSE.Value1, Standdescription_segments.StandType, QSTANDTYPE.Value1, Standdescription_segments.HarvestType, QHARVESTTYPE.Value1;
    # "

# (c) Anja: in de query worden enkel deze segmenten geselecteerd die NERGENS een NA hebben in hun bestandsbeschrijving (= 3202 records)
# (c)A: => beter om uit te splitsen én harvesttype te laten vallen (want er wordt verder toch niks mee gedaan)
# > table(plotStandDescriptionVBI2Orig$Value1.2)
# hakhout            hooghout          middelhout niet van toepassing
# 41                2918                  37                 935

query_StandDescription_standtype <-"
SELECT Standdescription_segments.IDPlots
, Standdescription_segments.ID
, Standdescription_segments.StandType
, QSTANDTYPE.Value1
FROM Standdescription_segments INNER JOIN QSTANDTYPE ON Standdescription_segments.StandType = QSTANDTYPE.ID
GROUP BY Standdescription_segments.IDPlots, Standdescription_segments.ID, Standdescription_segments.StandType, QSTANDTYPE.Value1;
"

query_StandDescription_landuse <-"
SELECT Standdescription_segments.IDPlots
, Standdescription_segments.ID
, Standdescription_segments.Landuse
, QSTANDDESCR_LANDUSE.Value1
FROM Standdescription_segments INNER JOIN QSTANDDESCR_LANDUSE ON Standdescription_segments.Landuse = QSTANDDESCR_LANDUSE.ID
GROUP BY Standdescription_segments.IDPlots, Standdescription_segments.ID, Standdescription_segments.Landuse, QSTANDDESCR_LANDUSE.Value1;
"

query_herblayerVBI2<-"
SELECT Herblayer.IDPlots
, Herblayer.Species
, Herblayer.Coverage_date1
, Herblayer.Coverage_date2
, [qVEG_HerbSpecies].Value1
FROM Herblayer
INNER JOIN [qVEG_HerbSpecies]
ON Herblayer.Species = [qVEG_HerbSpecies].ID;
"

connectieVBI2 <- odbcConnectAccess2007(dbVBI2) #dit is een accdb file

listTbl<-sqlTables(connectieVBI2)
# plotStandDescriptionVBI2Orig <- sqlQuery(connectieVBI2, query_StandDescription, stringsAsFactors = TRUE);str(plotStandDescriptionVBI2Orig)
plotStandDescriptionVBI2Orig_a <- sqlQuery(connectieVBI2, query_StandDescription_landuse, stringsAsFactors = TRUE);str(plotStandDescriptionVBI2Orig_a)
plotStandDescriptionVBI2Orig_b <- sqlQuery(connectieVBI2, query_StandDescription_standtype, stringsAsFactors = TRUE);str(plotStandDescriptionVBI2Orig_b)
herblayerVBI2Orig <- sqlQuery(connectieVBI2, query_herblayerVBI2, stringsAsFactors = TRUE);str(herblayerVBI2Orig)

odbcClose(connectieVBI2)


herblayerVBI2Orig<-reshape::rename(herblayerVBI2Orig,c(Value1="NameNl",Species="IDSpVBI2"))

plotStandDescriptionVBI2Orig <- merge(plotStandDescriptionVBI2Orig_a, plotStandDescriptionVBI2Orig_b, by=c("IDPlots","ID"), suffix = c("a", "b"),all=TRUE)
str(plotStandDescriptionVBI2Orig)



####---------------------
#### gegevens VBI1  => dynamic strata ----
####---------------------

query_StandDescription<-"SELECT tblHoofd.PLOTNR, tblHoofd.BESTTYPE1, tblBestandstypes.NAAM, tblHoofd.EIGENCAT, tblEigenaarsCategorieën.NAAM, tblHoofd.VEGTYPE, tblBostypologie.VEGTYPE, tblHoofd.DROPREDEN
FROM tblBestandstypes RIGHT JOIN (tblBostypologie RIGHT JOIN (tblHoofd LEFT JOIN tblEigenaarsCategorieën ON tblHoofd.EIGENCAT = tblEigenaarsCategorieën.EigCatID) ON tblBostypologie.VEGNR = tblHoofd.VEGTYPE) ON tblBestandstypes.BESTTYPEID = tblHoofd.BESTTYPE1
WHERE (((tblHoofd.DROPREDEN)=0));
"

connectieVBI1 <- odbcConnectAccess2007(dbVBI1) 

plotStandDescriptionVBI1Orig <- sqlQuery(connectieVBI1, query_StandDescription, stringsAsFactors = TRUE);print(str(plotStandDescriptionVBI1Orig  ))

odbcClose(connectieVBI1)

query_veglayerVBI1<-"
SELECT Opnamen.Opnamenummer
, Opnamen.[Belgisch volgnummer]
, Opnamen.Vegetatielaag
, Opnamen.Abund_Transf
FROM Opnamen
;
"

connectieVBI1 <- odbcConnectAccess2007(dbVBI1_veg) 

veglayerVBI1Orig <- sqlQuery(connectieVBI1, query_veglayerVBI1, stringsAsFactors = TRUE);print(str(veglayerVBI1Orig ))

odbcClose(connectieVBI1)

veglayerVBI1Orig<-reshape::rename(veglayerVBI1Orig,c(Opnamenummer="IDPlots","Belgisch volgnummer"="IDSpVBI1", Vegetatielaag="Layer",Abund_Transf="Coverage"))



####---------------------
#### Externe data ----
####---------------------

connectieExterneData <- odbcConnectAccess2007(dbExterneData)

speciesList<-sqlFetch(connectieExterneData, "tblspeciesListComb", stringsAsFactors = TRUE);print(str(speciesList ))

odbcClose(connectieExterneData)



###---------------------------------------------
### Databank applicatie bostypologie ----
###----------------------------------------------

q1 <- "SELECT tblReferentie.ID, tblSoort.SPECIESNAME, tblSoort.SPECIES_NR, tblReferentie.TypologieCode, tblReferentie.BostypeCode, tblReferentie.Freq, tblReferentie.KarBed, tblReferentie.IndVal
FROM tblSoort INNER JOIN tblReferentie ON tblSoort.SPECIES_NR = tblReferentie.SPECIES_NR"

q2 <- "SELECT tblSchaal.ID, tblSchaalWaarde.SchaalCode, tblSchaalWaarde.Code, tblSchaalWaarde.PctWaarde
FROM tblSchaal INNER JOIN tblSchaalWaarde ON tblSchaal.ID = tblSchaalWaarde.SchaalCode"

q3 <- "SELECT * FROM tblSoort"

dbpath <- here::here("Data/ExterneData/BostypologieApplicatieDB.mdb")
conn <- odbcConnectAccess(dbpath)
#sqlTables(conn)
Referentie <- sqlQuery(conn, q1)
Schaal <- sqlQuery(conn, q2)
Soortenlijst <- sqlQuery(conn, q3)
odbcClose(conn)

