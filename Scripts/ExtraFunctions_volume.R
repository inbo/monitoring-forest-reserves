
# Functies om volumes te berekenen
# enkel snag wordt anders berekend dan in FM ...


# SNAG ----

# my.CalcVolSnag <-function(data_dendro, varNameDiameter="dbh_mm",varNameHeight="calc_height_m"){
#   
#   data_dendro_snag <- data_dendro %>% 
#     filter(intact_snag == 10) %>% 
#     select(period, plot_id, tree_measure_id, species, alive_dead, intact_snag, 
#            dbh_mm, height_m, calc_height_m)
#     mutate(i = )
#     
#     
#     
# }
# 
# 
# 
# (Eq. 1)
# where	dh – stem diameter at height h
# H – total height 
# d1.3 – diameter at breast height
# i, p, q – model parameters
# 
# 
# 
# 


# INLEZEN TARIEVEN ----------
# zie dbExterneData

query_tarieven2ing<-"
SELECT
tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value1
, tblTarieven_2ing.Tarief
, tblTarieven_2ing.groepNaam
, tblTarieven_2ing.a
, tblTarieven_2ing.b
, tblTarieven_2ing.c
, tblTarieven_2ing.d
, tblTarieven_2ing.e
, tblTarieven_2ing.f
, tblTarieven_2ing.g
, tblTarieven_2ing.Formule_type
FROM tblTariefgroepBoomsoort
INNER JOIN tblTarieven_2ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_2ing.groepID
;"

query_tarieven1ing<-"
SELECT tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value1
, tblTariefgroepBoomsoort.TariefID
, tblTarieven_1ing.groepNaam
, tblTarieven_1ing.a
, tblTarieven_1ing.b
, tblTarieven_1ing.c
, tblTarieven_1ing.d
, tblTarieven_1ing.Tarief
FROM tblTariefgroepBoomsoort
LEFT JOIN tblTarieven_1ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_1ing.groepID
;"

query_tarieven1ingKroon<-"
SELECT tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value1
, tblTariefgroepBoomsoort.TariefID
, tblTarieven_1ingKroon.groepNaam
, tblTarieven_1ingKroon.a
, tblTarieven_1ingKroon.b
, tblTarieven_1ingKroon.c
, tblTarieven_1ingKroon.d
, tblTarieven_1ingKroon.Tarief
FROM tblTariefgroepBoomsoort
LEFT JOIN tblTarieven_1ingKroon ON tblTariefgroepBoomsoort.TariefID = tblTarieven_1ingKroon.groepID
;"


connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file

cat("\n\n tarieven 2 ing\n--------------------------------\n")
tarieven2ingOrig <- sqlQuery(connectieExterneData, query_tarieven2ing, stringsAsFactors = TRUE);print(str(tarieven2ingOrig ))

cat("\n\n tarieven 1 ing\n--------------------------------\n")
tarieven1ingOrig <- sqlQuery(connectieExterneData, query_tarieven1ing, stringsAsFactors = TRUE);print(str(tarieven1ingOrig ))

cat("\n\n tarieven 1 ing kroonhout\n--------------------------------\n")
tarieven1ingKroonOrig <- sqlQuery(connectieExterneData, query_tarieven1ingKroon, stringsAsFactors = TRUE);print(str(tarieven1ingKroonOrig ))

#
# boomEigenschapOrig<-sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)

odbcClose(connectieExterneData)

tarieven1ing<-plyr::rename(tarieven1ingOrig,c(ID="species",Value1="NameNl"))
tarieven2ing<-plyr::rename(tarieven2ingOrig,c(ID="species",Value1="NameNl"))
tarieven1ingKroon <- plyr::rename (tarieven1ingKroonOrig,c(ID="species",Value1="NameNl"))



####----------------------------------------------------------------
#### Functie die grondvlak berekent en volume waarbij tarieven en aantal ingangen worden gespecifieerd. De berekeningen gebeuren op basis van een data.frame met de velden 'Perimeter_cm' en 'Height_m' (in geval van 2 ingangen)
####----------------------------------------------------------------

# FUNCTIE
my.CalcVolBA<-function(treeMeasurements,tarieven,nIngang,varNameDiameter="dbh_mm",varNameHeight="calc_height_m"){

  tarieven<-tarieven[,names(tarieven) != "NameNl"]
  trees<-merge(treeMeasurements,tarieven,by="species",all.x=TRUE)

  #Hulpvariabelen
  perimeter <- pi*trees[,varNameDiameter]/10
  radius_m <- trees[,varNameDiameter]/2000

  #Hulpvariabelen bewaard in dataset
  trees$basal_area_m2 <- pi * radius_m^2
  trees$D <- trees[,varNameDiameter]/10

  if (nIngang==2){

    trees$vol_stem_m3 <-
      ifelse( trees$Formule_type == 1,
              yes =
                trees$a + trees$b * perimeter +
                trees$c *(perimeter^2)+ trees$d *(perimeter^3) +
                trees$e*trees[,varNameHeight] + trees$f*trees[,varNameHeight]* perimeter +
                trees$g*trees[,varNameHeight]*(perimeter^2),
              no =
                1/1000 *
                #spil
                (exp(1.10597 * log(trees[,varNameHeight]) + 1.78865 * log(trees$D) - 3.07192) -
                   #Verlies
                   exp(-4.608923 * log(trees$D) + 3.005989 * log(trees[,varNameHeight]) -
                         1.3209 * log(trees[,varNameHeight])*log(trees[,varNameHeight])+ 1.605266 * log(trees$D) * log(trees[,varNameHeight]) + 5.410272))
      )


  } else if (nIngang==1){
    trees$vol_stem_m3<- trees$a + trees$b * perimeter + trees$c *(perimeter^2)+ trees$d *(perimeter^3)
  } else {
    trees$vol_stem_m3 = NaN
  }

  trees<-trees[,!names(trees) %in% c("a","b","c","d","e","f","g","Formule_type","Tarief","groepNaam", "TariefID", "D")]
  trees$vol_stem_m3<-pmax(0,trees$vol_stem_m3)
  trees
}




####----------------------------------------------------------------
#### Functie die volume van het (zwaar) kroonhout berekent, waarbij tarief wordt gespecifieerd.
# De berekeningen gebeuren op basis van een data.frame met het veld 'Perimeter_cm'
####----------------------------------------------------------------

my.CalcVolBranches<-function(treeMeasurements,tarieven,varNameDiameter="dbh_mm",varNameHeight="calc_height_m"){

  tarieven<-tarieven[,names(tarieven) != "NameNl"]
  trees<-merge(treeMeasurements,tarieven,by="species",all.x=TRUE)

  #Hulpvariabelen
  perimeter <- pi*trees[,varNameDiameter]/10
  radius_m <- trees[,varNameDiameter]/2000

  #Hulpvariabelen bewaard in dataset
  trees$vol_crown_m3<- trees$a + trees$b * perimeter + trees$c *(perimeter^2)+ trees$d *(perimeter^3)

  trees<-trees[,!names(trees) %in% c("a","b","c","d","Tarief","groepNaam", "TariefID", "D")]
  trees$vol_crown_m3<-pmax(0,trees$vol_crown_m3)
  trees
}



