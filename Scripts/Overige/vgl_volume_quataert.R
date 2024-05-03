rm(list=ls())

source(here::here("scripts/Setup.R"))

# test volume beuk en eik obv VBI en obv R


# Creating the dataframe
df0 <- data.frame(
  species = c(7, 87, 64, 6),
  height_m = rep(25, 4),  # 25 meters for all species
  dbh_mm = rep(250, 4)     # 250 mm for all species
)

# dbExterneData --> dbh_150
con <- odbcConnectAccess2007(dbExterneData)
tblCoefOmzetOmtrek <- sqlFetch(con, "tblCoefOmzetOmtrek", stringsAsFactors = FALSE)
odbcClose(con)

df <- df0 %>% 
  left_join(tblCoefOmzetOmtrek, by = c("species" = "IDTreeSp")) %>% 
  mutate(perimeter = pi * dbh_mm / 10
         , perimeter_150 = (perimeter - A) / B 
  ) %>% 
  mutate(dbh_mm_150 = 10*perimeter_150/pi)
df


# 1) oude  obv dbh_130

df_130 <- my.CalcVolBA(df, tarieven2ing, nIngang = 2, 
                                      varNameDiameter = "dbh_mm",
                                      varNameHeight = "height_m")
df_130 <- my.CalcVolBranches(df_130, tarieven1ingKroon, 
                                            varNameDiameter = "dbh_mm")

df_130 <- df_130 %>% mutate(calc_volume_m3 = vol_stem_m3 + vol_crown_m3)

df_130


# 2) obv dbh_150
df_150 <- my.CalcVolBA(df, tarieven2ing, nIngang = 2, 
                    varNameDiameter = "dbh_mm_150",
                    varNameHeight = "height_m")
df_150 <- my.CalcVolBranches(df_150, tarieven1ingKroon, 
                          varNameDiameter = "dbh_mm")

df_150 <- df_150 %>% mutate(calc_volume_m3 = vol_stem_m3 + vol_crown_m3)


df_150


# 2) cfr package

df <- df0 %>% 
  mutate(calc_height_m = height_m
         , ind_sht_cop = 10
         , intact_snag = 11)
  
df_package <- calc_stem_volume(df) %>% 
  mutate(calc_volume_m3 = vol_bole_m3 + vol_crown_m3)

df_package
df_130
df_150

# vbi en df_package geven zelfde resultaat
én vbi functie obv C130 geeft hogere waardes, zoals je zou verwachten

WAAROM geeft mijn oude functie van BR dan andere waardes dan ????
  
  

# voor soort (sp 6) zonder veranderd tarief kom ik zelfde uit 
# maar bij andere heb ik lager en package hoger volume dan voorheen !!

# cfr VBI : 1 = 87 eik, 4 = 7 beuk; 12 = 6 berk, 5 = 64 kastanje 
# IDTreeSp height_m dbh_mm Perimeter_cm BasalArea_m2  D     C150    Volume VolumeKroon calc_volume_m3
#      1       25    250     78.53982   0.04908739 25 77.65171 0.5549715  0.03258440      0.5875559
#      4       25    250     78.53982   0.04908739 25 77.72590 0.5121239  0.03444113      0.5465650
#     5       25    250     78.53982   0.04908739 25 77.72590 0.5121239  0.03444113      0.5465650
#    12       25    250     78.53982   0.04908739 25 77.04892 0.5249320  0.02033299      0.5452650





# 
# trees$a + trees$b * perimeter +
#   trees$c *(perimeter^2)+ trees$d *(perimeter^3) +
#   trees$e*trees[,varNameHeight] + trees$f*trees[,varNameHeight]* perimeter +
#   trees$g*trees[,varNameHeight]*(perimeter^2),
# 
# yes =
#   .data$a + .data$b * .data$perimeter +
#   .data$c * .data$perimeter ^ 2 +
#   .data$d * .data$perimeter ^ 3 +
#   .data$e * .data$calc_height_m +
#   .data$f * .data$calc_height_m * .data$perimeter +
#   .data$g * .data$calc_height_m * .data$perimeter ^ 2,
# 
# 
# 	a	b	c	d	e	f	g	

0.1645 -0.005612*77.65171 + 0.0000291*77.65171^2 + 0*77.65171^3	-0.00725*25 +	0.00025*25*77.65171 + 0.0000023*25*77.65171^2
#  voor eik zou het [1] 0.5549714 moeten zijn 

# vbi 0.5549715  
# dus ergens een fout in mijn eigen code !!