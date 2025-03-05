# LIS en FAS metingen samenvoegen
# In de paper weet ik bvan elke boom of ze met volopname of met LIS gemeten 'zou' worden afhankelijk van elke treshold-diameter. Daarom kan ik dus beiden gewoon samentellen (er zitten geen dubbeltellingen in)
#In de huidige bosreservaten data worden alle bomen op de LIS gemeten en de bomen <30 cm ook in de FAS. Als je beiden wil combineren dan zijn er wel dubbeltellingen. Je kan nu op 3 manieren een volume berekenen: 
#* enkel LIS: alle diameters tot 10 cm maar minder nauwkeurig op plotniveau
#* enkel FAS: diameters vanaf 30 cm en nauwkeurig op plotniveau
#* combinatie van beiden maar dan is compensatie nodig. Voor de laatste ga je als volgt te werk: 
# 1 verwijder alle metingen boven de 30 cm uit de LIS dataset (deze bomen zijn zeker meegenomen in FAS)
# 2 Bereken de theoretische LIS diameter van alle bomen in de FAS data zoals hieronder weergegeven. Waarden boven de 30 cm worden verwijdert (deze zijn geen dubbeltellingen meer gezien (1). De resterende diameters worden als negetieve waarden aan de LIS data toegevoegd. Aan de hand van DeVries formule worden de diameters omgezet naar een volume per plot. Negatieve diameters compenseren op die manier voor gemeten LIS diameters van bomen die ook in FAS data zitten. 

# Bereken de theoretische LIS diameter van alle bomen in de FAS data
# LIS met 3 lijnen: één naar Noorden van 3 tot 18m, 2 andere onder hoek van 120 graden

plot$rico <- (plot$yd-plot$yD) / (plot$xd-plot$xD)# rico van lijnstuk dood hout
plot$b <- plot$yD-plot$xD*plot$rico # b in de formule y = rico*x + b
  
  ricol2 <- -sinpi(30/180)/cospi(30/180) # rico van lijn 2
  plot$yl2 <- plot$b /(1-(plot$rico/ricol2))
  plot$xl2 <- plot$yl2/ricol2
  plot$afstl2 <- sqrt(plot$xl2^2+plot$yl2^2) 
  plot$diaml2 <- plot$D + ((plot$xl2-plot$xD) * (plot$d-plot$D)/(plot$xd-plot$xD))
  plot$diaml2 <- ifelse(plot$afstl2<plot$rA4 & plot$afstl2>3 & plot$yl2<0 & plot$d<=plot$diaml2 & plot$diaml2<=plot$D, round(plot$diaml2,0), 0) # indien aan alle voorwaarden voldaan dan snijdt de boom transectlijn 2, anders wordt de waarde op 0 gezet
  
# idem voor lijn 3  
  ricol3 <- -ricol2
  plot$yl3 <- plot$b /(1-(plot$rico/ricol3))
  plot$xl3 <- plot$yl3/ricol3
  plot$afstl3 <- sqrt(plot$xl3^2+plot$yl3^2) 
  plot$diaml3 <- plot$D + ((plot$xl3-plot$xD) * (plot$d-plot$D)/(plot$xd-plot$xD))
  plot$diaml3 <- ifelse( plot$afstl3<plot$rA4 & plot$afstl3>3 & plot$yl3<0 & plot$d<=plot$diaml3 & plot$diaml3<=plot$D, round(plot$diaml3,0), 0)
  
  # idem voor lijn 1 op de y-as 
  plot$ykruis <- -(plot$rico * plot$xD) + plot$yD
  plot$diamy <- plot$D - (plot$xD * (plot$d-plot$D)/(plot$xd-plot$xD))
  plot$diamy <- ifelse( plot$ykruis<plot$rA4 & plot$ykruis>3 & plot$d<=plot$diamy & plot$diamy<=plot$D, round(plot$diamy,0), 0)

  #boven de 30 cm worden verwijdert (deze zijn geen dubbeltellingen meer gezien (1)
  plot$diamy <- ifelse(plot$diamy>=300,0,plot$diamy)
  plot$diaml2 <- ifelse(plot$diaml2>=300,0,plot$diamy)
  plot$diaml3 <- ifelse(plot$diaml3>=300,0,plot$diamy)
  
plot$DVfactor45<- pi^2/(45*8) * (plot$rA4*plot$rA4*pi)/10000 #vast deel van De Vries (1986) formule om volume met line intersecten te berekenen voor 45 m transetline (is op zich ook afhankelijk van de straal!)

plotliney <- plot
plotline2 <- plot
plotline3 <- plot

plotliney$d2 <- -(plot$diamy/10)^2  # (c)A: diameter in cm en in t kwadraat
plotline2$d2 <- -(plot$diaml2/10)^2
plotline3$d2 <- -(plot$diaml3/10)^2
plotline <- rbind(plotliney, plotline2, plotline3)
Compensatievol <-plotline %>%
  group_by(IDPlots) %>%
  summarise(vol=sum(d2)*first(DVfactor45)) 

# Voltotal = VOL-FAS + VOL-LIS(d>=30) + Compensatievol$vol
# In bovenstaand script zit nog een probleem dat ik nog niet heb opgelost! Als een boom perfect N-Z gelegen is dan is de rico oneindig (/0) maar geeft r NA. Tot nu toe zijn er 3 bomen in de violledige dataset waar dit het geval is.  