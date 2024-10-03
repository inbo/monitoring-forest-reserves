## vergelijking LIS en FAS

library(lme4)
library(lmerTest)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(vctrs)


# 1 data
## data inlezen
setwd("G:/Mijn Drive/bosreservaten/line intersect")

data1 <- read.csv('DW_1eSet_XYv2.csv', header=TRUE, sep = ';', dec = ',')
data2 <- read.csv('DW_2eSet_XYv2.csv', header=TRUE, sep = ';', dec = ',')
data3 <- read.csv('DW_3eSet_XYv2.csv', header=TRUE, sep = ';', dec = ',')
data2link <- read.csv('IDlink_2eSet.csv', header=TRUE, sep = ';', dec = ',')
data3link <- read.csv('IDlink_3eSet.csv', header=TRUE, sep = ';', dec = ',')
overzichtplots <- read.csv('overzichtplots.csv', header=TRUE, sep = ';', dec = ',')

data1$Period <- 1
data2$Period <- 2
data3$Period <- 3
data1$IDlink <- 0
data2 <- left_join(data2, data2link, by=c("IDPlots","IDDeadwood"))
data3 <- left_join(data3, data3link, by=c("IDPlots","IDDeadwood"))

data <- rbind(data1, data2, data3)
data$IDPlotsP <- paste(data$IDPlots, data$Period, sep='-')
data$IDDeadwoodP <- paste(data$IDDeadwood, data$Period, sep='-')

## data organiseren zodat er één record per log is
datamet <- dplyr::select(data, IDPlotsP, IDDeadwoodP, IDDeadwood, IDlink, Period, ID, X_m, Y_m, Z_m, Diam_mm) %>%
  group_by(IDPlotsP, IDDeadwoodP) %>%
  summarise(d1=first(Diam_mm), d2=last(Diam_mm), x1=first(X_m), x2=last(X_m), y1=first(Y_m), y2=last(Y_m), z1=first(Z_m), z2=last(Z_m), IDDeadwood=first(IDDeadwood), IDlink=first(IDlink), Period=first(IDDeadwood)) %>%
  ungroup() 

## basis (D) en top (d) toewijzen
datamet$d <-ifelse(datamet$d1<datamet$d2, datamet$d1, datamet$d2)
datamet$xd <-ifelse(datamet$d1<datamet$d2, datamet$x1, datamet$x2)
datamet$yd <-ifelse(datamet$d1<datamet$d2, datamet$y1, datamet$y2)
datamet$zd <-ifelse(datamet$d1<datamet$d2, datamet$z1, datamet$z2)
datamet$D <-ifelse(datamet$d1<datamet$d2, datamet$d2, datamet$d1)
datamet$xD <-ifelse(datamet$d1<datamet$d2, datamet$x2, datamet$x1)
datamet$yD <-ifelse(datamet$d1<datamet$d2, datamet$y2, datamet$y1)
datamet$zD <-ifelse(datamet$d1<datamet$d2, datamet$z2, datamet$z1)  

datamet <- dplyr::select(datamet, IDPlotsP, IDDeadwoodP, IDDeadwood, IDlink, Period, d, D, xd, yd, zd, xD, yD, zD)

datarest <- dplyr::select(data[data$ID==1,], IDPlots, IDPlotsP, IDDeadwoodP, IDDeadwood, Length_m, Volume_m3, DecayStage, Genus, Wood, ForestReserve, CalcLength_m, CalcVolume_m3, Period)
datarest$IDFull <- paste(datarest$IDPlotsP, datarest$IDDeadwoodP) 
datamet$IDFull <- paste(datamet$IDPlotsP, datamet$IDDeadwoodP) 
datamet <- dplyr::select(datamet, IDFull, d, D, xd, yd, zd, xD, yD, zD, IDlink)

data <- left_join(datamet, datarest, by='IDFull')


##### afknippen op straal
straal <- select(overzichtplots, IDPlots, rA4)
data <- left_join(data, straal, by= "IDPlots")

# stukje oud en nieuw script aan elkaar plakken
data$IDPlots <- data$IDPlotsP
data$IDDeadwood <- data$IDDeadwoodP

plot <- data

# 1.1 afknippen op straal

plot$INd <- sqrt(plot$xd^2+plot$yd^2)    #afstand tot middelpunt
plot$IND <- sqrt(plot$xD^2+plot$yD^2)
plot$rico <- (plot$yd-plot$yD) / (plot$xd-plot$xD)# rico van lijnstuk dood hout
plot$b <- plot$yD-plot$xD*plot$rico # b in de formule y = rico*x + b
plot$Discr <- (2*plot$rico*plot$b)^2 - 4*(1+plot$rico^2)*(-plot$rA4^2+plot$b^2)  # discriminant voor snijpunt cirkel en dood hout

plot$xcirkel1 <- (-2*plot$rico*plot$b - sqrt(abs(plot$Discr)))/(2*(1+plot$rico^2)) #x waarde van kruising 1 (uitgaande dat Discr positief is)
plot$xcirkel2 <- (-2*plot$rico*plot$b + sqrt(abs(plot$Discr)))/(2*(1+plot$rico^2)) #kruising 2 (uitgaande dat Discr positief is)

#4 situaties,element ligt erin, element ligt er deels in en deels uit en zowel d als D liggen er uit. In het laatste geval kan een deel van het element nog steeds in de cirkel liggen als Discr <0 (=rechte snijdt de cirkel op 2 plaatsen)
plotin <- plot[plot$INd<=plot$rA4 & plot$IND<=plot$rA4,] 
plotdout <- plot[plot$INd>plot$rA4 & plot$IND<=plot$rA4,]
plotDout <- plot[plot$IND>plot$rA4 & plot$INd<=plot$rA4,]
plotout <- plot[plot$IND>plot$rA4 & plot$INd>plot$rA4 & plot$Discr>0,]

# per situatie de nieuwe diameter en x, y berekenen
plotdout$xdnew <- ifelse((plotdout$xcirkel1<plotdout$xd & plotdout$xcirkel1>plotdout$xD) | (plotdout$xcirkel1>plotdout$xd & plotdout$xcirkel1<plotdout$xD), plotdout$xcirkel1, plotdout$xcirkel2)
plotdout$yd <- plotdout$rico*plotdout$xdnew + plotdout$b
plotdout$d <- plotdout$D + (plotdout$xdnew - plotdout$xD) * (plotdout$d-plotdout$D)/(plotdout$xd-plotdout$xD)
plotdout$zd <- plotdout$zD + (plotdout$xdnew - plotdout$xD) * (plotdout$zd-plotdout$zD)/(plotdout$xd-plotdout$xD)
plotdout$xd <- plotdout$xdnew

plotDout$xDnew <- ifelse((plotDout$xcirkel1<plotDout$xd & plotDout$xcirkel1>plotDout$xD) | (plotDout$xcirkel1>plotDout$xd & plotDout$xcirkel1<plotDout$xD), plotDout$xcirkel1, plotDout$xcirkel2)
plotDout$yD <- plotDout$rico*plotDout$xDnew + plotDout$b
plotDout$D <- plotDout$D + (plotDout$xDnew - plotDout$xD) * (plotDout$d-plotDout$D)/(plotDout$xd-plotDout$xD)
plotDout$zD <- plotDout$zD + (plotDout$xDnew - plotDout$xD) * (plotDout$zd-plotDout$zD)/(plotDout$xd-plotDout$xD)
plotDout$xD <- plotDout$xDnew


plotout$d1 <- plotout$D + (plotout$xcirkel1 - plotout$xD) * (plotout$d-plotout$D)/(plotout$xd-plotout$xD)
plotout$d2 <- plotout$D + (plotout$xcirkel2 - plotout$xD) * (plotout$d-plotout$D)/(plotout$xd-plotout$xD)
plotout$d1 <- ifelse (plotout$d1<plotout$D & plotout$d1>plotout$d, plotout$d1, 0) #snijdt het lijnstuk werkelijk de cirkel of enkel de rechte voorbij het lijnsuk
plotout$d2 <- ifelse (plotout$d1<plotout$D & plotout$d1>plotout$d, plotout$d2, 0)

plotout$z1 <- plotout$zD + (plotout$xcirkel1 - plotout$xD) * (plotout$zd-plotout$zD)/(plotout$xd-plotout$xD)
plotout$z2 <- plotout$zD + (plotout$xcirkel2 - plotout$xD) * (plotout$zd-plotout$zD)/(plotout$xd-plotout$xD)
plotout$z1 <- ifelse (plotout$z1<plotout$zD & plotout$z1>plotout$zd, plotout$z1, 0) #snijdt het lijnstuk werkelijk de cirkel of enkel de rechte voorbij het lijnsuk
plotout$z2 <- ifelse (plotout$z1<plotout$zD & plotout$z1>plotout$zd, plotout$z2, 0)

plotout$d <- ifelse (plotout$d1 < plotout$d2, plotout$d1, plotout$d2)
plotout$zd <- ifelse (plotout$d1 < plotout$d2, plotout$z1, plotout$z2)
plotout$xd <- ifelse (plotout$d1 < plotout$d2, plotout$xcirkel1, plotout$xcirkel2)
plotout$yd <- plotout$rico*plotout$xd + plotout$b

plotout$D <- ifelse (plotout$d1 > plotout$d2, plotout$d1, plotout$d2)
plotout$zD <- ifelse (plotout$d1 > plotout$d2, plotout$z1, plotout$z2)
plotout$xD <- ifelse (plotout$d1 > plotout$d2, plotout$xcirkel1, plotout$xcirkel2)
plotout$yD <- plotout$rico*plotout$xD + plotout$b


# de 4 datasets/situaties terug samenbrengen
plotDout <- select(plotDout,-xDnew)
plotdout <- select(plotdout,-xdnew)
plotout <- select(plotout,-c(d1,d2,z1,z2))

plot <- rbind(plotin, plotdout, plotDout, plotout)
plot$L <- sqrt((plot$xd-plot$xD)^2+(plot$yd-plot$yD)^2+(plot$zd-plot$zD)^2) #nieuwe lengte bepalen

