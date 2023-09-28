library(caret)
library(dplyr)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(raster)

setwd("D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Factsheets/2022")

list.files()

data<-read.csv("TNSD_22.csv", header=T, sep=";")

##Aufarbeitung der Hitergrunddaten

setwd("U:/GIS-data/AVR-Daten/Daten_mutiert/Grundflächen/Raster_Daten")

list.files()

BUL<-raster("Bul_Raster.tif")
GA<-raster("GA_Raster.tif")
SE<-raster("SE_Raster.tif")
FO<-raster("FO_Raster.tif")
WA<-raster("WA_Raster.tif")
AC<-raster("AC_Raster.tif")

BUL<-aggregate(BUL, fact=2)
GA<-aggregate(GA, fact=2)
SE<-aggregate(SE, fact=2)
FO<-aggregate(FO, fact=2)
WA<-aggregate(WA, fact=2)
AC<-aggregate(AC, fact=2)

BUL<- as.data.frame(BUL, xy = TRUE)
BUL<-filter(BUL, Bul_Raster==1)

GA<- as.data.frame(GA, xy = TRUE)
GA<-filter(GA, GA_Raster==1)

SE<- as.data.frame(SE, xy = TRUE)
SE<-filter(SE, SE_Raster==1)

AC<- as.data.frame(AC, xy = TRUE)
AC<-filter(AC, AC_Raster==1)

FO<- as.data.frame(FO, xy = TRUE)
FO<-filter(FO, GA_Raster==1)

WA<- as.data.frame(WA, xy = TRUE)
WA<-filter(WA, SE_Raster==1)

##Farbenschemas

colorscheme2<-c("white","yellow", "orange", "red","darkred", "black")
colorscheme<-c("darkgreen","green","beige", "grey", "black")
colorscheme3<-c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026")


#Plotting

TN_22<-
  ggplot()+
  geom_raster(GA, mapping=aes(x=x, y=y), fill="darkolivegreen1", alpha=0.5) +
  geom_raster(AC, mapping=aes(x=x, y=y), fill="darkolivegreen1", alpha=0.5) +
  geom_raster(WA, mapping=aes(x=x, y=y), fill="lightblue", alpha=0.5) +
  geom_raster(BUL, mapping=aes(x=x, y=y), fill="grey", alpha=0.5) +
  geom_raster(SE, mapping=aes(x=x, y=y), fill="grey", alpha=0.5) +
  geom_raster(FO, mapping=aes(x=x, y=y), fill="darkgreen", alpha=0.5) +
  geom_point(data=data, 
             aes(x=CH_X, y=CH_Y, fill=TN), 
             colour="black", size=8, pch=21) + 
  labs(title="Berner Tropennächte 2022")+
  scale_fill_stepsn(limits=c(-1,12), breaks=c(0,2,4,6,8,10,12), 
                    labels=c("0","1 - 2","3 - 4","5 - 6","7 - 8","9 - 10","> 10"),
                    colors = colorscheme2, name="Anzahl", guide="legend")+
  theme_minimal()+
  theme(panel.grid =element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0.1,0.185),
        legend.background=element_rect(fill="white", color="black"),
        legend.key.size=(unit(0.8, "cm")),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust=0.5, margin=margin(b=-20,t=5) ,size=18))

TN_22

ggsave("TN_22.jpg", TN_22, path="D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Factsheets/2022", width=10, height=9.5375)


HD_22<-
  ggplot()+
  geom_raster(GA, mapping=aes(x=x, y=y), fill="darkolivegreen1", alpha=0.5) +
  geom_raster(AC, mapping=aes(x=x, y=y), fill="darkolivegreen1", alpha=0.5) +
  geom_raster(WA, mapping=aes(x=x, y=y), fill="lightblue", alpha=0.5) +
  geom_raster(BUL, mapping=aes(x=x, y=y), fill="grey", alpha=0.5) +
  geom_raster(SE, mapping=aes(x=x, y=y), fill="grey", alpha=0.5) +
  geom_raster(FO, mapping=aes(x=x, y=y), fill="darkgreen", alpha=0.5) +
  geom_point(data=data, 
             aes(x=CH_X, y=CH_Y, fill=HD), 
             colour="black", size=8, pch=21) + 
  labs(title="Berner Hitzetage 2022")+
  scale_fill_stepsn(limits=c(0,51), breaks=c(10,20,30,40,50), 
                    labels=c("< 10","11 - 20","21 - 30","31 - 40","> 40"),
                    colors = colorscheme, name="Anzahl", guide="legend")+
  theme_minimal()+
  theme(panel.grid =element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0.105,0.15),
        legend.background=element_rect(fill="white", color="black"),
        legend.key.size=(unit(0.8, "cm")),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust=0.5, margin=margin(b=-20,t=5) ,size=18))

HD_22

ggsave("HD_22.jpg", HD_22, path="D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Factsheets/2022", width=10, height=9.5375)
