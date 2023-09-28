library(caret)
library(dplyr)
library(ggmap)
library(ggplot2)

setwd("D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Kennzahlen pro Sommer/2022")

list.files()

data<-read.csv("Summary_2022_alle.csv", header=T, sep=";")

head(data)


height<-max(data$Latitude) - min(data$Latitude)
width<-max(data$Longitude) - min(data$Longitude)

boarders<-c(bottom=min(data$Latitude) -0.1 * height,
            top=max(data$Latitude) +0.1 * height,
            left=min(data$Longitude) -0.1 * width,
            right=max(data$Longitude) +0.1*width)

map<-get_stamenmap(boarders, zoom=13, maptype="terrain")

colorscheme=c("navyblue","blue","lightblue", "white", "gold", "red","red4")


Summer22_all<-
  ggmap(map) +
  geom_point(data=data, aes(y=Latitude, x=Longitude, fill=UHI_all),pch=21, size=4, color="black") +
  labs(title="UHI Summer 2022 all", y="", x="") +
  scale_fill_gradientn(colors=colorscheme,limits=c(-3,3), name="")+
  theme(legend.text=element_text(size=10), plot.title=element_text(size=16, hjust=0.5), axis.text=element_blank(), axis.ticks=element_blank(),
        legend.position="top", legend.key.width = unit(2, "cm"))

Summer22_night<-
  ggmap(map) +
  geom_point(data=data, aes(y=Latitude, x=Longitude, fill=UHI_night),pch=21, size=4, color="black") +
  labs(title="UHI Summer 2022 night", y="", x="") +
  scale_fill_gradientn(colors=colorscheme,limits=c(-3,3), name="")+
  theme(legend.text=element_text(size=10), plot.title=element_text(size=16, hjust=0.5), axis.text=element_blank(), axis.ticks=element_blank(),
        legend.position="top", legend.key.width = unit(2, "cm"))

Summer22_day<-
  ggmap(map) +
  geom_point(data=data, aes(y=Latitude, x=Longitude, fill=UHI_day),pch=21, size=4, color="black") +
  labs(title="UHI Summer 2022 day", y="", x="") +
  scale_fill_gradientn(colors=colorscheme,limits=c(-3,3), name="")+
  theme(legend.text=element_text(size=10), plot.title=element_text(size=16, hjust=0.5), axis.text=element_blank(), axis.ticks=element_blank(),
        legend.position="top", legend.key.width = unit(2, "cm"))


ggsave("Summer22_all.jpg", Summer22_all, path="D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Kennzahlen pro Sommer/2022", height=8.5, width=8)
ggsave("Summer22_night.jpg", Summer22_night, path="D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Kennzahlen pro Sommer/2022", height=8.5, width=8)
ggsave("Summer22_day.jpg", Summer22_day, path="D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Kennzahlen pro Sommer/2022", height=8.5, width=8)
