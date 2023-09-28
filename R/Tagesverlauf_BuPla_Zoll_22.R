library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Factsheets/2022")

data<-read.csv("Bupla_Zoll_22.csv", sep=";", header=T)

data<-mutate(data, UHI18 = Log_70_18-Log_99_18) %>%
  mutate(UHI19 = Log_70_19-Log_99_19) %>%
  mutate(UHI20 = Log_70_20-Log_99_20) %>%
  mutate(UHI21 = Log_70_21-Log_99_21) %>%
  mutate(UHI22 = Log_70_22-Log_99_22)

head(data)

data$Zeit<-strptime(data$Zeit,"%d.%m.%Y %H:%M")
data$Zeit<-as.POSIXct(data$Zeit)

data<-mutate(data, Jahr=year(Zeit)) %>%
  mutate(Monat=month(Zeit)) %>%
  mutate(Tag=day(Zeit)) %>%
  mutate(Stunde=hour(Zeit))

Uhrzeit=group_by(data, Stunde)

Mean_18<-summarise(Uhrzeit, meanUHI18=mean(UHI18, na.rm=T))
Mean_19<-summarise(Uhrzeit, meanUHI19=mean(UHI19, na.rm=T))
Mean_20<-summarise(Uhrzeit, meanUHI20=mean(UHI20, na.rm=T))
Mean_21<-summarise(Uhrzeit, meanUHI21=mean(UHI21, na.rm=T))
Mean_22<-summarise(Uhrzeit, meanUHI22=mean(UHI22, na.rm=T))

Alle<-cbind(Mean_18, Mean_19[,2], Mean_20[,2], Mean_21[,2], Mean_22[,2])

write.table(Alle, "BuPla_Zeiten_normal.csv", sep=";")

#################################################################################

#Nun ausserhalb vom R im csv die Zeiten um 12h schieben. 12 Uhr ist jetzt 1

list.files()

data2<-read.csv("BuPla_Zeiten_12hg.csv", header=T, sep=";")

head(data2)

data2$Stunde_neu<-as.character(data2$Stunde_neu)

Bupla<-
  ggplot(data2, mapping=aes(x=Stunde_neu)) +
  geom_line(aes(y = meanUHI18,color="2018"), lwd=1.2) +
  geom_line(aes(y = meanUHI19,color="2019"), lwd=1.2) +
  geom_line(aes(y = meanUHI20,color="2020"), lwd=1.2) +
  geom_line(aes(y = meanUHI21,color="2021"), lwd=1.2) +
  geom_line(aes(y = meanUHI22,color="2022"), lwd=1.5) +
  geom_hline(yintercept = 0, lty=3) +
  geom_hline(yintercept = 1, lty=3) +
  geom_hline(yintercept = 2, lty=3) +
  geom_hline(yintercept = 3, lty=3) +
  theme_bw() +
  scale_x_discrete(limits=c("12","13","14","15","16","17","18","19","20","21","22","23","0","1","2","3","4","5","6",
                            "7","8","9","10","11"))+
  scale_y_continuous(breaks=seq(0,3,0.5), minor_breaks=NULL) +
  labs(y = "°C",
       x = "Tageszeit MESZ",
       title = "Städtischer Wärmeinsel-Effekt",
       subtitle = "Temperaturdifferenz Bundesplatz - Zollikofen")+
  scale_colour_manual(values=c("grey", "blue", "gold", "red", "black"), breaks=c("2018", "2019", "2020", "2021", "2022"), name="") +
  theme(legend.position=c(0.50,0.05), legend.direction = "horizontal", legend.background=element_rect(fill="white", color="black"))

ggsave("Bupla.jpg", Bupla, path="D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Factsheets/2022", width=8, height=7)
