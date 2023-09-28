library(tidyverse)
library(dplyr)
library(lubridate)

setwd("D:/PhD_Moritz/Stadt Bern_Austausch/Berechnungen/Kennzahlen pro Sommer/2022")

list.files()

data<-read.csv("Rawdata_2022_JJA_7hg.csv", header=T, sep=";")

data$Zeit<-strptime(data$Zeit, format="%d.%m.%Y %H:%M")
data$Zeit<-as.POSIXct(data$Zeit)


data<-mutate(data, Monat=month(Zeit)) %>%
  mutate(Tag=day(Zeit)) %>%
  mutate(Stunde=hour(Zeit))

##F?r Tropenn?chte, Zeit (Nacht) von 9 bis 7 beachten

data_night<-filter(data, Stunde>=14)
data_day<-filter(data, Stunde<14)

Night_group<-group_by(data_night, Monat, Tag)
Day_group<-group_by(data_day, Monat, Tag)
All_group<-group_by(data, Monat, Tag)

Tmin_night<-summarise(Night_group, Log1 = min(Log_1, na.rm=T),Log2 = min(Log_2, na.rm=T),Log7 = min(Log_7, na.rm=T),
                      Log8 = min(Log_8, na.rm=T),Log9 = min(Log_9, na.rm=T),Log10 = min(Log_10, na.rm=T),Log13 = min(Log_13, na.rm=T),
                      Log14 = min(Log_14, na.rm=T),Log16 = min(Log_16, na.rm=T),Log17 = min(Log_17, na.rm=T),
                      Log19 = min(Log_19, na.rm=T),Log21 = min(Log_21, na.rm=T),Log22 = min(Log_22, na.rm=T),
                      Log23 = min(Log_23, na.rm=T),Log25 = min(Log_25, na.rm=T),Log26 = min(Log_26, na.rm=T),
                      Log28 = min(Log_28, na.rm=T),Log30 = min(Log_30, na.rm=T),Log31 = min(Log_31, na.rm=T),
                      Log32 = min(Log_32, na.rm=T),Log33 = min(Log_33, na.rm=T),Log34 = min(Log_34, na.rm=T),
                      Log36 = min(Log_36, na.rm=T),Log37 = min(Log_37, na.rm=T),Log38 = min(Log_38, na.rm=T),
                      Log39 = min(Log_39, na.rm=T),Log42 = min(Log_42, na.rm=T),
                      Log45 = min(Log_45, na.rm=T),Log48 = min(Log_48, na.rm=T),Log51 = min(Log_51, na.rm=T),
                      Log52 = min(Log_52, na.rm=T),Log53 = min(Log_53, na.rm=T),Log55 = min(Log_55, na.rm=T),
                      Log57 = min(Log_57, na.rm=T),Log58 = min(Log_58, na.rm=T),Log59 = min(Log_59, na.rm=T),
                      Log61 = min(Log_61, na.rm=T),Log62 = min(Log_62, na.rm=T),Log65 = min(Log_65, na.rm=T),
                      Log68 = min(Log_68, na.rm=T),Log69 = min(Log_69, na.rm=T),Log70 = min(Log_70, na.rm=T),
                      Log71 = min(Log_71, na.rm=T),Log73 = min(Log_73, na.rm=T),Log76 = min(Log_76, na.rm=T),
                      Log78 = min(Log_78, na.rm=T),Log79 = min(Log_79, na.rm=T),Log80 = min(Log_80, na.rm=T),
                      Log82 = min(Log_82, na.rm=T),Log83 = min(Log_83, na.rm=T),Log86 = min(Log_86, na.rm=T),
                      Log87 = min(Log_87, na.rm=T),Log101 = min(Log_101, na.rm=T),Log102 = min(Log_102, na.rm=T),
                      Log110 = min(Log_110, na.rm=T), Log111 = min(Log_111, na.rm=T), Log112 = min(Log_112, na.rm=T),
                      Log113 = min(Log_113, na.rm=T), Log114 = min(Log_114, na.rm=T), Log115 = min(Log_115, na.rm=T),
                      Log116 = min(Log_116, na.rm=T), Log117 = min(Log_117, na.rm=T), Log118 = min(Log_118, na.rm=T),
                      Log119 = min(Log_119, na.rm=T), Log122 = min(Log_122, na.rm=T), Log124 = min(Log_124, na.rm=T),
                      Log125 = min(Log_125, na.rm=T), Log131 = min(Log_131, na.rm=T), Log132 = min(Log_132, na.rm=T),
                      Log133 = min(Log_133, na.rm=T), Log141 = min(Log_141, na.rm=T), Log142 = min(Log_142, na.rm=T),
                      Log143 = min(Log_143, na.rm=T), Log151 = min(Log_151, na.rm=T), Log152 = min(Log_152, na.rm=T),
                      Log125 = min(Log_125, na.rm=T), Log131 = min(Log_131, na.rm=T), Log132 = min(Log_132, na.rm=T),
                      Log153 = min(Log_153, na.rm=T), Log154 = min(Log_154, na.rm=T), Log155 = min(Log_155, na.rm=T),
                      Log99 = min(Log_99, na.rm=T),Log98 = min(Log_98, na.rm=T))

warnings()

##Average Tmin

Tmin_night$Log10[which(is.infinite(Tmin_night$Log10))] <- NA
Tmin_night$Log111[which(is.infinite(Tmin_night$Log111))] <- NA
Tmin_night$Log117[which(is.infinite(Tmin_night$Log117))] <- NA
Tmin_night$Log118[which(is.infinite(Tmin_night$Log118))] <- NA
Tmin_night$Log119[which(is.infinite(Tmin_night$Log119))] <- NA
Tmin_night$Log53[which(is.infinite(Tmin_night$Log53))] <- NA



Tmin_avg<-colMeans(Tmin_night[,3:82], na.rm=T)


##Tropical Nights

v<-c(3:82)

TN<-numeric()

for(i in v) {
  my_out<-length(which(Tmin_night[,i]>=20 & Tmin_night[,i]<=30))
  TN<-c(TN,my_out)
}

##Gfr?rli Nights


GN<-numeric()

for(i in v) {
  my_out2<-length(which(Tmin_night[,i]<=10 & Tmin_night[,i]>=0))
  GN<-c(GN,my_out2)
}


Logs<-colnames(Tmin_night[3:82])
Tropical_Nights_20<-data.frame(Logs,Tmin_avg, TN, GN)


###############################################################################################################

#F?r die andern Parameter: Nacht von 22 bis 6

data_night<-filter(data, Stunde>=15 & Stunde< 23)
data_day<-filter(data, Stunde<15 | Stunde ==23)

Night_group<-group_by(data_night, Monat, Tag)
Day_group<-group_by(data_day, Monat, Tag)
All_group<-group_by(data, Monat, Tag)


Night_mean<-summarise(Night_group, Log1 = mean(Log_1, na.rm=T),Log2 = mean(Log_2, na.rm=T),Log7 = mean(Log_7, na.rm=T),
                      Log8 = mean(Log_8, na.rm=T),Log9 = mean(Log_9, na.rm=T),Log10 = mean(Log_10, na.rm=T),Log13 = mean(Log_13, na.rm=T),
                      Log14 = mean(Log_14, na.rm=T),Log16 = mean(Log_16, na.rm=T),Log17 = mean(Log_17, na.rm=T),
                      Log19 = mean(Log_19, na.rm=T),Log21 = mean(Log_21, na.rm=T),Log22 = mean(Log_22, na.rm=T),
                      Log23 = mean(Log_23, na.rm=T),Log25 = mean(Log_25, na.rm=T),Log26 = mean(Log_26, na.rm=T),
                      Log28 = mean(Log_28, na.rm=T),Log30 = mean(Log_30, na.rm=T),Log31 = mean(Log_31, na.rm=T),
                      Log32 = mean(Log_32, na.rm=T),Log33 = mean(Log_33, na.rm=T),Log34 = mean(Log_34, na.rm=T),
                      Log36 = mean(Log_36, na.rm=T),Log37 = mean(Log_37, na.rm=T),Log38 = mean(Log_38, na.rm=T),
                      Log39 = mean(Log_39, na.rm=T),Log42 = mean(Log_42, na.rm=T),
                      Log45 = mean(Log_45, na.rm=T),Log48 = mean(Log_48, na.rm=T),Log51 = mean(Log_51, na.rm=T),
                      Log52 = mean(Log_52, na.rm=T),Log53 = mean(Log_53, na.rm=T),Log55 = mean(Log_55, na.rm=T),
                      Log57 = mean(Log_57, na.rm=T),Log58 = mean(Log_58, na.rm=T),Log59 = mean(Log_59, na.rm=T),
                      Log61 = mean(Log_61, na.rm=T),Log62 = mean(Log_62, na.rm=T),Log65 = mean(Log_65, na.rm=T),
                      Log68 = mean(Log_68, na.rm=T),Log69 = mean(Log_69, na.rm=T),Log70 = mean(Log_70, na.rm=T),
                      Log71 = mean(Log_71, na.rm=T),Log73 = mean(Log_73, na.rm=T),Log76 = mean(Log_76, na.rm=T),
                      Log78 = mean(Log_78, na.rm=T),Log79 = mean(Log_79, na.rm=T),Log80 = mean(Log_80, na.rm=T),
                      Log82 = mean(Log_82, na.rm=T),Log83 = mean(Log_83, na.rm=T),Log86 = mean(Log_86, na.rm=T),
                      Log87 = mean(Log_87, na.rm=T),Log101 = mean(Log_101, na.rm=T),Log102 = mean(Log_102, na.rm=T),
                      Log110 = mean(Log_110, na.rm=T), Log111 = mean(Log_111, na.rm=T), Log112 = mean(Log_112, na.rm=T),
                      Log113 = mean(Log_113, na.rm=T), Log114 = mean(Log_114, na.rm=T), Log115 = mean(Log_115, na.rm=T),
                      Log116 = mean(Log_116, na.rm=T), Log117 = mean(Log_117, na.rm=T), Log118 = mean(Log_118, na.rm=T),
                      Log119 = mean(Log_119, na.rm=T), Log122 = mean(Log_122, na.rm=T), Log124 = mean(Log_124, na.rm=T),
                      Log125 = mean(Log_125, na.rm=T), Log131 = mean(Log_131, na.rm=T), Log132 = mean(Log_132, na.rm=T),
                      Log133 = mean(Log_133, na.rm=T), Log141 = mean(Log_141, na.rm=T), Log142 = mean(Log_142, na.rm=T),
                      Log143 = mean(Log_143, na.rm=T), Log151 = mean(Log_151, na.rm=T), Log152 = mean(Log_152, na.rm=T),
                      Log125 = mean(Log_125, na.rm=T), Log131 = mean(Log_131, na.rm=T), Log132 = mean(Log_132, na.rm=T),
                      Log153 = mean(Log_153, na.rm=T), Log154 = mean(Log_154, na.rm=T), Log155 = mean(Log_155, na.rm=T),
                      Log99 = mean(Log_99, na.rm=T),Log98 = mean(Log_98, na.rm=T))


Night_mean$Log10[which(is.infinite(Night_mean$Log10))] <- NA
Night_mean$Log111[which(is.infinite(Night_mean$Log111))] <- NA
Night_mean$Log117[which(is.infinite(Night_mean$Log117))] <- NA
Night_mean$Log118[which(is.infinite(Night_mean$Log118))] <- NA
Night_mean$Log119[which(is.infinite(Night_mean$Log119))] <- NA
Night_mean$Log53[which(is.infinite(Night_mean$Log53))] <- NA

Tmean_avg_Night<-colMeans(Night_mean[,3:82], na.rm=T)

##################################################################################################################
#UHI pro Nacht rechnen

UHI_mean<-mutate(Night_mean, ULog1 = Log1-Log99,ULog2 = Log2-Log99,ULog7 = Log7-Log99, ULog8=Log8-Log99,
                 ULog9=Log9-Log99, ULog10=Log10-Log99, ULog13=Log13-Log99, ULog14=Log14-Log99, ULog16=Log16-Log99,
                 ULog17=Log17-Log99, ULog19=Log19-Log99, ULog21=Log21-Log99, ULog22=Log22-Log99, ULog23=Log23-Log99,
                 ULog25=Log25-Log99, ULog26=Log26-Log99, ULog28=Log28-Log99, ULog30=Log30-Log99, ULog31=Log31-Log99,
                 ULog32=Log32-Log99, ULog33=Log33-Log99, ULog34=Log34-Log99, ULog36=Log36-Log99, ULog37=Log37-Log99,
                 ULog38=Log38-Log99, ULog39=Log39-Log99, ULog42=Log42-Log99, ULog45=Log45-Log99,
                 ULog48=Log48-Log99, ULog51=Log51-Log99, ULog52=Log52-Log99, ULog53=Log53-Log99, ULog55=Log55-Log99,
                 ULog57=Log57-Log99, ULog58=Log58-Log99, ULog59=Log59-Log99, ULog61=Log61-Log99, ULog62=Log62-Log99,
                 ULog65=Log65-Log99, ULog68=Log68-Log99, ULog69=Log69-Log99, ULog70=Log70-Log99, ULog71=Log71-Log99,
                 ULog73=Log73-Log99, ULog76=Log76-Log99, ULog78=Log78-Log99, ULog79=Log79-Log99, ULog80=Log80-Log99,
                 ULog82=Log82-Log99, ULog83=Log83-Log99, ULog86=Log86-Log99, ULog87=Log87-Log99, ULog101=Log101-Log99,
                 ULog102=Log102-Log99, ULog110=Log110-Log99, ULog111=Log111-Log99, ULog112=Log112-Log99, ULog113=Log113-Log99,
                 ULog114=Log114-Log99, ULog115=Log115-Log99, ULog116=Log116-Log99, ULog117=Log117-Log99, ULog98=Log98-Log99,
                 ULog114=Log114-Log99, ULog115=Log115-Log99, ULog116=Log116-Log99, ULog117=Log117-Log99, ULog118=Log118-Log99,
                 ULog119=Log119-Log99, ULog122=Log122-Log99, ULog124=Log124-Log99, ULog125=Log125-Log99, ULog131=Log131-Log99,
                 ULog132=Log132-Log99, ULog133=Log133-Log99, ULog141=Log141-Log99, ULog142=Log142-Log99, ULog143=Log143-Log99,
                 ULog151=Log151-Log99, ULog152=Log152-Log99, ULog153=Log153-Log99, ULog154=Log154-Log99, ULog155=Log155-Log99)

UHI_mean<-select(UHI_mean, ULog1, ULog2, ULog7, ULog8, ULog9, ULog10, ULog13, ULog14, ULog16, ULog17, ULog19, ULog21, ULog22,
                 ULog23, ULog25, ULog26, ULog28, ULog30, ULog31, ULog32, ULog33, ULog34, ULog36, ULog37, ULog38, ULog39,
                 ULog42, ULog45, ULog48, ULog51, ULog52, ULog53, ULog55, ULog57, ULog58, ULog59, ULog61, ULog62,
                 ULog65, ULog68, ULog69, ULog70, ULog71, ULog73, ULog76, ULog78, ULog79, ULog80, ULog82, ULog83, ULog86,
                 ULog87, ULog101, ULog102, ULog110, ULog111, ULog112, ULog113, ULog114, ULog115, ULog116, ULog117,
                 ULog118, ULog119, ULog122, ULog124, ULog125, ULog131, ULog132, ULog133, ULog141, ULog142, ULog143,
                 ULog151, ULog152, ULog153, ULog154, ULog155, ULog98)

rowMeans(UHI_mean, na.rm=T)


#####################################################################################################################

D_mean<-summarise(Day_group, Log1 = mean(Log_1, na.rm=T),Log2 = mean(Log_2, na.rm=T),Log7 = mean(Log_7, na.rm=T),
                  Log8 = mean(Log_8, na.rm=T),Log9 = mean(Log_9, na.rm=T),Log10 = mean(Log_10, na.rm=T),Log13 = mean(Log_13, na.rm=T),
                  Log14 = mean(Log_14, na.rm=T),Log16 = mean(Log_16, na.rm=T),Log17 = mean(Log_17, na.rm=T),
                  Log19 = mean(Log_19, na.rm=T),Log21 = mean(Log_21, na.rm=T),Log22 = mean(Log_22, na.rm=T),
                  Log23 = mean(Log_23, na.rm=T),Log25 = mean(Log_25, na.rm=T),Log26 = mean(Log_26, na.rm=T),
                  Log28 = mean(Log_28, na.rm=T),Log30 = mean(Log_30, na.rm=T),Log31 = mean(Log_31, na.rm=T),
                  Log32 = mean(Log_32, na.rm=T),Log33 = mean(Log_33, na.rm=T),Log34 = mean(Log_34, na.rm=T),
                  Log36 = mean(Log_36, na.rm=T),Log37 = mean(Log_37, na.rm=T),Log38 = mean(Log_38, na.rm=T),
                  Log39 = mean(Log_39, na.rm=T),Log42 = mean(Log_42, na.rm=T),
                  Log45 = mean(Log_45, na.rm=T),Log48 = mean(Log_48, na.rm=T),Log51 = mean(Log_51, na.rm=T),
                  Log52 = mean(Log_52, na.rm=T),Log53 = mean(Log_53, na.rm=T),Log55 = mean(Log_55, na.rm=T),
                  Log57 = mean(Log_57, na.rm=T),Log58 = mean(Log_58, na.rm=T),Log59 = mean(Log_59, na.rm=T),
                  Log61 = mean(Log_61, na.rm=T),Log62 = mean(Log_62, na.rm=T),Log65 = mean(Log_65, na.rm=T),
                  Log68 = mean(Log_68, na.rm=T),Log69 = mean(Log_69, na.rm=T),Log70 = mean(Log_70, na.rm=T),
                  Log71 = mean(Log_71, na.rm=T),Log73 = mean(Log_73, na.rm=T),Log76 = mean(Log_76, na.rm=T),
                  Log78 = mean(Log_78, na.rm=T),Log79 = mean(Log_79, na.rm=T),Log80 = mean(Log_80, na.rm=T),
                  Log82 = mean(Log_82, na.rm=T),Log83 = mean(Log_83, na.rm=T),Log86 = mean(Log_86, na.rm=T),
                  Log87 = mean(Log_87, na.rm=T),Log101 = mean(Log_101, na.rm=T),Log102 = mean(Log_102, na.rm=T),
                  Log110 = mean(Log_110, na.rm=T), Log111 = mean(Log_111, na.rm=T), Log112 = mean(Log_112, na.rm=T),
                  Log113 = mean(Log_113, na.rm=T), Log114 = mean(Log_114, na.rm=T), Log115 = mean(Log_115, na.rm=T),
                  Log116 = mean(Log_116, na.rm=T), Log117 = mean(Log_117, na.rm=T), Log118 = mean(Log_118, na.rm=T),
                  Log119 = mean(Log_119, na.rm=T), Log122 = mean(Log_122, na.rm=T), Log124 = mean(Log_124, na.rm=T),
                  Log125 = mean(Log_125, na.rm=T), Log131 = mean(Log_131, na.rm=T), Log132 = mean(Log_132, na.rm=T),
                  Log133 = mean(Log_133, na.rm=T), Log141 = mean(Log_141, na.rm=T), Log142 = mean(Log_142, na.rm=T),
                  Log143 = mean(Log_143, na.rm=T), Log151 = mean(Log_151, na.rm=T), Log152 = mean(Log_152, na.rm=T),
                  Log125 = mean(Log_125, na.rm=T), Log131 = mean(Log_131, na.rm=T), Log132 = mean(Log_132, na.rm=T),
                  Log153 = mean(Log_153, na.rm=T), Log154 = mean(Log_154, na.rm=T), Log155 = mean(Log_155, na.rm=T),
                  Log99 = mean(Log_99, na.rm=T),Log98 = mean(Log_98, na.rm=T))


D_mean$Log10[which(is.infinite(D_mean$Log10))] <- NA
D_mean$Log111[which(is.infinite(D_mean$Log111))] <- NA
D_mean$Log117[which(is.infinite(D_mean$Log117))] <- NA
D_mean$Log118[which(is.infinite(D_mean$Log118))] <- NA
D_mean$Log119[which(is.infinite(D_mean$Log119))] <- NA
D_mean$Log53[which(is.infinite(D_mean$Log53))] <- NA

Tmean_avg_Day<-colMeans(D_mean[,3:82], na.rm=T)



######################################################################################################################

##All_mean

All_mean<-summarise(All_group, Log1 = mean(Log_1, na.rm=T),Log2 = mean(Log_2, na.rm=T),Log7 = mean(Log_7, na.rm=T),
                    Log8 = mean(Log_8, na.rm=T),Log9 = mean(Log_9, na.rm=T),Log10 = mean(Log_10, na.rm=T),Log13 = mean(Log_13, na.rm=T),
                    Log14 = mean(Log_14, na.rm=T),Log16 = mean(Log_16, na.rm=T),Log17 = mean(Log_17, na.rm=T),
                    Log19 = mean(Log_19, na.rm=T),Log21 = mean(Log_21, na.rm=T),Log22 = mean(Log_22, na.rm=T),
                    Log23 = mean(Log_23, na.rm=T),Log25 = mean(Log_25, na.rm=T),Log26 = mean(Log_26, na.rm=T),
                    Log28 = mean(Log_28, na.rm=T),Log30 = mean(Log_30, na.rm=T),Log31 = mean(Log_31, na.rm=T),
                    Log32 = mean(Log_32, na.rm=T),Log33 = mean(Log_33, na.rm=T),Log34 = mean(Log_34, na.rm=T),
                    Log36 = mean(Log_36, na.rm=T),Log37 = mean(Log_37, na.rm=T),Log38 = mean(Log_38, na.rm=T),
                    Log39 = mean(Log_39, na.rm=T),Log42 = mean(Log_42, na.rm=T),
                    Log45 = mean(Log_45, na.rm=T),Log48 = mean(Log_48, na.rm=T),Log51 = mean(Log_51, na.rm=T),
                    Log52 = mean(Log_52, na.rm=T),Log53 = mean(Log_53, na.rm=T),Log55 = mean(Log_55, na.rm=T),
                    Log57 = mean(Log_57, na.rm=T),Log58 = mean(Log_58, na.rm=T),Log59 = mean(Log_59, na.rm=T),
                    Log61 = mean(Log_61, na.rm=T),Log62 = mean(Log_62, na.rm=T),Log65 = mean(Log_65, na.rm=T),
                    Log68 = mean(Log_68, na.rm=T),Log69 = mean(Log_69, na.rm=T),Log70 = mean(Log_70, na.rm=T),
                    Log71 = mean(Log_71, na.rm=T),Log73 = mean(Log_73, na.rm=T),Log76 = mean(Log_76, na.rm=T),
                    Log78 = mean(Log_78, na.rm=T),Log79 = mean(Log_79, na.rm=T),Log80 = mean(Log_80, na.rm=T),
                    Log82 = mean(Log_82, na.rm=T),Log83 = mean(Log_83, na.rm=T),Log86 = mean(Log_86, na.rm=T),
                    Log87 = mean(Log_87, na.rm=T),Log101 = mean(Log_101, na.rm=T),Log102 = mean(Log_102, na.rm=T),
                    Log110 = mean(Log_110, na.rm=T), Log111 = mean(Log_111, na.rm=T), Log112 = mean(Log_112, na.rm=T),
                    Log113 = mean(Log_113, na.rm=T), Log114 = mean(Log_114, na.rm=T), Log115 = mean(Log_115, na.rm=T),
                    Log116 = mean(Log_116, na.rm=T), Log117 = mean(Log_117, na.rm=T), Log118 = mean(Log_118, na.rm=T),
                    Log119 = mean(Log_119, na.rm=T), Log122 = mean(Log_122, na.rm=T), Log124 = mean(Log_124, na.rm=T),
                    Log125 = mean(Log_125, na.rm=T), Log131 = mean(Log_131, na.rm=T), Log132 = mean(Log_132, na.rm=T),
                    Log133 = mean(Log_133, na.rm=T), Log141 = mean(Log_141, na.rm=T), Log142 = mean(Log_142, na.rm=T),
                    Log143 = mean(Log_143, na.rm=T), Log151 = mean(Log_151, na.rm=T), Log152 = mean(Log_152, na.rm=T),
                    Log125 = mean(Log_125, na.rm=T), Log131 = mean(Log_131, na.rm=T), Log132 = mean(Log_132, na.rm=T),
                    Log153 = mean(Log_153, na.rm=T), Log154 = mean(Log_154, na.rm=T), Log155 = mean(Log_155, na.rm=T),
                    Log99 = mean(Log_99, na.rm=T),Log98 = mean(Log_98, na.rm=T))


All_mean$Log10[which(is.infinite(All_mean$Log10))] <- NA
All_mean$Log111[which(is.infinite(All_mean$Log111))] <- NA
All_mean$Log117[which(is.infinite(All_mean$Log117))] <- NA
All_mean$Log118[which(is.infinite(All_mean$Log118))] <- NA
All_mean$Log119[which(is.infinite(All_mean$Log119))] <- NA
All_mean$Log53[which(is.infinite(All_mean$Log53))] <- NA
Tmean_avg_all<-colMeans(All_mean[,3:82], na.rm=T)


##################################################################################################################

##MaxTemps


All_max<-summarise(All_group, Log1 = max(Log_1, na.rm=T),Log2 = max(Log_2, na.rm=T),Log7 = max(Log_7, na.rm=T),
                   Log8 = max(Log_8, na.rm=T),Log9 = max(Log_9, na.rm=T),Log10 = max(Log_10, na.rm=T),Log13 = max(Log_13, na.rm=T),
                   Log14 = max(Log_14, na.rm=T),Log16 = max(Log_16, na.rm=T),Log17 = max(Log_17, na.rm=T),
                   Log19 = max(Log_19, na.rm=T),Log21 = max(Log_21, na.rm=T),Log22 = max(Log_22, na.rm=T),
                   Log23 = max(Log_23, na.rm=T),Log25 = max(Log_25, na.rm=T),Log26 = max(Log_26, na.rm=T),
                   Log28 = max(Log_28, na.rm=T),Log30 = max(Log_30, na.rm=T),Log31 = max(Log_31, na.rm=T),
                   Log32 = max(Log_32, na.rm=T),Log33 = max(Log_33, na.rm=T),Log34 = max(Log_34, na.rm=T),
                   Log36 = max(Log_36, na.rm=T),Log37 = max(Log_37, na.rm=T),Log38 = max(Log_38, na.rm=T),
                   Log39 = max(Log_39, na.rm=T),Log42 = max(Log_42, na.rm=T),
                   Log45 = max(Log_45, na.rm=T),Log48 = max(Log_48, na.rm=T),Log51 = max(Log_51, na.rm=T),
                   Log52 = max(Log_52, na.rm=T),Log53 = max(Log_53, na.rm=T),Log55 = max(Log_55, na.rm=T),
                   Log57 = max(Log_57, na.rm=T),Log58 = max(Log_58, na.rm=T),Log59 = max(Log_59, na.rm=T),
                   Log61 = max(Log_61, na.rm=T),Log62 = max(Log_62, na.rm=T),Log65 = max(Log_65, na.rm=T),
                   Log68 = max(Log_68, na.rm=T),Log69 = max(Log_69, na.rm=T),Log70 = max(Log_70, na.rm=T),
                   Log71 = max(Log_71, na.rm=T),Log73 = max(Log_73, na.rm=T),Log76 = max(Log_76, na.rm=T),
                   Log78 = max(Log_78, na.rm=T),Log79 = max(Log_79, na.rm=T),Log80 = max(Log_80, na.rm=T),
                   Log82 = max(Log_82, na.rm=T),Log83 = max(Log_83, na.rm=T),Log86 = max(Log_86, na.rm=T),
                   Log87 = max(Log_87, na.rm=T),Log101 = max(Log_101, na.rm=T),Log102 = max(Log_102, na.rm=T),
                   Log110 = max(Log_110, na.rm=T), Log111 = max(Log_111, na.rm=T), Log112 = max(Log_112, na.rm=T),
                   Log113 = max(Log_113, na.rm=T), Log114 = max(Log_114, na.rm=T), Log115 = max(Log_115, na.rm=T),
                   Log116 = max(Log_116, na.rm=T), Log117 = max(Log_117, na.rm=T), Log118 = max(Log_118, na.rm=T),
                   Log119 = max(Log_119, na.rm=T), Log122 = max(Log_122, na.rm=T), Log124 = max(Log_124, na.rm=T),
                   Log125 = max(Log_125, na.rm=T), Log131 = max(Log_131, na.rm=T), Log132 = max(Log_132, na.rm=T),
                   Log133 = max(Log_133, na.rm=T), Log141 = max(Log_141, na.rm=T), Log142 = max(Log_142, na.rm=T),
                   Log143 = max(Log_143, na.rm=T), Log151 = max(Log_151, na.rm=T), Log152 = max(Log_152, na.rm=T),
                   Log125 = max(Log_125, na.rm=T), Log131 = max(Log_131, na.rm=T), Log132 = max(Log_132, na.rm=T),
                   Log153 = max(Log_153, na.rm=T), Log154 = max(Log_154, na.rm=T), Log155 = max(Log_155, na.rm=T),
                   Log99 = max(Log_99, na.rm=T),Log98 = max(Log_98, na.rm=T))

All_max$Log10[which(is.infinite(All_max$Log10))] <- NA
All_max$Log111[which(is.infinite(All_max$Log111))] <- NA
All_max$Log117[which(is.infinite(All_max$Log117))] <- NA
All_max$Log118[which(is.infinite(All_max$Log118))] <- NA
All_max$Log119[which(is.infinite(All_max$Log119))] <- NA
All_max$Log53[which(is.infinite(All_max$Log53))] <- NA

Tmax_avg_all<-colMeans(All_max[,3:82], na.rm=T)

##Summer Days

v<-c(3:82)

SD<-numeric()

for(i in v) {
  my_out<-length(which(All_max[,i]>=25 & All_max[,i]<=50))
  SD<-c(SD,my_out)
}


##Hot Days

v<-c(3:82)

HD<-numeric()

for(i in v) {
  my_out<-length(which(All_max[,i]>=30 & All_max[,i]<=50))
  HD<-c(HD,my_out)
}


##very Hot Days

v<-c(3:82)

vHD<-numeric()

for(i in v) {
  my_out<-length(which(All_max[,i]>=35 & All_max[,i]<=50))
  vHD<-c(vHD,my_out)
}




Output<-data.frame(Logs, Tmean_avg_all, Tmean_avg_Night, Tmean_avg_Day, Tmin_avg, Tmax_avg_all, TN, GN, SD, HD, vHD)

Output<-mutate(Output, UHI_all=Tmean_avg_all-20.61285)
Output<-mutate(Output, UHI_night=Tmean_avg_Night-16.68675)
Output<-mutate(Output, UHI_day=Tmean_avg_Day-22.57926)

Output<-select(Output, Logs, Tmean_avg_all, UHI_all, Tmean_avg_Night, UHI_night, Tmean_avg_Day, UHI_day, Tmin_avg, Tmax_avg_all, TN, GN, SD, HD, vHD)

write.table(Output, "Summary_2022_22.csv", sep=";")
