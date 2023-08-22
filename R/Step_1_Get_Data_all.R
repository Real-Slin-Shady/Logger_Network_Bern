library(influxdbclient)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

# You can generate an API token from the "API Tokens Tab" in the UI
token = "tu3zUeCazQobS4TrIIRftQS3Tr4xoZQoZaRf0Ve0iCrU4LZSY1jTS3laCJ_OjwJxWJ6WsKuwXN_tVV10R73hyg=="

client <- InfluxDBClient$new(url = "https://influx.smcs.abilium.io",
                             token = token,
                             org = "abilium")

# Get data, adjust time range
tables <- client$query('from(bucket: "smcs") |> range(start: 2023-08-20) |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")')

meta <- read_csv("./data/metadata_network_2023.csv")|>
  mutate(name = Code_grafana)

code_grafana <- meta$Code_grafana

# Create empty dataframe for all loggers
combined <- tibble(time = as.POSIXct(character(), format = "%Y-%m-%d %H:%M"))
for (i in code_grafana) {
  combined[[i]] = double()
}

##################################################################################
# clean data inside the individual loggers and round time to 10mins

temp <- tables[[1]] |>
  dplyr::select(time, decoded_payload_temperature, decoded_payload_humidity) |>
  dplyr::rename(RH = decoded_payload_humidity, Temp = decoded_payload_temperature, Time = time) |>
  dplyr::mutate(Time = ymd_hms(Time)) |>
  dplyr::mutate(Time = round_date(Time, unit="10 minutes")) |>
  group_by(Time) |>
    summarise(Temp = mean(Temp), RH = mean(RH))
log_name <-

##################################################################################
for (x in 1:length(tables)) {
  tables[[x]]$time <- with_tz(tables[[x]]$time,"Europe/Zurich")
  temp <- tables[[x]] |>
    filter(time > laggingtime)|>
    group_by(topic)|>
    summarise(temp = mean(decoded_payload_temperature))

  combined <- combined |>
    add_row(name = temp$topic, temperature = temp$temp)
  #somewhow emtpy rows are dropped, nice ;)

}
combined <- combined |> mutate(
  name = substr(name, 8,19)
)


combined <- combined |>
  filter(name %in% meta$Code_grafana)
#only take ours


# TODO clean tables and separate RH and T

# TODO read in metadata and combine

Zoll_alleine<-tables[[9]]
Wankdorf_3<-tables[[8]]
Wankdrof_2<-tables[[11]]
VF_Schacht<-tables[[13]]
Zoll_zusammen<-tables[[14]]

##########################################################################3
#Create friendly Data Tables

#Zollikofen alleine

#Nur die wichtigen Spalten werden ?bernommen

ZA<-Zoll_alleine[,7:9]%>%
  select(time, decoded_payload_temperature,decoded_payload_humidity)%>%
  rename(RH=decoded_payload_humidity, Temp=decoded_payload_temperature, Time=time)

##Jetzt werden die Daten auf 10 Min Schritte gerundet

ZA$Time<-strptime(ZA$Time,"%Y-%m-%d %H:%M")
ZA$Time<-as.POSIXct(ZA$Time)

UTC<-round_date(ZA$Time, unit="10 minutes")

ZA_final<-data.frame(ZA, UTC)%>%
  select(Time, UTC, Temp, RH)

ZA_final$UTC<-strptime(ZA_final$UTC,"%Y-%m-%d %H:%M")
ZA_final$UTC<-as.POSIXct(ZA_final$UTC)

#Nun werden die doppelten Werte pro 10 Min Schritt gemittelt

ZA_cleaned <- ZA_final %>%
  group_by(UTC) %>%
  summarise(Temp = mean(Temp), RH = mean(RH))

#Plot und speichern!

ggplot(ZA_cleaned)+
  geom_line(aes(x=UTC, y=Temp))

setwd("D:/Daten Temp_Bern/Logger_Validation/Nice_Tables")

write.csv(ZA_cleaned, "Zoll_alleine.csv", row.names=F)

##Zoll zusammen

ZZ<-Zoll_zusammen[,7:9]%>%
  select(time, decoded_payload_temperature,decoded_payload_humidity)%>%
  rename(RH=decoded_payload_humidity, Temp=decoded_payload_temperature, Time=time)

ZZ$Time<-strptime(ZZ$Time,"%Y-%m-%d %H:%M")
ZZ$Time<-as.POSIXct(ZZ$Time)

UTC<-round_date(ZZ$Time, unit="10 minutes")

ZZ_final<-data.frame(ZZ, UTC)%>%
  select(Time, UTC, Temp, RH)

ZZ_final$UTC<-strptime(ZZ_final$UTC,"%Y-%m-%d %H:%M")
ZZ_final$UTC<-as.POSIXct(ZZ_final$UTC)

#Nun werden die doppelten Werte pro 10 Min Schritt gemittelt

ZZ_cleaned <- ZZ_final %>%
  group_by(UTC) %>%
  summarise(Temp = mean(Temp), RH = mean(RH))

#Plot und speichern!

ggplot(ZZ_cleaned)+
  geom_line(aes(x=UTC, y=Temp))

setwd("D:/Daten Temp_Bern/Logger_Validation/Nice_Tables")

write.csv(ZZ_cleaned, "Zoll_zusammen.csv", row.names=F)

##Wankdorf 3

W3<-Wankdorf_3[,7:9]%>%
  select(time, decoded_payload_temperature,decoded_payload_humidity)%>%
  rename(RH=decoded_payload_humidity, Temp=decoded_payload_temperature, Time=time)

W3$Time<-strptime(W3$Time,"%Y-%m-%d %H:%M")
W3$Time<-as.POSIXct(W3$Time)

UTC<-round_date(W3$Time, unit="10 minutes")

W3_final<-data.frame(W3, UTC)%>%
  select(Time, UTC, Temp, RH)

W3_final$UTC<-strptime(W3_final$UTC,"%Y-%m-%d %H:%M")
W3_final$UTC<-as.POSIXct(W3_final$UTC)

#Nun werden die doppelten Werte pro 10 Min Schritt gemittelt

W3_cleaned <- W3_final %>%
  group_by(UTC) %>%
  summarise(Temp = mean(Temp), RH = mean(RH))

#Plot und speichern!

ggplot(W3_cleaned)+
  geom_line(aes(x=UTC, y=Temp))

setwd("D:/Daten Temp_Bern/Logger_Validation/Nice_Tables")

write.csv(W3_cleaned, "Wankdorf_3.csv", row.names=F)



##Wankdorf 2

W2<-Wankdrof_2[,7:9]%>%
  select(time, decoded_payload_temperature,decoded_payload_humidity)%>%
  rename(RH=decoded_payload_humidity, Temp=decoded_payload_temperature, Time=time)

W2$Time<-strptime(W2$Time,"%Y-%m-%d %H:%M")
W2$Time<-as.POSIXct(W2$Time)

UTC<-round_date(W2$Time, unit="10 minutes")

W2_final<-data.frame(W2, UTC)%>%
  select(Time, UTC, Temp, RH)

W2_final$UTC<-strptime(W2_final$UTC,"%Y-%m-%d %H:%M")
W2_final$UTC<-as.POSIXct(W2_final$UTC)

#Nun werden die doppelten Werte pro 10 Min Schritt gemittelt

W2_cleaned <- W2_final %>%
  group_by(UTC) %>%
  summarise(Temp = mean(Temp), RH = mean(RH))

#Plot und speichern!

ggplot(W2_cleaned)+
  geom_line(aes(x=UTC, y=Temp))

setwd("D:/Daten Temp_Bern/Logger_Validation/Nice_Tables")

write.csv(W2_cleaned, "Wankdorf_2.csv", row.names=F)

##Viererfeld Schacht

VS<-VF_Schacht[,7:9]%>%
  select(time, decoded_payload_temperature,decoded_payload_humidity)%>%
  rename(RH=decoded_payload_humidity, Temp=decoded_payload_temperature, Time=time)

VS$Time<-strptime(VS$Time,"%Y-%m-%d %H:%M")
VS$Time<-as.POSIXct(VS$Time)

UTC<-round_date(VS$Time, unit="10 minutes")

VS_final<-data.frame(VS, UTC)%>%
  select(Time, UTC, Temp, RH)

VS_final$UTC<-strptime(VS_final$UTC,"%Y-%m-%d %H:%M")
VS_final$UTC<-as.POSIXct(VS_final$UTC)

#Nun werden die doppelten Werte pro 10 Min Schritt gemittelt

VS_cleaned <- VS_final %>%
  group_by(UTC) %>%
  summarise(Temp = mean(Temp), RH = mean(RH))

#Plot und speichern!

ggplot(VS_cleaned)+
  geom_line(aes(x=UTC, y=Temp))

setwd("D:/Daten Temp_Bern/Logger_Validation/Nice_Tables")

write.csv(VS_cleaned, "Viererfeld_Schacht.csv", row.names=F)



