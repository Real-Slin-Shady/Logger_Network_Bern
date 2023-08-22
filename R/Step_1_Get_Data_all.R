library(influxdbclient)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

################################################################################
# READ IN DATA FROM GRAFANA AND METADATA

# You can generate an API token from the "API Tokens Tab" in the UI
token = "tu3zUeCazQobS4TrIIRftQS3Tr4xoZQoZaRf0Ve0iCrU4LZSY1jTS3laCJ_OjwJxWJ6WsKuwXN_tVV10R73hyg=="

client <- InfluxDBClient$new(url = "https://influx.smcs.abilium.io",
                             token = token,
                             org = "abilium")

# Adjust start time here!
start = "2023-08-20"

tables <- client$query(paste0('from(bucket: "smcs") |> range(start: ', start, ') |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")'))

meta <- read_csv("./data/metadata_network_2023.csv")|>
  mutate(name = Code_grafana)

code_grafana <- meta$Code_grafana

# Create empty two dataframes for all loggers
combined_T <- tibble(Time = as.POSIXct(character(), format = "%Y-%m-%d %H:%M"))
combined_RH <- tibble(Time = as.POSIXct(character(), format = "%Y-%m-%d %H:%M"))


################################################################################
# DATA WRANGLING

# Combine all the loggers into one big table (separate for T and RH)
for (x in 1:length(tables)) {

  log_name <- tables[[x]]$name[1]

  # only take our loggers
  if(log_name %in% meta$Code_grafana) {

    # Get the site name of the logger
    # comment out the following line if you want to use the codes instead
    log_name <- meta$STANDORT_NEU[which(meta$Code_grafana == log_name)]

    # clean data inside the individual loggers and round time to 10mins
    log_data <- tables[[x]] |>
      dplyr::select(time, decoded_payload_temperature, decoded_payload_humidity) |>
      dplyr::rename(RH = decoded_payload_humidity, Temp = decoded_payload_temperature, Time = time) |>
      dplyr::mutate(Time = ymd_hms(Time)) |>
      dplyr::mutate(Time = round_date(Time, unit="10 minutes")) |>
      group_by(Time) |>
      summarise(Temp = mean(Temp), RH = mean(RH))

    # separate the T and RH values
    log_data_T <- log_data |>
      dplyr::select(Time, Temp)

    log_data_RH <- log_data |>
      dplyr::select(Time, RH)

    # add data to the Temperature and RH tables by full join
    # rename the Temp / RH column with the logger's name
    combined_T <- combined_T |>
      full_join(log_data_T, by = "Time") |>
      rename_with(~ log_name, Temp)

    combined_RH <- combined_RH |>
      full_join(log_data_RH, by = "Time") |>
      rename_with(~ log_name, RH)
  }
}


################################################################################
# PLOTTING

ggplot(data = combined_T, aes(x=Time)) +
  geom_line(aes(y = `Bundesplatz`, colour = 'Bundesplatz')) + # make sure to use these `` for site names with a space in between
  geom_line(aes(y = `Ostermundigen Oberfeld`, colour = 'Ostermundigen Oberfeld')) +
  geom_line(aes(y = `Monbijou-Park`, colour = 'Monbijou-Park')) +
  geom_line(aes(y = `Bollwerk`, colour = 'Bollwerk')) +
  labs(y = "Temperature [°C]")


################################################################################
# EXPORT

# set your local directory
setwd("...")

write_csv(combined_T, paste0("T_all_loggers_", start,"-present.csv"))
write_csv(combined_RH, paste0("RH_all_loggers_", start,"-present.csv"))

