
packages <- c("influxdbclient","ggplot2","tidyverse","lubridate")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}else{
  print("All packages sucessfully installed")
}

invisible(lapply(packages, library, character.only = TRUE))


# You can generate an API token from the "API Tokens Tab" in the UI
token = "tu3zUeCazQobS4TrIIRftQS3Tr4xoZQoZaRf0Ve0iCrU4LZSY1jTS3laCJ_OjwJxWJ6WsKuwXN_tVV10R73hyg=="

client <- InfluxDBClient$new(url = "https://influx.smcs.abilium.io",
                             token = token,
                             org = "abilium")


tables <- client$query('from(bucket: "smcs") |> range(start: 2023-05-01) |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")')


currenttime <- Sys.time()
laggingtime <- currenttime - lubridate::minutes(30)

# test_tabel <- tables[[80]]
# test_tabel$time <- with_tz(test_tabel$time,"Europe/Zurich")
# test_tabel |>
#   filter(time > laggingtime)
combined <- tibble(name = character(), temperature = double())


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


meta <- read_csv("./data/metadata_network_2023.csv")|>
  mutate(name = Code_grafana)

combined <- combined |>
  filter(name %in% meta$Code_grafana)
#only take ours

#Attach meta information
combined <- combined |>
  inner_join(meta, by = "name")

meta_LUR <- read_csv("./data/LUR_Test.csv") |>
  select(-HuM)

combined <- combined |>
  inner_join(meta_LUR, by = "STANDORT")


#Dataframe to export (cleanup):

correlators <- combined |>
  select(-c(temperature,name,Diff_HoheZoll,Longitude,Latitude,Log_Nr_19,Doppel_Messnetz_23,BEFESTIGUNG,ZUSTAENDIG,Art,Log_NR,LV_03_E,LV_03_N,OST_CHTOPO,NORD_CHTOPO,Code_grafana,STANDORT_NEU,STANDORT)) |>
  colnames() |>
  paste(collapse  = "+")
formula_local = as.formula(paste("temperature","~", correlators))

model <- lm(formula_local, data = combined)

tiff_names <- list.files("./data/Tiffs/")

tiffs <- list()
for(tiff in tiff_names){
  print(tiff)
tiffs[[tiff]] <- raster::raster(paste("./data/Tiffs/",tiff,sep = ""))
coeff_temp <- unname(coefficients(model)[c(str_sub(tiff,end = -5))])
tiffs[[tiff]] <- tiffs[[tiff]]*coeff_temp
print(coeff_temp)
}

out = unname(coefficients(model)["(Intercept)"])
for (tiff in tiffs) {
  out = out + tiff
}



raster::plot(out, col = heat.colors(100))
points(combined$LV_03_N,combined$LV_03_E , pch = 16)
title(paste("R^2 = ",summary(model)$r.squared))



