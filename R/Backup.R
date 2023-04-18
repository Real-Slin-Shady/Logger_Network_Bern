
packages <- c("influxdbclient","ggplot2","tidyverse")

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


tables <- client$query('from(bucket: "smcs") |> range(start: 2022-06-01) |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")')

# files = list.files("./data")
# for (file in files) {
#   for (x in 1:length(tables)) {
#     if (substr(unique(tables[[x]]$topic),8,19) == substr(file,1,12)) {
#       tables[[x]] |>
#         dplyr::rows_update()
#       print(unique(tables[[x]]$topic))
#     }
#   }
#
# }
#Unnecessary, may be implemented later, for now everything is overwritten and then uploaded to github


for (x in 1:length(tables)) {
  write_csv(tables[[x]],paste("data/",substr(unique(tables[[x]]$topic),8,19),".csv",sep = ""))
  print(substr(unique(tables[[x]]$topic),8,19))
}
