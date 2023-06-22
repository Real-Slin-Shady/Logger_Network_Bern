
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


tables <- client$query('from(bucket: "smcs") |> range(start: 2023-05-01) |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")')



# purrr::map(tables, ~colnames(.,sub("_", "X_", colnames(tables))))



files = list.files("./data")


# Formatting tables

for (x in 1:length(tables)) {
  colnames(tables[[x]]) <- sub("_", "X", colnames(tables[[x]]))

  tables[[x]] <- tables[[x]] |>
    mutate(
      time = round(as.POSIXct(time), "secs"),
      Xtime = round(as.POSIXct(Xtime), "secs"),
      Xstart = round(as.POSIXct(Xstart), "secs"),
      Xstop = round(as.POSIXct(Xstop), "secs"))
}




#adding backup data to avoid overwrite

for (file in files) {
  for (x in 1:length(tables)) {
    if (substr(unique(tables[[x]]$topic),8,19) == substr(file,1,12)) {
        temp = read_csv(paste("data/",file,sep = ""),col_types = cols("T","T","T","c","c","c","d","d","T"))
             print(x)
            print(paste("Name live Table:",unique(tables[[x]]$topic)))
            print(paste("Name file :",file))
            print(paste("rows live Table:",nrow(tables[[x]])))
            print(paste("rows file:",nrow(temp)))
        tables[[x]] <- bind_rows(temp,tables[[x]])
        tables[[x]] <- dplyr::distinct(tables[[x]],time, .keep_all= TRUE)
            print(paste("rows added:",nrow(tables[[x]])-nrow(temp)))
            cat(paste("rows combined :",nrow(tables[[x]]),"\n \n"))


    }
  }

}



for (x in 1:length(tables)) {
  write_csv(tables[[x]],paste("data/",substr(unique(tables[[x]]$topic),8,19),".csv",sep = ""))
  print(substr(unique(tables[[x]]$topic),8,19))
}
