###############################################################
# LOGGER DATA CLEA-NUP FROM URBAN CLIMATE MEASUREMENT NETWORK #
###############################################################

# Reads in logger data from csv in 10min temporal resolution.
# Data is the cleaned up by inserting NA where time step does not contain

library(dplyr)
# https://dplyr.tidyverse.org/

wd_data <- 'C:/A_Noémie/Studium/GIUB/Other stuff/Loggerdaten/Logger_raw'
wd_res <- 'C:/A_Noémie/Studium/GIUB/Other stuff/Loggerdaten/Logger_cleaned'
  
setwd(wd_data)

### READ IN LOGGER FILES

# Get file names and write them in a list
# https://stackoverflow.com/questions/14958516/read-all-files-in-directory-and-apply-multiple-functions-to-each-data-frame
loggers_filenames <- list.files(path=wd_data, pattern="*.csv", recursive=FALSE)

#--------------------------------------------------------------------------------

### CLEAN UP DATA

# do it for all loggers in a folder
for (i in loggers_filenames) {
  setwd(wd_data)
  logger <- read.csv(i)
  logger <- rename(logger, temp = T) #variablen wenn möglich nie T oder F nennen, weil dies auch für TRUE und FALSE steht :)
  # Group rows of same time step together by mean
  logger_cleaned <- logger |>
    group_by(Round_Time) |> 
    summarise(temp = mean(temp), RH = mean(RH))
  setwd(wd_res)
  write.csv(logger_cleaned, paste(sub(".csv$","",i), "_clean.csv"), row.names=FALSE)
}


# # only for 1 logger
# setwd(wd_data)
# logger1 <- read.csv('Zoll_alleine.csv')
# logger1 <- rename(logger1, temp = T) #variablen wenn möglich nie T oder F nennen, weil dies auch für TRUE und FALSE steht :)
# logger1_cleaned <- logger1 |> 
#   group_by(Round_Time) |>
#   summarise(temp = mean(temp), RH = mean(RH))
# 
# head(logger1_cleaned)
# head(logger1)


### NEXT STEPS

# Combine all logger dataframes with left join. (Rename columns temp and RH to temp_loggername and RH_loggername)
# Insert NA values where measurements are missing