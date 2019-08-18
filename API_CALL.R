setwd("C:/Users/osull/OneDrive/Documents/Dublin_Bike_Task")

library(httr) # Communication to the web
library(jsonlite) # Web data manipulation
library(lubridate) # Date functions
# library(tidyr) # 

api_key <- 'b1df416906d3deace2327610759bb5448bbc3799'

api_call <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=Dublin&apiKey=b1df416906d3deace2327610759bb5448bbc3799")

station_status <- jsonlite::fromJSON(api_call)
station_status['Time_of_Call'] <- now()
# View(station_status)
# str(station_status)
# sapply(station_status, class)

unnest_dataframes <- function(x) {
  y <- do.call(data.frame, x)
  if("data.frame" %in% sapply(y,class)) unnest_dataframes(y)
  y
}

station_status <- unnest_dataframes(station_status)

write.table(station_status, file="Station_Status.csv", sep=",", append=TRUE, col.names =!file.exists("Station_Status.csv"))

# names(station_status)
