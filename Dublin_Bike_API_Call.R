
# set your working directory to the necessary path, this is where the Excel file will be stored
setwd("C:/Users/osull/Dublin_Bike_Task")

# load libraries needed to do the task
library(httr) # Communication to the web
library(jsonlite) # Web data manipulation
library(lubridate) # Date functions

# create a variable with the generated API key & insert key here
api_key <- "XXXX"

# create a variable to call the API from the Dublin Bike website
api_call <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=Dublin&apiKey=",api_key)

# create a dataframe that stores the output from the API call
station_status <- jsonlite::fromJSON(api_call)

# inserting a new column with the current time
station_status['Time_of_Call'] <- now()

# the below line gives us the datatypes of all our variables
# sapply(station_status, class)

# creating a function to unnest any data frames within the dataframe (necessary to save to a CSV file)
unnest_dataframes <- function(x) {
  y <- do.call(data.frame, x)
  if("data.frame" %in% sapply(y,class)) unnest_dataframes(y)
  y
}

# applying the unnesting function to the dataframe
final_station_status <- unnest_dataframes(station_status)

# last step, output the dataframe to a CSV file
# append=true allows us to update the file while col.names=!file.exists allows us to only add column headers if the file doesn't exist
write.table(final_station_status, file="Station_Status.csv", sep=",", append=TRUE, col.names =!file.exists("Station_Status.csv"))
