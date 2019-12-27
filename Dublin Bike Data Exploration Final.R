# setting my working directory
setwd("xxx")

# load the libraries you want to use
library(ggplot2) #allows us to create nice graphs
library(anytime) #allows us to convert datatypes
library(lubridate) #gives us date functions 
library(chron) #more date functions
library(dplyr) #useful summary functions
library(gganimate) #animates graphs
library(corrplot) #used for correlation plots
library(tidyr) #used to pivot data
library(RColorBrewer) #color scheme
library(gplots) #for a heatmap
library(ggrepel) #allows us to highlight labels in plots

# creating a dataframe with the dublin bike dataset we created over the week
# row.names = null is to handle the incorrect number of column headers we have 
# we were missing the first column header this solution fixes our problem!
bike_data <- read.csv('Station_Status.csv', row.names = NULL)

# this displays the data in a tabular view, really handy for a quick overview of the data
View(bike_data)

# view the column headers along with their data type - basically, a structural summary of the data
str(bike_data)

# a more streamlined view of this could be to use sapply
sapply(bike_data, class)

# we can see that Time_of_Call has been stored as a factor, lets change this to a datetime format
bike_data$Time_of_Call <- anytime(as.factor(bike_data$Time_of_Call))

# we also don't need the 1st column, so lets remove this
bike_data <- bike_data[-c(1)]

# lets see the range of dates & time we have in our data
range(bike_data$Time_of_Call)

# how many null values do we have in our dataset? None!
sum(is.na(bike_data))

# we can use the unique function to see the number of Dublin Bike stands there are (there are 111!)
unique(bike_data$name)

# lets focus on 1 station to start - GRAND CANAL DOCK
# side note: I find limiting data on a single factor useful when trying to explore data
# once I understand how a single factor behaves, I can apply it to the others

# creating a reduced dataframe of just rows which contain data from Grand Canal Dock
grand_canal <- subset(bike_data, name == 'GRAND CANAL DOCK')

# lets plot the number of available bikes 
theme_set(theme_bw())
ggplot(data= grand_canal, aes(x=Time_of_Call, y=available_bikes)) + geom_line()

# we can add more variables into the same plot using the following
p1 <- ggplot(data=grand_canal, aes(Time_of_Call)) +
  geom_line(aes(y = bike_stands, color = "bike_stands"), size=1)+
  geom_line(aes(y = available_bikes, color = "available_bikes"), size =1)

# but we also want to edit the chart titles
p1 <- p1+ggtitle("Bike Availability - Grand Canal Square")

# making the chart title a little prettier
p1 <- p1+theme(plot.title = element_text(size=10, face="bold", 
                                         margin = margin(10, 0, 10, 0)))

# we also want to edit the axis names
p1 <- p1 + labs(x="Time", y = "Number of Bikes")

# finally, turn off the legend title
p1 <- p1+theme(legend.title=element_blank())

# lets view our graph
p1

# great, but all of the above code can be written as one statment, like this
p2 <- ggplot(data=grand_canal, aes(Time_of_Call)) +
  geom_line(aes(y = bike_stands, color = "bike_stands"), size=1)+
  geom_line(aes(y = available_bikes, color = "available_bikes"), size =1)+
  ggtitle("Bike Availability - Grand Canal Square")+
  labs(x="Time", y = "Number of Bikes")+
  theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0)),
        legend.title=element_blank())

# now we have the exact same plot  
p2


# before moving on, we notice from below that 'smithfield station' is closed
# lets remove this station from the dataset 
bike_data <- bike_data[!(bike_data$name=="SMITHFIELD"),]

# unfortuantely, there is no way to show which bikes end up at what stop
# however, what we could look at is which stations have a high correlation with eachother
# this can go onto influence any clustering analysis we do 
# focusing on available bikes, we can transpose our data to allow us to run correaltions on each stops available bikes

# lets create a condensed dataset by isolating columns based on their column number
bike_data_reduced <- bike_data[c(3,11,14)]

# spread() essentailly pivots our data
bike_data_pivot <- spread(bike_data_reduced, name, available_bikes)
bike_data_pivot <- bike_data_pivot[-c(1)]

# calculate the correlations
m <- cor(bike_data_pivot)

# plot the correlations
corrplot(m, method='color', type = 'upper')

# the above plot is very hard to get meaningful data from
# lets plot a heatmap instead
heatmap(x=m)

# the above heatmap is hectic, lets use a different heatmap package instead
# setting the color scheme - 1st the colors, then the shading
Colors=rev(brewer.pal(6,"Spectral"))
Colors=colorRampPalette(Colors)(100)

# this function auto includes a clustering algorithm which arranges our plot
heatmap.2(m, dendrogram ="row", trace="none", density.info = "density", 
          col = Colors, labCol =NA, margins=c(1,8))

# isolating Friday mornings records
bike_friday_morn <- subset(bike_data, Time_of_Call > ymd_hms("2019-08-23 06:00:00") &
                             Time_of_Call < ymd_hms("2019-08-23 10:00:00") )


# quick way of finding out the max and min times for this dataset
range(bike_friday_morn$Time_of_Call)

# again, lets focus on 1 stop
grand_canal_fri <- subset(bike_friday_morn, name == 'GRAND CANAL DOCK')

# lets create a point plot
# we can see that as the morning goes on, we can see a reduction in the number of available bikes
# also including a trend line
ggplot(data= grand_canal_fri, aes(x=Time_of_Call, y=available_bike_stands))+
      geom_point()+
      geom_smooth(method="lm")


# which stops had the lowest average available bike stands during this time period?
# this groups our data by the number & name of our stop along with the time_of_call
Stations_Status_Summary <- bike_friday_morn %>%
  group_by(number,name, Time_of_Call) %>%
  dplyr::summarise(open_stands_percent = (available_bike_stands/bike_stands)*100,
                   bikes_available_percent = (available_bikes/bike_stands)*100)

# we can then use the above output to calculate the mean for each stop using the aggregate function
mean_availability <- aggregate(Stations_Status_Summary[,4:5], 
                               list(Stations_Status_Summary$name), mean)

# now lets plot the means (bit of a messy plot because it's trying to plot all the station names)
ggplot(data= mean_availability, aes(x=Group.1, y=open_stands_percent)) + geom_point()

# something more useful might be to find the stations with the highest & lowest availability

# the only thing with this is that it doesn't give you the name of the station
tail(sort(mean_availability$open_stands_percent),5)

# we can use order to get us the headers too
lowest_mean <- tail(mean_availability[order(mean_availability$open_stands_percent),],5)
highest_mean <- head(mean_availability[order(mean_availability$open_stands_percent),],5)

# now lets graph the busiest and quietest stops over the morning
# create a dataframe with the data of the stops we want
# (I'm sure there's a quicker way of doing this BTW!)
bike_fri_reduced <- subset(bike_friday_morn, subset = (bike_friday_morn$name == "PRINCES STREET / O'CONNELL STREET"   |
                                                         bike_friday_morn$name == 'JERVIS STREET'  |
                                                         bike_friday_morn$name == 'MOLESWORTH STREET' |
                                                         bike_friday_morn$name == 'BENSON STREET' |
                                                         bike_friday_morn$name == 'HEUSTON STATION (CAR PARK)' |
                                                         bike_friday_morn$name == 'GRANTHAM STREET'   |
                                                         bike_friday_morn$name == 'GOLDEN LANE'  |
                                                         bike_friday_morn$name == 'CHARLEVILLE ROAD' |
                                                         bike_friday_morn$name == 'CHRISTCHURCH PLACE' |
                                                         bike_friday_morn$name == 'WESTERN WAY'))
# creating a plot of available bikes over the morning
p3 <- ggplot(data= bike_fri_reduced, aes(x=Time_of_Call, y=available_bikes, color=name)) + 
  geom_line(size = 1)+geom_point()

# introducing gganimate by including transition_reveal
animate_friday_morning <- p3 + transition_reveal(Time_of_Call)
animate_friday_morning

# finally, lets do this for the full dataset over the full week
Full_Stations_Status_Summary <- bike_data %>%
  group_by(number,name, Time_of_Call) %>%
  dplyr::summarise(open_stands_percent = (available_bike_stands/bike_stands)*100,
                   bikes_available_percent = (available_bikes/bike_stands)*100)
full_mean_availability <- aggregate(Full_Stations_Status_Summary[,4:5], list(Full_Stations_Status_Summary$name), 
                                    mean)
full_lowest_mean <- head(full_mean_availability[order(full_mean_availability$bikes_available_percent),],5)
full_highest_mean <- tail(full_mean_availability[order(full_mean_availability$bikes_available_percent),],5)

full_bike_reduced <- subset(bike_data, subset = (bike_data$name == "HEUSTON STATION (CENTRAL)"   |
                                                    bike_data$name == 'HEUSTON BRIDGE (NORTH)'  |
                                                    bike_data$name == 'SMITHFIELD NORTH' |
                                                    bike_data$name == 'HEUSTON BRIDGE (SOUTH)' |
                                                    bike_data$name == 'KILLARNEY STREET' |
                                                    bike_data$name == 'WESTERN WAY' |
                                                    bike_data$name == 'RATHDOWN ROAD'   |
                                                    bike_data$name == 'HARCOURT TERRACE'  |
                                                    bike_data$name == 'GRANGEGORMAN LOWER (CENTRAL)' |
                                                    bike_data$name == 'GRANGEGORMAN LOWER (SOUTH)'))

p4 <- ggplot(data= full_bike_reduced, aes(x=Time_of_Call, y=available_bikes, color=name)) + 
  geom_line(size = 1)+geom_point()

# introducing gganimate by including transition_reveal
animate_full_week <- p4 + transition_reveal(Time_of_Call)
animate_full_week

# the above plot is a little busy, so lets isolate the busiest 5

busiest_bike_reduced <- subset(bike_data, subset = (bike_data$name == 'WESTERN WAY' |
                                                   bike_data$name == 'RATHDOWN ROAD'   |
                                                   bike_data$name == 'HARCOURT TERRACE'  |
                                                   bike_data$name == 'GRANGEGORMAN LOWER (CENTRAL)' |
                                                   bike_data$name == 'GRANGEGORMAN LOWER (SOUTH)'))

p5 <- ggplot(data= busiest_bike_reduced, aes(x=Time_of_Call, y=available_bikes, color=name)) + 
  geom_line(size = 1)+geom_point()

p5
animate_busiest_week <- p5 + transition_reveal(Time_of_Call)
animate_busiest_week

# lets try a bit of clustering
# in its most simplest format, lets use kmeans clustering with only the coordinates of the stations
# isolating only the longs and lats
cluster_bike_data <- bike_data%>%select(5,6)

# setting a seed so results are repeatable
set.seed(20)

# setting 3 clusters
clusters <- kmeans(cluster_bike_data, 3)

# plotting the longs and lats in ggplot
# ggmap is available but requires an API key (details on how to do this are in a link at the bottom)
ggplot(bike_data)+
  geom_point( aes(x=position.lat, y = position.lng), 
              color=clusters$cluster)+
  ggtitle("Clustering Stations by Location - Plotting by Longitudes & Latitudes")+
  labs(x="Latitude", y = "Longitude")+  coord_flip()

# the above plot gives us 3 clear distinct areas - east, central and west Dublin city centre

# what happens when we include more data into our clustered data, how does our map change?
# longs, lats, bike stands
cluster_bike_data2 <- bike_data%>%select(5,6,9)

clusters2 <- kmeans(cluster_bike_data2, 3)
ggplot(bike_data)+
  geom_point(aes(x=position.lat, y = position.lng),color=clusters2$cluster)+
  ggtitle("Clustering Stations by Number of Bike Stands - Plotting by Longitudes & Latitudes")+
  labs(x="Latitude", y = "Longitude")+
  theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0))) +
  coord_flip()

# using different values of bike stands, we can see that our clustering 
# is now forming communities of stations based on the number of bike stands
# check this by replacing (bike_stands>=20 & bike_stands<=30) with bike_stands >30
ggplot(bike_data, aes(x=position.lat, y = position.lng, label=name))+
  geom_point(color=clusters2$cluster)+
  ggtitle("Clustering Stations by Number of Bike Stands - Plotting by Longitudes & Latitudes")+
  labs(x="Latitude", y = "Longitude")+
  theme(plot.title = element_text(size=10, face="bold",margin = margin(10, 0, 10, 0))) +
  coord_flip()+
  geom_text(aes(label=ifelse((bike_stands>=20 & bike_stands<=30), as.character(name), '')),
            hjust=0,vjust=0, size=2)


# with thanks to https://www.littlemissdata.com/blog/maps for a guide to plotting
# https://www.visibledata.co.uk/blog/2018/12/05/2018-12-05-using-ggmap-after-july-2018/



