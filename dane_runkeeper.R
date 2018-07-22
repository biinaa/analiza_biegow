# packages ---------------------------------------------------------------------
install.packages("fpc") 
install.packages("plyr") 
install.packages("dplyr") 
install.packages("mapproj")
install.packages("plotKML")
install.packages("lubridate")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
install.packages("plotly")

library("fpc") 
library("plyr") 
library("dplyr") 
library("mapproj")
library("plotKML")
library("lubridate")
library("leaflet")
library("ggplot2")
library("ggthemes")
library("scales")
library("plotly")

# data from runkeeper ----------------------------------------------------------
unzip('dane/runkeeper-data-export-30874753-2016-09-03-1708.zip',
      exdir = 'dane/.')

files <- dir('dane/.', pattern = "\\.gpx")

# variables
index <- c()
latitude <- c()
longitude <- c()
file <- c()

k <- 1

# loop
for (i in 1:length(files)) {
    curr_route <- readGPX(paste('dane/', files[i], sep = ''))
    
    for (j in curr_route$tracks[[1]]) {
        location <- j
        file <- c(file, rep(paste('dane/', files[i], sep = ''), dim(location)[1])) 
        index <- c(index, rep(k, dim(location)[1]))
        latitude <- c(latitude, location$lat)
        longitude <- c(longitude, location$lon)
        k <- k + 1
    }
    
}

routes <- data.frame(cbind(index, latitude, longitude, file))
rm(i, j, k, index, latitude, longitude, file, curr_route)


# data types
routes$file <- as.character(routes$file)
routes$latitude <- as.numeric(levels(routes$latitude)[routes$latitude])
routes$longitude <- as.numeric(levels(routes$longitude)[routes$longitude])
routes <- transform(routes, index = as.numeric(index))

str(routes)

# load meta data
meta_data <- read.csv("dane/cardioActivities.csv", stringsAsFactors=FALSE)
meta_data <- plyr::rename(meta_data, replace = c("GPX.File" = "file"))
meta_data$file <- paste('dane/', meta_data$file, sep = '')

# bind routes, select columns and filter data
routes_meta <- left_join(routes, meta_data, by="file") %>%
    arrange(index) %>%
    filter(Type == 'Running') %>% # only running activities
    select(index, latitude, longitude, file, Date, Distance..km., Duration, Average.Pace,
           Average.Speed..km.h., Calories.Burned, Climb..m.) # select columns

routes_meta$Date <- ymd_hms(routes_meta$Date)

# data are collected from 2 different cities - set centers

clusters <- pamk(routes_meta[,c("latitude", "longitude")], krange=2:20, diss=T, usepam=F)$pamobject$medoids

lat_range <- clusters[1,][1] + rnorm(20, sd=0.1)
lon_range <-clusters[1,][2] + rnorm(20, sd=0.1)

routes_meta <- routes_meta %>%
    mutate(City = if_else(
        (latitude > min(lat_range) & latitude < max(lat_range) & 
        longitude > min(lon_range) &  longitude < max(lon_range)),
        'poz',
        'wwa'),
        YEARMODA = as.Date(Date))

# get historical weather data
source('dane_pogoda.R')

routes_meta_tmp <- left_join(routes_meta, pogoda_poz, by = "YEARMODA") %>%
    filter(City == 'poz') %>%
    select(-STN_NAME, -YEARMODA)

routes_meta_tmp2 <- left_join(routes_meta, pogoda_wwa, by = "YEARMODA") %>%
    filter(City == 'wwa') %>%
    select(-STN_NAME, -YEARMODA)

routes_meta <- bind_rows(routes_meta_tmp, routes_meta_tmp2)

# plots ------------------------------------------------------------------------

# dataset for plots

routes_meta_plots <- routes_meta %>%
    select(index, Date, Distance..km., Duration, Average.Pace,
           Calories.Burned, Climb..m., City, TEMP) %>%
    unique() %>%
    mutate(Hour = hour(Date), 
           WeekDay = wday(Date, week_start = 1, label=TRUE), 
           AvgPace = parse_date_time(ms(Average.Pace), "MS"),
           Temp2 = cut(TEMP, seq(-5,30,5))) # extra columns for plots

# weekly running habbits

weekly_plot <- ggplot(routes_meta_plots, aes(WeekDay, label = ..count..)) +
    geom_bar(fill = "#377eb8") +
    xlab("dzień tygodnia") +
    ylab("liczba biegów") +
    theme_hc()

ggplotly(weekly_plot)

# runs by start hour

hour_plot <- ggplot(routes_meta_plots, aes(Hour, label = ..count..)) +
    geom_bar(fill = "#377eb8") +
    xlab("godzina treningu") +
    ylab("liczba biegów") +
    theme_hc()

ggplotly(hour_plot)

# pace plot

pace_plot <- ggplot(routes_meta_plots, aes(Date, AvgPace, label = Average.Pace, label2 = Date)) +
    geom_line(colour = "#377eb8", size = 0.8) + 
    theme_hc() + 
    xlab("") +
    ylab("tempo [MM:SS]") +
    scale_y_datetime(labels = date_format("%M:%S"))

ggplotly(pace_plot, tooltip = c("label", "label2"))

# plot routes

r <- nrow(clusters)

for (i in 1:r) {
    lat_range <- clusters[r,][1] + rnorm(20, sd=0.1)
    lon_range <-clusters[r,][2] + rnorm(20, sd=0.1)
    setroutes <- routes_meta %>% 
                    filter((latitude > min(lat_range) & latitude < max(lat_range)),
                           longitude > min(lon_range) &  longitude < max(lon_range))

    leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolylines(lat = setroutes$latitude, lng = setroutes$longitude, weight = 1)
}

# weather chart

weather_plot <- ggplot(routes_meta_plots, aes(Temp2, AvgPace, label = Average.Pace)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "#377eb8") +
    theme_hc() +
    xlab("temperatura") +
    ylab("tempo [MM:SS]") +
    scale_y_datetime(labels = date_format("%M:%S"))

weather_plotly <- ggplotly(weather_plot, tooltip = NULL)

layout(weather_plotly, hovermode = FALSE)


# With the hover in the original plot:
print(obj)



save(routes_meta_plots, routes_meta, file = "dane_shiny.RData")




