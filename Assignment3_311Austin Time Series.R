


library(tidyverse)
library(stringr)
library(lubridate)
library(leaflet)
library(DT)
library(forecast)
library(lubridate)

#read the data

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

Austin311 = read_csv(file.choose())

#review the data

print(head(Austin311), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#CommonComplaint
Austin311 %>%
  group_by(complaint_description) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(complaint_description = reorder(complaint_description,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = complaint_description,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = complaint_description, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Complaint', 
       y = 'Count', 
       title = 'Complaint and Count') +
  coord_flip() + 
  theme_bw()

#ActiveCounty
Austin311 %>%
  group_by(county) %>%
  filter(!is.na(county)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(county = reorder(county,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = county,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = county, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'County', 
       y = 'Count', 
       title = 'County and Count') +
  coord_flip() + 
  theme_bw()

#OwningDept

Austin311 %>%
  group_by(owning_department) %>%
  filter(!is.na(owning_department)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(owning_department = reorder(owning_department,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = owning_department,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = owning_department, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Owning Department', 
       y = 'Count', 
       title = 'Owning Department and Count') +
  coord_flip() + 
  theme_bw()

#Source

Austin311 %>%
  group_by(source) %>%
  filter(!is.na(source)) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(source = reorder(source,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = source,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = source, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Source', 
       y = 'Count', 
       title = 'Source and Count') +
  coord_flip() + 
  theme_bw()

#Time/Day/Month

max(Austin311$created_date,na.rm = TRUE)

min(Austin311$created_date,na.rm = TRUE)

#trend
Austin311 %>%
  mutate(year = year(ymd_hms(created_date))) %>%
  mutate(month = month(ymd_hms(created_date))) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  arrange(year,month) %>%
  mutate(YearMonth = make_date(year=year,month=month) ) %>%
  
  
  ggplot(aes(x=YearMonth,y=Count,group = 1)) +
  geom_line(size=1, color="red")+
  geom_point(size=3, color="red") +
  labs(x = 'Time', y = 'Count',title = 'Trend of 311 Calls') +
  theme_bw() 

#BusyMonth
Austin311 %>%
  mutate(month = month.abb[month(ymd_hms(created_date))]) %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(month = reorder(month,Count)) %>%
  
  ggplot(aes(x = month,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = month, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Month', 
       y = 'Count', 
       title = 'Month and Count') +
  coord_flip() + 
  theme_bw()

#BusiestDay
GetTop10BusyDays = function(Austin311)
{
  
  Austin311 = Austin311 %>%
    mutate(day = day(ymd_hms(Austin311$created_date)))
  
  Austin311 %>%
    filter(!is.na(day)) %>%
    group_by(day) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(day = reorder(day,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = day,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = day, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'day', 
         y = 'Count', 
         title = 'day and Count') +
    coord_flip() + 
    theme_bw()
}

GetTop10BusyDays(Austin311)


#leastBusyDay

GetLeast10BusyDays = function(Austin311)
{
  
  Austin311 = Austin311 %>%
    mutate(day = day(ymd_hms(Austin311$created_date)))
  
  Austin311 %>%
    filter(!is.na(day)) %>%
    group_by(day) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(day = reorder(day,Count)) %>%
    tail(10) %>%
    
    ggplot(aes(x = day,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = day, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'day', 
         y = 'Count', 
         title = 'day and Count') +
    coord_flip() + 
    theme_bw()
}

GetLeast10BusyDays(Austin311)

#DayOfTheWeek 

Austin311$dayOfWeek = wday(Austin311$created_date, label = TRUE)

GetDayOfWeekAnalysis = function(Austin311)
{
  Austin311 %>%
    filter(!is.na(dayOfWeek)) %>%
    group_by(dayOfWeek) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(dayOfWeek = reorder(dayOfWeek,Count)) %>%
    
    
    ggplot(aes(x = dayOfWeek,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = dayOfWeek, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'dayOfWeek', 
         y = 'Count', 
         title = 'dayOfWeek and Count') +
    coord_flip() + 
    theme_bw()
}

GetDayOfWeekAnalysis(Austin311)


#BusyHour
Austin311$hour = hour(Austin311$created_date)

GetTop10BusyHours = function(Austin311)
{
  Austin311 %>%
    filter(!is.na(hour)) %>%
    group_by(hour) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(hour = reorder(hour,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = hour,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor2) +
    geom_text(aes(x = hour, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'hour', 
         y = 'Count', 
         title = 'hour and Count') +
    coord_flip() + 
    theme_bw()
  
}

GetTop10BusyHours(Austin311)

#LeastBusyHour
GetLeast10BusyHrs = function(Austin311)
{
  Austin311 %>%
    filter(!is.na(hour)) %>%
    group_by(hour) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(hour = reorder(hour,Count)) %>%
    tail(10) %>%
    
    ggplot(aes(x = hour,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = hour, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'hour', 
         y = 'Count', 
         title = 'hour and Count') +
    coord_flip() + 
    theme_bw()
  
}

GetLeast10BusyHrs(Austin311)


#DistributionOfCalls

center_lon = median(Austin311$longitude,na.rm = TRUE)
center_lat = median(Austin311$latitude,na.rm = TRUE)

Austin311SampleAll = Austin311 %>% sample_n(50e3) %>%
  filter(!is.na(latitude) ) %>%
  filter(!is.na(longitude)) 

#Animal Services Office

Austin311SampleAnimalServices = Austin311 %>% sample_n(50e3) %>%
  filter(owning_department == 'Animal Services Office') %>%
  filter(!is.na(latitude) ) %>%
  filter(!is.na(longitude)) 

leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  
  addCircles(data = Austin311SampleAll,lng = ~longitude, lat = ~latitude, 
             color = ~c("red"), group ="All")  %>%
  
  addCircles(data = Austin311SampleAnimalServices,lng = ~longitude, lat = ~latitude, 
             color = ~c("blue"), group ="AnimalServices")  %>%
  
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 13) %>%
  addLayersControl(
    baseGroups =  c("All","AnimalServices"),
    options = layersControlOptions(collapsed = FALSE)
  ) 



#ClusteringOfCalls

Austin311SampleAll %>% leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>% 
  
  addMarkers(lng = ~longitude, lat = ~latitude,clusterOptions = markerClusterOptions()) %>%
  # controls
  
  setView(lng=center_lon, lat=center_lat,zoom = 12) 


#TimeSeriesInMonths

Austin311TrendData = Austin311 %>%
  mutate(year = year(ymd_hms(created_date))) %>%
  mutate(month = month(ymd_hms(created_date))) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  arrange(year,month)

tsAustin311TrendData = ts(Austin311TrendData)

datatable((tsAustin311TrendData), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#PredictionUsingArima

fit <- auto.arima(tsAustin311TrendData[1:40,3])

preds = forecast(fit, h = 5)

preds %>% autoplot(include=40) +theme_bw()

#preditionTest
predictions = as.numeric(preds$mean)

cat("\n","The predictions are  ",predictions)

#predictionError
error = sqrt( mean( (tsAustin311TrendData[41:45,3] - predictions)^2))

cat("\n","The RMSE is ", error)

#TimeSeriesInWeeks
Austin311TrendData = Austin311 %>%
  mutate(year = year(ymd_hms(created_date))) %>%
  mutate(WeekNo = week(ymd_hms(created_date))) %>%
  
  filter(!is.na(year)) %>%
  filter(!is.na(WeekNo)) %>%
  group_by(year,WeekNo) %>%
  summarise(Count = n()) %>%
  arrange(year,WeekNo)

tsAustin311TrendData = ts(Austin311TrendData)

datatable((tsAustin311TrendData), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#PredictionUsingArima
fit <- auto.arima(tsAustin311TrendData[1:188,3])

preds = forecast(fit, h = 5)

preds %>% autoplot(include=188) +theme_bw()

#PredictionTest
predictions = as.numeric(preds$mean)

cat("\n","The predictions are  ",predictions)

#PredictionError
error = sqrt( mean( (tsAustin311TrendData[189:193,3] - predictions)^2))

cat("\n","The RMSE is ", error)


#PredictionUsingETS

fit <- ets(tsAustin311TrendData[1:188,3])

preds = forecast(fit, h = 5)

preds %>% autoplot(include=188) +theme_bw()

#PredictionTest
predictions = as.numeric(preds$mean)

cat("\n","The predictions are  ",predictions)

#PredictionError
error = sqrt( mean( (tsAustin311TrendData[189:193,3] - predictions)^2))

cat("\n","The RMSE is ", error)
