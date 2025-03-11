
  
library(readxl)
library(tidyverse)
install.packages("dplyr")

library(forecast)
library(dplyr)
library(ggplot2)



options(warn=-1)
trip_data <-  read_excel("/Users/yvett/OneDrive/Desktop/Datasets for Analysis/trip.xlsx")
station_data <- read.csv("/Users/yvett/OneDrive/Desktop/Datasets for Analysis/station.csv")   



#trip data 
summary(trip_data)
summary(station_data)

dplyr::glimpse(trip_data)
dplyr::glimpse(station_data)




str(trip_data)
str(station_data)


##############################requirements by Managers##########################

#1. Trips by calendar date.- done
#2. Total number of trips by day of the week. - done 
#3. Total number of trips by calendar date - weekend vs. weekday. - done 
#4. Separate plots for weekend and weekday - done
#5. Total trips by hour of the day - done 
#6. Number of trips by hour, across the year - done
#7. Usage by city - done
#8. Usage by Customers vs. Subscribers - done






#add duration in minutes
trip_data <- trip_data %>% dplyr::mutate(duration_mins = duration/60)


#add start_month to see seasonality in start months
trip_data <- trip_data %>% dplyr::mutate(start_date_month = lubridate::month(start_date, label = TRUE))

#add start_year to see trends by year
trip_data <- trip_data %>% dplyr::mutate(start_date_year = lubridate::year(start_date))

#add start_weekday to see trends by year
trip_data <- trip_data %>% dplyr::mutate(start_date_weekday = lubridate::wday(start_date, label = TRUE))

#add start_weekday or weekend to see trends by year
trip_data <- trip_data %>% dplyr::mutate(start_date_weekend_ind = ifelse( start_date_weekday == "Sat" | start_date_weekday == "Sun", "Weekend", "Weekday")) 


#add start_hour  to see trends by hour
trip_data <- trip_data %>% dplyr::mutate(start_date_hour = lubridate::hour(start_date))
trip_data$start_date_hour <- as.factor(trip_data$start_date_hour)

#add end_hour  to see trends by hour
trip_data <- trip_data %>% dplyr::mutate(end_date_hour = lubridate::hour(end_date))
trip_data$end_date_hour <- as.factor(trip_data$end_date_hour)





##By calendar date -  duration by weekday

trip_data %>% 
  dplyr::group_by(start_date_weekday) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_weekday, y =  total_duration_mins, fill = start_date_weekday )) + 
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Riding Duration during Days", x = "Frequency", y = "Day of Week")+
  scale_fill_hue(c = 40) +
  coord_flip()


##By calendar date -  duration by weekday, split by subscription_type

trip_data %>% 
  dplyr::group_by(subscription_type, start_date_weekday) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_weekday, y =  total_duration_mins, fill = start_date_weekday )) + 
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 40) +
  coord_flip() + 
  facet_grid(~subscription_type)




##By calendar date -  duration by weekend

trip_data %>% 
  dplyr::group_by(start_date_weekend_ind) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_weekend_ind, y =  total_duration_mins, fill = start_date_weekend_ind )) + 
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank())+
  scale_fill_manual(values = c("seagreen","gray"))+
  coord_flip()



##By calendar date -  duration by weekend, split by subscription_type

trip_data %>% 
  dplyr::group_by(subscription_type, start_date_weekend_ind, start_date_weekday) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_weekend_ind, y =  total_duration_mins, fill = start_date_weekend_ind )) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("mediumorchid1" , "lightsalmon")) +
  theme(panel.background = element_blank())+
  coord_flip() + 
  facet_grid(~subscription_type)













##By calendar date -  duration by hour of day

trip_data %>% 
  dplyr::group_by(start_date_hour) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_hour, y =  total_duration_mins, fill = start_date_hour )) + 
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank())+
  scale_fill_hue(c= 60)








##By calendar date -  duration by weekday, split by subscription_type

trip_data %>% 
  dplyr::group_by(subscription_type, start_date_weekday) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_weekday, y =  total_duration_mins, fill = start_date_weekday )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  coord_flip() + 
  theme(panel.background = element_blank())+
  facet_grid(~subscription_type)

























##By calendar date - number of trips by weekday
`
trip_data %>% 
  dplyr::group_by(start_date_weekday) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_weekday, y =  number_of_trips, fill = start_date_weekday )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  coord_flip()









##By calendar date - month

trip_data %>% 
  dplyr::group_by(start_date_month) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_month, y =  total_duration_mins, fill = start_date_month )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  coord_flip()



##By calendar date - by month split by year

trip_data %>% 
  dplyr::group_by(start_date_year, start_date_month) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_month, y =  total_duration_mins, fill = start_date_month )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 60) +
  coord_flip() + 
  theme(panel.background = element_blank())+
  facet_grid(~ start_date_year)









#TRIPS!!!!!

#Analysis by number of trips (each id is a trip)





##By calendar date - month

trip_data %>% 
  dplyr::group_by(start_date_month) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_month, y =  number_of_trips, fill = start_date_month )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  coord_flip()








##By calendar date -  number of trips by hour of day

trip_data %>% 
  dplyr::group_by(start_date_hour) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_hour, y =  number_of_trips, fill = start_date_hour )) + 
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 40)




##By calendar date -  number of trips by hour of day, split by which day

trip_data %>% 
  dplyr::group_by(start_date_weekday, start_date_hour) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_hour, y =  number_of_trips, fill = start_date_hour )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  facet_grid(~start_date_weekday)+
  coord_flip()



##By calendar date -  number of trips by hour of day, split by weekend or not

trip_data %>% 
  dplyr::group_by(start_date_weekend_ind, start_date_hour) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_hour, y =  number_of_trips, fill = start_date_hour )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  facet_grid(~start_date_weekend_ind)+
  theme(panel.background = element_blank())+
  coord_flip()


##By calendar date -  number of trips by hour of day, split by month 

trip_data %>% 
  dplyr::group_by(start_date_month, start_date_hour) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_hour, y =  number_of_trips, fill = start_date_hour )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  facet_grid(~start_date_month) +
  coord_flip()



##By calendar date -  number of trips by hour of day, split by year

trip_data %>% 
  dplyr::group_by(start_date_year, start_date_hour) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = start_date_hour, y =  number_of_trips, fill = start_date_hour )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  facet_grid(~start_date_year) +
  theme(panel.background = element_blank())+
  coord_flip()







trip_data %>% 
  dplyr::group_by(start_date_month) %>% 
  dplyr::summarise( count = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )

#Join the station data with trip_data
#before we take a look at the varaibles in each data 


glimpse(trip_data)
glimpse(station_data)


#Join the data - could join by name & start_station_name,  name & end_station_name

combined_data <- dplyr::left_join(trip_data, station_data, by = c("start_station_name" = "name") ) %>% dplyr::rename(id = id.x)

combined_data

glimpse(combined_data)





# Number of trips by city
combined_data %>% 
  dplyr::group_by(city) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = city, y =  number_of_trips, fill = city )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  #facet_grid(~start_date_year) +
  theme(panel.background = element_blank())
  




# Number of trips by city, split by subscription_type

combined_data %>% 
  dplyr::group_by(subscription_type, city) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = city, y =  number_of_trips, fill = city )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  facet_grid(~subscription_type) +
  theme(panel.background = element_blank())+
  coord_flip()




# Number of trips by bike id

combined_data %>% 
  dplyr::group_by(subscription_type, start_date_weekend_ind, bike_id) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = bike_id, y =  number_of_trips )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) 


combined_data %>% 
  dplyr::group_by(city, subscription_type, start_date_weekend_ind, bike_id) %>% 
  dplyr::summarise( number_of_trips = length(id), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) )  %>% 
  ggplot2::ggplot(mapping = aes( x = bike_id, y =  number_of_trips )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) +
  theme(panel.background = element_blank())+
  facet_grid(~city)



ggplot(trip_data, aes(x = subscription_type, fill = (subscription_type))) + 
  geom_bar() +
  geom_text(aes(label=..count..), stat ="count", vjust =1.5,colour = "black")+
  scale_fill_manual(values =c("lightpink","lightblue")) +
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Usage Type", x = "Type of User", y = "Frequency")

trip_data %>% 
  dplyr::group_by(start_date_weekend_ind, start_date_month) %>% 
  dplyr::summarise( trips = n(), total_duration_mins =  sum(duration_mins), avg_duration_mins = mean(duration_mins) ) 

trip_ts <- 
  trip_data %>% 
  dplyr::select(start_date, end_date, duration, bike_id, duration_mins, start_date_month) %>%
  dplyr::arrange(desc(start_date)) %>% 
  dplyr::mutate(start_date_ym = format(as.Date(start_date), "%Y-%m-%d") ) %>% 
  dplyr::group_by(start_date_ym) %>% 
  dplyr::summarise(trips = length(bike_id))


trip_ts
head(trip_ts)
tail(trip_ts)




trip_ts <- trip_ts[-(1:3) ,]
trip_ts


##forecasting

library(forecast)
library(readxl)
library(xts)
library(zoo)






trip_ts_msts  <- msts(trip_ts$trips, seasonal.periods = c(7, 356.25)) 
plot(trip_ts_msts)

#Partioning the data
nValid <- 60  #can be more than 60 but careful not to go too far away
nTrain <- length(trip_ts_msts) - nValid
train.ts <- window(trip_ts_msts, start = c(1,1), end = c(1, nTrain))
valid.ts <- window(trip_ts_msts, start = c(1, nTrain + 1), end = c(1, nTrain + nValid))



#Forecasting using TBATS
trip_ts_tbats <- tbats(train.ts)


#forecasted model
trip_ts_tbats_pred<-forecast(trip_ts_tbats, h=nValid)

#Plot of Model
plot(trip_ts_tbats_pred)

#Performance Measures
accuracy(trip_ts_tbats_pred , valid.ts)


####################USING STLM######################################
trip_ts_stlm <- stlm(trip_ts_msts , s.window = "periodic" , method = "ets")
trip_ts_stlm_pred <- forecast(trip_ts_stlm , h=nValid)
plot(trip_ts_stlm_pred)
#Performance Measures
accuracy(trip_ts_stlm_pred,valid.ts)

