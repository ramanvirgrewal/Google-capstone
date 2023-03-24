.libPaths("C:/Users/Computer User 1/AppData/Local/R/win-library/4.2")
rm(list = ls())
gc()
install.packages(c("tidyverse", "lubridate", "data table"))

getwd()
library(tidyverse)
library(lubridate)
library(data.table)
# library(data.table)
setwd("C:/Users/Computer User 1/Documents/GitHub/Ramanvir/Google-capstone")

local_folder1 <- getwd()

in_folder  <- file.path(local_folder1, 'in_folder')
out_folder <- paste(local_folder1, 'out_folder', sep = '/')
trips_jan_2022 <- fread(file.path(in_folder,"202201-divvy-tripdata.csv"))
trips_feb_2022 <- fread(file.path(in_folder,"202202-divvy-tripdata.csv"))
trips_mar_2022 <- fread(file.path(in_folder,"202203-divvy-tripdata.csv"))
trips_apr_2022 <- fread(file.path(in_folder,"202204-divvy-tripdata.csv"))
trips_may_2022 <- fread(file.path(in_folder,"202205-divvy-tripdata.csv"))
trips_jun_2022 <- fread(file.path(in_folder,"202206-divvy-tripdata.csv"))
trips_jul_2022 <- fread(file.path(in_folder,"202207-divvy-tripdata.csv"))
trips_aug_2022 <- fread(file.path(in_folder,"202208-divvy-tripdata.csv"))
trips_sep_2022 <- fread(file.path(in_folder,"202209-divvy-tripdata.csv"))
trips_oct_2022 <- fread(file.path(in_folder,"202210-divvy-tripdata.csv"))
trips_nov_2022 <- fread(file.path(in_folder,"202211-divvy-tripdata.csv"))
trips_dec_2022 <- fread(file.path(in_folder,"202212-divvy-tripdata.csv"))

trips_merged<-rbind(trips_jan_2022,trips_feb_2022,trips_mar_2022, trips_apr_2022, trips_may_2022,
                    trips_jun_2022, trips_jul_2022, trips_aug_2022, trips_sep_2022, trips_oct_2022,
                    trips_nov_2022, trips_dec_2022)

# which(is.na(modified_trips$ride_length))
# which(is.na(modified_trips$week_day))
# 
# sum(is.na(trips_merged))
# 
# View(trips_merged)

trips_merged<-trips_merged %>% 
  mutate(ride_length=ended_at-started_at, week_day=lubridate::wday(started_at, label=TRUE))

#making new ride lenght column in hours
trips_merged <- trips_merged %>% 
  mutate(ride_length_hours=difftime(ended_at, started_at, units = "hours"))

# glimpse(trips_merged)

# , label=TRUE
#double checking whether week starts on Monday or Sunday, 
# as in which of the two days is denoted by number 1

trips_merged<-trips_merged %>% 
  mutate(week_day_num=lubridate::wday(started_at))

#so found out that week starts on Sunday

# trips_merged$new_week_day<-weekdays(as.Date(trips_merged$started_at))
#   
# wday(trips_merged$started_at, label=TRUE)

# arranged_trips<-trips_merged %>% arrange(-ride_length)
#checking the number of rows with ride_length being less than or equal to zero 
# length(which(trips_merged$ride_length == 0))
length(which(trips_merged$ride_length < 0))
# glimpse(trips_merged)

selected_trips<-trips_merged %>% 
  filter(ride_length>0) 

# min(selected_trips$ride_length)

selected_trips %>% 
  group_by(member_casual) %>% 
  summarize(mean_ride_length=mean(ride_length_hours)) 

# ?wday  
# 
# length(which(wday(selected_trips$ended_at)>wday(selected_trips$started_at)))
# 
# which(trips_merged$ended_at>trips_merged$started_at)
# which(trips_merged$ended_at<trips_merged$started_at)
# which(trips_merged$ended_at==trips_merged$started_at)

#finding out if there are any trips with negative ride_length
# experiment_trips<-trips_merged %>% 
#   filter(ride_length<0)
# 
# experiment_trips<-trips_merged %>% 
#   filter(ended_at<started_at)

#checking if there are any trips where the end date is not the same as the start date
 

selected_trips<-selected_trips %>% 
  mutate(weekend_weekday = case_when(week_day_num == 1 | week_day_num == 7 ~ 'weekend',
                                     week_day_num == 6 | week_day_num ==2 | week_day_num==3 | 
                                       week_day_num ==4 | week_day_num==5 ~ 'weekday'))

# max(selected_trips$ride_length)

#to have numeral in month column
selected_trips<-selected_trips%>%
  mutate(month=month(started_at))
# head(selected_trips_new)

#to have characters in month column
#selected_trips_new%>%
 # mutate(month=month(started_at, label = TRUE)) 
# selected_trips_new<-selected_trips_new%>%
#   mutate(month_yr=format(as.Date(started_at), "%Y-%m"))
# selected_trips_new%>% 
#   mutate(started_at=ymd(started_at),
#          month_yr = format_ISO8601(started_at, precision = "ym"))

#setting date column
selected_trips<-selected_trips %>% 
  mutate(month_amended=as.Date(paste0("2022-", sprintf("%02d", month), "-01")))
#setting date column without the day

selected_trips<-selected_trips%>%
  mutate(month_year=format(as.Date(started_at), "%Y-%m"))
           
           
# ?as.Date
#   
# glimpse(selected_trips_new)

#generating specific data frames for plots
df_plot1<-selected_trips %>% 
 group_by(month_amended, member_casual) %>% drop_na() %>% 
  summarize(Monthly_travel=sum(ride_length_hours))  

df_plot2<-selected_trips %>% 
  group_by(month_amended, member_casual, weekend_weekday) %>% drop_na() %>% 
  summarize(Monthly_travel=sum(ride_length_hours))

mindate<-min(selected_trips$month_year)
maxdate<-max(selected_trips$month_year)
# glimpse(mindate)
# glimpse(maxdate)

#studying the numbers
# df_month <- selected_trips %>% 
#   group_by(month) %>% drop_na() %>% 
#   summarize(Monthly_travel=sum(ride_length_hours))
# 
# df_month_weeekend_weekday <- selected_trips %>% 
#   group_by(month, weekend_weekday) %>% drop_na() %>% 
#   summarize(Monthly_travel=sum(ride_length_hours))
# 
# df_month_member_casual <- selected_trips %>% 
#   group_by(month, member_casual) %>% drop_na() %>% 
#   summarize(Monthly_travel=sum(ride_length_hours))

# df_plot2<-selected_trips %>%
#   group_by(month, member_casual, weekend_weekday) %>% drop_na() %>%
#   summarize(Monthly_travel=sum(ride_length_hours)

# hours(df_plot2$Monthly_travel_hrs)

#trying to convert seconds into hours

# df_plot3<-selected_trips %>% group_by(month, member_casual, weekend_weekday) %>% drop_na() %>% 
#     summarize(Monthly_travel_hrs=dhours(sum(ride_length)))
  

# df_plot2 <- gsub("secs", "", df_plot2$Monthly_travel_hrs)
# glimpse(df_plot2)

# df_plot2 %>% mutate(Monthly_travel_hours = hour(seconds_to_period(Monthly_travel_hrs)))
                    
                    
# Data %>%mutate(Hours = hour(seconds_to_period(Time)),
#   mutate(Hours = hour(seconds_to_period(Time)),
#          Minutes = minute(seconds_to_period(Time))) %>%
#   select(Hours, Minutes, ColA, ColB)
# df1$x1<-gsub("1","",as.character(df1$x1))



#plotting
ggplot(data=df_plot1) + 
  geom_smooth(aes(x=month_amended, y=Monthly_travel, color=member_casual))+
  labs(title="Analysis of Cyclistic Ridership", 
       subtitle=paste("Casual riders travel more than annual members of the bike-share company."), 
       caption = paste("Data from:", mindate, "to", maxdate),
       x="Month", y="Travel Time in Hours")

ggplot(data=df_plot1) + 
  geom_point(aes(x=month_amended, y=Monthly_travel, color=member_casual))


#to save the plot
ggsave("monthly_travel.png",
              width = 12,
            height = 8)

ggsave("monthly_travel_geompoint.png",
       width = 12,
       height = 8)
       
ggplot(data=df_plot2)+ 
  geom_smooth(aes(x=month_amended, y=Monthly_travel, color=member_casual))+
  facet_wrap(~weekend_weekday)+
  labs(title="Analysis of Cyclistic Ridership", 
       subtitle=paste("Casual riders travel more than annual members of the bike-share company."), 
                                   caption = paste("Data from:", mindate, "to", maxdate),
                                   x="Month", y="Travel Time in Hours")

# ggplot(data=df_plot2) + 
#   geom_point(aes(x=month, y=Monthly_travel, color=member_casual))+
#   facet_wrap(~weekend_weekday)

#to save the plot
ggsave("monthly_travel_weekends.png",
       width = 12,
       height = 8)


# ggsave("monthly_travel_weekends_geompoint.png",
#        width = 12,
#        height = 8)
