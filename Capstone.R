.libPaths("C:/Users/Computer User 1/AppData/Local/R/win-library/4.2")
rm(list = ls())
gc()
install.packages(c("tidyverse", "data.table"))

library(tidyverse)
library(lubridate)
library(data.table)
setwd("C:/Users/Computer User 1/OneDrive/Documents/Google certificate/Google-capstone")

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
  mutate(ride_length=ended_at-started_at, week_day=wday(started_at))

# arranged_trips<-trips_merged %>% arrange(-ride_length)
#checking the number of rows with ride_length being less than or equal to zero 
# length(which(arranged_trips$ride_length == 0))

selected_trips<-trips_merged %>% 
  filter(ride_length>0) 

# min(selected_trips$ride_length)

selected_trips %>% 
  group_by(member_casual) %>% 
  summarize(mean_ride_length=mean(ride_length)) 

# ?wday  
# 
# length(which(wday(selected_trips$ended_at)>wday(selected_trips$started_at)))
# 
# which(selected_trips$ended_at>selected_trips$started_at)

selected_trips<-selected_trips %>% 
  mutate(weekend_weekday = case_when(week_day == 6 | week_day == 7 ~ 'weekend',
                        week_day == 1 | week_day ==2 | week_day==3 | 
                          week_day ==4 | week_day==5 ~ 'weekday'))

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
  mutate(month=as.Date(paste0("2022-", sprintf("%02d", month), "-01")))

# ?as.Date
#   
# glimpse(selected_trips_new)

#generating specific data frames for plots
df_plot1<-selected_trips %>% 
 group_by(month, member_casual) %>% drop_na() %>% 
  summarize(Monthly_travel=sum(ride_length))  

df_plot2<-selected_trips %>% 
  group_by(month, member_casual, weekend_weekday) %>% drop_na() %>% 
  summarize(Monthly_travel_hrs=sum(ride_length)/3600)  


#plotting
ggplot(data=df_plot1) + 
  geom_smooth(aes(x=month, y=Monthly_travel, color=member_casual))

#to save the plot
ggsave("monthly_travel.png",
              width = 12,
            height = 8)
       
ggplot(data=df_plot2) + 
  geom_smooth(aes(x=month, y=Monthly_travel_hrs, color=member_casual))+
  facet_wrap(~weekend_weekday)

#to save the plot
ggsave("monthly_travel_weekends.png",
       width = 12,
       height = 8)
