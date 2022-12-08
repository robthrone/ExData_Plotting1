library(tidyverse)
library(lubridate)

data <- read_delim("household_power_consumption.txt")

data <- data %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(between(Date, as.Date('2007-02-01'), as.Date('2007-02-02'))) %>% 
  mutate(Global_active_power = as.integer(Global_active_power)) %>% 
  mutate(Global_reactive_power = as.integer(Global_reactive_power)) %>% 
  mutate(Voltage = as.integer(Voltage)) %>% 
  mutate(Global_intensity = as.integer(Global_intensity))
# specifying the format
format <- "%Y-%m-%d %H:%M:%S"

data <- data %>% 
  mutate(dat_time = as.POSIXct(paste(Date, Time), format=format)) %>% 
  mutate(dayz = wday(dat_time, label = TRUE))
 
plot2 <- data %>% 
  ggplot(aes(dat_time, Global_active_power))+
  geom_line()+
  ylab("Global Active Pwr (kilowatts)")+
  xlab("")+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%a")
  

ggsave("plot2.png", plot = plot2)

