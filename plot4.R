library(tidyverse)
library(lubridate)
library(patchwork)

data <- read_delim("household_power_consumption.txt")

data <- data %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(between(Date, as.Date('2007-02-01'), as.Date('2007-02-02'))) %>% 
  mutate(Global_active_power = as.integer(Global_active_power)) %>% 
  mutate(Global_reactive_power = as.double(Global_reactive_power)) %>% 
  mutate(Voltage = as.integer(Voltage)) %>% 
  mutate(Global_intensity = as.integer(Global_intensity)) %>% 
  mutate(Sub_metering_1 = as.integer(Sub_metering_1)) %>% 
  mutate(Sub_metering_2 = as.integer(Sub_metering_2))
# specifying the format
format <- "%Y-%m-%d %H:%M:%S"

data <- data %>% 
  mutate(dat_time = as.POSIXct(paste(Date, Time), format=format)) %>% 
  mutate(dayz = wday(dat_time, label = TRUE))

first_plot <- data %>% 
  ggplot(aes(dat_time, Global_active_power))+
  geom_line()+
  ylab("Global Active Pwr (kilowatts)")+
  xlab("")+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%a")

third_plot <- data %>% 
  pivot_longer(Sub_metering_1:Sub_metering_3, names_to = "subs", values_to = "value") %>% 
  ggplot(aes(dat_time, value, color = subs))+
  geom_line()+
  scale_color_manual(values=c("Sub_metering_1"="black","Sub_metering_2"="red","Sub_metering_3"="blue")) +
  ylab("Energy Sub metering")+
  xlab("")+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%a")+
  theme_classic()+
  theme(legend.title= element_blank())

second_plot <- data %>% 
  ggplot(aes(dat_time, Voltage))+
  geom_line()+
  ylab("Voltage")+
  xlab("datetime")+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%a")

fourth_plot <- data %>% 
  ggplot(aes(dat_time, Global_reactive_power))+
  geom_line()+
  xlab("datetime")+
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2 , 0.3, 0.4, 0.5))+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%a")

full_plot <- first_plot+second_plot+third_plot+fourth_plot

ggsave("plot4.png", plot = full_plot)
