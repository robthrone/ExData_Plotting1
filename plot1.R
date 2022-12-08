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

plot1 <- data %>% 
  ggplot(aes(Global_active_power, fill = "red"))+
  geom_histogram()+
  labs(title = "Global Active Power")+
  xlab("Global Active Pwr (kilowatts)")+
  ylab("Frequency")

ggsave("plot1.png", plot = plot1)
