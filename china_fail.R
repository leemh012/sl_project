# install.packages("readr")
library(dplyr)

pm10 <- read.csv("./weather/chinaint/China_Station_PM10.csv")
colnames(pm10)[1] <- "Year"
head(pm10)

pm25 <- read.csv("./weather/chinaint/China_Station_PM25.csv")
colnames(pm25)[1] <- "Year"
head(pm25)


pm10 <- pm10 %>% select(c(1,2,3,4,9))
pm25 <- pm25 %>% select(c(1,2,3,4,9))

head(pm10)

pm10 %>% group_by(Year, Month, Day) %>% summarise(pm10 = mean(X5, na.rm=TRUE))

as.data.frame(pm10)
