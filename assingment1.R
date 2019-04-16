library(dplyr)
library(ggplot2)
library(e1071)
library(zoo)
library(TTR)
# install.packages("caret")
library(caret)
library(nnet)
# install.packages("tidyquant")
# library(tidyquant)
getwd()
setwd("C:/Users/LeeMH/Desktop/데이터")
weather <- read.csv("./weather/airquality/final_data5.csv")
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")
weather <- select(weather, -seq(30,34))

summary(weather)
str(weather)

# factor 변수 factor화 최대.풍속.풍향, 최대.풍속.풍향(16 방)
weather$최대.순간.풍속.풍향 <- as.factor(weather$최대.순간.풍속.풍향)
weather$최대.풍속.풍향 <- as.factor(weather$최대.풍속.풍향)

levels(weather$최대.순간.풍속.풍향)
levels(weather$최대.풍속.풍향)
# 풍정합 일정시간내에 이동한 공기의 취주거
# 결측치 제거(강수나 적설량은 결측치를 0으로 바꿈
weather[which(is.na(weather$강수.계속시간)), '강수.계속시간'] <- 0
weather[which(is.na(weather$일강수량)), '일강수량'] <- 0
weather[which(is.na(weather$일.최심적설)), '일.최심적설'] <- 0
weather[which(is.na(weather$합계.대형증발량)), '합계.대형증발량'] <- 0
weather[which(is.na(weather$안개.계속시간)), '안개.계속시간'] <- 0
summary(weather)
dim(weather)

weather %>% ggplot(aes(date, 최고기온 )) +
  geom_line(color='blue') +
  geom_line(aes(date, 평균.이슬점온도), color='#00e9ff') +
  ylab("최고 기온 & 이슬점 온도")

weather %>%
  ggplot(aes(date, 최대.순간.풍속)) +
  geom_line(color="blue") + geom_line(aes(date, 최대.풍속), color = "#00e9ff") +
  geom_line(aes(date, 평균.풍속 ), color = "#d204f7") + ylab("풍속")

weather %>% ggplot(aes(date, 풍정합)) + geom_line()

weather %>% ggplot(aes(date, 최소.상대습도)) + geom_line(color='#00e9ff') + geom_line(aes(date, 평균.상대습도), color = "blue")

weather$months <- as.factor(months((weather$date)))
weather$years <-  as.factor(substr(weather$date, 1,4))

colnames(weather)

mean_max_temp <- weather %>% group_by(years,months) %>% summarise(Max_temp_mean = mean(최고기온, na.rm = TRUE))
weather[which(is.na(weather$최고기온)), '최고기온'] <- as.vector(unlist(mean_max_temp[which(mean_max_temp$years == 2017 & mean_max_temp$months == "10월"), 'Max_temp_mean']))

mean_max_m_ws <- weather %>% group_by(years,months) %>% summarise(ws_m_mean = mean(최대.순간.풍속, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$최대.순간.풍속)), c('date', '최대.순간.풍속')])[1]
for (i in seq(numofNA)) {
  print(i)
  year <- as.character(weather[which(is.na(weather$최대.순간.풍속)), c('years','months', '최대.순간.풍속')][1,1])
  print(year)
  month <- as.character(weather[which(is.na(weather$최대.순간.풍속)), c('years','months', '최대.순간.풍속')][1,2])
  print(month)
  weather[which(is.na(weather$최대.순간.풍속)), '최대.순간.풍속'][1] <- as.vector(unlist(mean_max_m_ws[which(mean_max_m_ws$years == year & mean_max_m_ws$months == month), 'ws_m_mean']))
  print(as.vector(unlist(mean_max_m_ws[which(mean_max_m_ws$years == year & mean_max_m_ws$months == month), 'ws_m_mean'])))
}

weather[which(is.na(weather$최대.순간.풍속)), ]

mean_max_ws <- weather %>% group_by(years,months) %>% summarise(ws_mean = mean(최대.풍속, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$최대.풍속)), c('date', '최대.풍속')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$최대.풍속)), c('years','months', '최대.풍속')][1,1]
  month <- weather[which(is.na(weather$최대.풍속)), c('years','months', '최대.풍속')][1,2]
  weather[which(is.na(weather$최대.풍속)), '최대.풍속'][1]  <- as.vector(unlist(mean_max_ws[which(mean_max_ws$years == year & mean_max_ws$months == month), 'ws_mean']))
}

mean_avg_ws <- weather %>% group_by(years,months) %>% summarise(ws_avg_mean = mean(평균.풍속, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$평균.풍속)), c('date', '평균.풍속')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$평균.풍속)), c('years','months', '평균.풍속')][1,1]
  month <- weather[which(is.na(weather$평균.풍속)), c('years','months', '평균.풍속')][1,2]
  weather[which(is.na(weather$평균.풍속)), '평균.풍속'][1]  <- as.vector(unlist(mean_avg_ws[which(mean_avg_ws$years == year & mean_avg_ws$months == month), 'ws_avg_mean']))
}

mean_wind_watch <- weather %>% group_by(years,months) %>% summarise(ww_mean = mean(풍정합, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$풍정합)), c('date', '풍정합')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$풍정합)), c('years','months', '풍정합')][1,1]
  month <- weather[which(is.na(weather$풍정합)), c('years','months', '풍정합')][1,2]
  weather[which(is.na(weather$풍정합)), '풍정합'][1]  <- as.vector(unlist(mean_wind_watch[which(mean_wind_watch$years == year & mean_wind_watch$months == month), 'ww_mean']))
}

mean_avg_dt <- weather %>% group_by(years,months) %>% summarise(avg_dt = mean(평균.이슬점온도, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$평균.이슬점온도)), c('date', '평균.이슬점온도')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$평균.이슬점온도)), c('years','months', '평균.이슬점온도')][1,1]
  month <- weather[which(is.na(weather$평균.이슬점온도)), c('years','months', '평균.이슬점온도')][1,2]
  weather[which(is.na(weather$평균.이슬점온도)), '평균.이슬점온도'][1]  <- as.vector(unlist(mean_avg_dt[which(mean_avg_dt$years == year & mean_avg_dt$months == month), 'avg_dt']))
}

min_humidity <- weather %>% group_by(years,months) %>% summarise(min_rh = mean(최소.상대습도, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$최소.상대습도)), c('date', '최소.상대습도')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$최소.상대습도)), c('years','months', '최소.상대습도')][1,1]
  month <- weather[which(is.na(weather$최소.상대습도)), c('years','months', '최소.상대습도')][1,2]
  weather[which(is.na(weather$최소.상대습도)), '최소.상대습도'][1]  <- as.vector(unlist(min_humidity[which(min_humidity$years == year & min_humidity$months == month), 'min_rh']))
}

sum_sh <- weather %>% group_by(years,months) %>% summarise(sum_sh = mean(합계.일조.시간, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$합계.일조.시간)), c('date', '합계.일조.시간')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$합계.일조.시간)), c('years','months', '합계.일조.시간')][1,1]
  month <- weather[which(is.na(weather$합계.일조.시간)), c('years','months', '합계.일조.시간')][1,2]
  weather[which(is.na(weather$합계.일조.시간)), '합계.일조.시간'][1]  <- as.vector(unlist(sum_sh[which(sum_sh$years == year & sum_sh$months == month), 'sum_sh']))
}

#시간당.최다일사량
timely_max_irradiation  <- weather %>% group_by(years,months) %>% summarise(t_max_i = mean(시간당.최다일사량, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$시간당.최다일사량)), c('date', '시간당.최다일사량')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$시간당.최다일사량)), c('years','months', '시간당.최다일사량')][1,1]
  month <- weather[which(is.na(weather$시간당.최다일사량)), c('years','months', '시간당.최다일사량')][1,2]
  weather[which(is.na(weather$시간당.최다일사량)), '시간당.최다일사량'][1]  <- as.vector(unlist(timely_max_irradiation[which(timely_max_irradiation$years == year & timely_max_irradiation$months == month), 't_max_i']))
}

#합계.일사
sum_irradiation  <- weather %>% group_by(years,months) %>% summarise(sum_irr = mean(합계.일사, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$합계.일사)), c('date', '풍정합')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$합계.일사)), c('years','months', '합계.일사')][1,1]
  month <- weather[which(is.na(weather$합계.일사)), c('years','months', '합계.일사')][1,2]
  weather[which(is.na(weather$합계.일사)), '합계.일사'][1]  <- as.vector(unlist(sum_irradiation[which(sum_irradiation$years == year & sum_irradiation$months == month), 'sum_irr']))
}

group_NO2 <- weather %>% group_by(years,months) %>% summarise(no2 = mean(NO2, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$NO2)), c('date', 'NO2')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$NO2)), c('years','months', 'NO2')][1,1]
  month <- weather[which(is.na(weather$NO2)), c('years','months', 'NO2')][1,2]
  weather[which(is.na(weather$NO2)), 'NO2'][1]  <- as.vector(unlist(group_NO2[which(group_NO2$years == year & group_NO2$months == month), 'no2']))
}

group_CO <- weather %>% group_by(years,months) %>% summarise(co = mean(CO, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$CO)), c('date', 'CO')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$CO)), c('years','months', 'CO')][1,1]
  month <- weather[which(is.na(weather$CO)), c('years','months', 'CO')][1,2]
  weather[which(is.na(weather$CO)), 'CO'][1]  <- as.vector(unlist(group_CO[which(group_CO$years == year & group_CO$months == month), 'co']))
}

group_SO2 <- weather %>% group_by(years,months) %>% summarise(so2 = mean(SO2, na.rm = TRUE))
numofNA <- dim(weather[which(is.na(weather$SO2)), c('date', 'SO2')])[1]
for (i in seq(numofNA)) {
  year <- weather[which(is.na(weather$SO2)), c('years','months', 'SO2')][1,1]
  month <- weather[which(is.na(weather$SO2)), c('years','months', 'SO2')][1,2]
  weather[which(is.na(weather$SO2)), 'SO2'][1]  <- as.vector(unlist(group_SO2[which(group_SO2$years == year & group_SO2$months == month), 'so2']))
}

group_china <- weather %>% group_by(years, months) %>% summarise(china_dust = median(china_PM25, na.rm=TRUE))
numofNA <- dim(weather[which(is.na(weather$china_PM25)), c('date', 'china_PM25')])[1]
print(numofNA)
for (i in seq(numofNA)) {
  print(i)
  year <- weather[which(is.na(weather$china_PM25)), c('years','months', 'china_PM25')][1,1]
  month <- weather[which(is.na(weather$china_PM25)), c('years','months', 'china_PM25')][1,2]
  weather[which(is.na(weather$china_PM25)), 'china_PM25'][1]  <- as.vector(unlist(group_china[which(group_china$years == year & group_china$months == month), 'china_dust']))
}

summary(weather)

length(levels(weather$최대.풍속.풍향))

group_direction1 <- weather %>% group_by(years,months, 최대.순간.풍속.풍향) %>% summarise(count = n())
group_direction1 <- as.data.frame(group_direction1)

group_direction2 <- weather %>% group_by(years,months, 최대.풍속.풍향) %>% summarise(count = n())
group_direction2 <- as.data.frame(group_direction2)
colnames(group_direction1)[4] <- "value"
colnames(group_direction2)[4] <- "value"

weather[which(is.na(weather$최대.순간.풍속.풍향)),]

ggplot(group_direction1[which(group_direction1$months == "10월"),], aes(x=factor(최대.순간.풍속.풍향), y=value, fill=years)) + 
  geom_bar(stat = "identity") + ggtitle("10월풍향") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("최대.순간.풍속.풍향")

ggplot(group_direction2[which(group_direction2$months == "10월"),], aes(x=factor(최대.풍속.풍향), y=value, fill=years)) + 
  geom_bar(stat = "identity") + ggtitle("10월풍향") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("최대.풍속.풍향")

ggplot(group_direction1[which(group_direction1$months == "12월"),], aes(x=factor(최대.순간.풍속.풍향), y=value, fill=years)) + 
  geom_bar(stat = "identity") + ggtitle("12월 풍향") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("최대.순간.풍속.풍향")

ggplot(group_direction2[which(group_direction2$months == "12월"),], aes(x=factor(최대.풍속.풍향), y=value, fill=years)) + 
  geom_bar(stat = "identity") + ggtitle("12월 풍향") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("최대.풍속.풍향")

weather[which(is.na(weather$최대.순간.풍속.풍향)),'최대.순간.풍속.풍향'] <- c("290", "320")
weather[which(is.na(weather$최대.풍속.풍향)),'최대.풍속.풍향'] <- c("270", "290")

summary(weather)

minmaxscaler<- function(x) {
  round((x-min(x))/(max(x)-min(x)),3)
}


colnames(weather)

colvec  <- c("평균기온", "최저기온", "최고기온", "강수.계속시간", "일강수량", "최대.순간.풍속", "최대.풍속", "평균.풍속",
            "풍정합", "평균.이슬점온도", "최소.상대습도", "평균.상대습도", "평균.증기압", "평균.현지기압", "합계.일조.시간",
            "시간당.최다일사량", "합계.일사", "일.최심적설", "합계.대형증발량", "안개.계속시간", "PM10", "PM2.5", "O3",
            "NO2", "CO", "SO2", "종합대기지수", "china_PM25")

weather2 <- weather
weather2[,colvec] <- apply(select(weather,colvec),2,minmaxscaler)

weather2 %>% ggplot(aes(종합대기상태, china_PM25,  fill=종합대기상태)) + geom_boxplot()
weather2[-which(weather2$PM10 >= 0.75),] %>% ggplot(aes(최대.순간.풍속.풍향, PM10,  fill=종합대기상태)) + geom_boxplot()
weather2[-which(weather2$PM10 >= 0.75 | weather2$china_PM25 > 0.9),] %>% ggplot(aes(PM10, china_PM25, col=종합대기상태 )) + geom_point()

weather2$종합대기상태 <- factor(weather2$종합대기상태, levels=c("좋음","보통","나쁨","매우 나쁨"))
levels(weather2$종합대기상태)
#360=North, 45=NE, 90=East, 135=SE, 180=South, 225=SW, 270=West, 315=NW
levels(weather2$최대.풍속.풍향) <- c("NE", "NE", "E", "E", "SE", "SE", "S", "S", "SW", "SW", "W", "W", "NW", "NW", "N", "N")
levels(weather2$최대.순간.풍속.풍향) <- c("NE", "NE", "E", "E", "SE", "SE", "S", "S", "SW", "SW", "W", "W", "NW", "NW", "N", "N")



analysis_data <- weather2
analysis_data$종합대기지수 <- lag(analysis_data$종합대기지수)
analysis_data$china_PM25 <- lag(analysis_data$china_PM25)

head(analysis_data)
tail(analysis_data)

analysis_data <- analysis_data[-1,]

analysis_data %>% ggplot(aes(종합대기상태)) + geom_bar()
analysis_data %>% ggplot(aes(종합대기상태)) + geom_bar(aes(fill=최대.풍속.풍향)) + facet_wrap(~최대.풍속.풍향)

analysis_data %>% ggplot(aes(종합대기상태, china_PM25,  fill=종합대기상태)) + geom_boxplot()
analysis_data %>% ggplot(aes(최대.풍속.풍향, PM2.5,  fill=종합대기상태)) + geom_boxplot()
analysis_data[-which(weather2$PM10 >= 0.75),] %>% ggplot(aes(최대.풍속.풍향, PM10,  fill=종합대기상태)) + geom_boxplot()
analysis_data %>% ggplot(aes(최대.순간.풍속.풍향, PM2.5,  fill=종합대기상태)) + geom_boxplot()
analysis_data[-which(weather2$PM10 >= 0.75),] %>% ggplot(aes(최대.순간.풍속.풍향, PM10,  fill=종합대기상태)) + geom_boxplot()

analysis_data %>% ggplot(aes(PM2.5, china_PM25, col=종합대기상태)) + 
  geom_point() + stat_smooth(method = lm, se = TRUE, color = "red") 

analysis_data[-which(weather2$PM10 >= 0.75|weather2$china_PM25 >= 0.75),] %>% ggplot(aes(PM10, china_PM25, col=종합대기상태)) + 
  geom_point() + stat_smooth(method = lm, se = TRUE, color = "red") 

rownames(analysis_data) <- analysis_data$date

# analysis_data <- analysis_data[,-1]

analysis_data$index <- seq(2,(dim(analysis_data)[1]+1))
analysis_data

colnames(train)
set.seed(1234)
train <- sample_frac(analysis_data, 0.7)
train_idx <- as.vector(train$index)
rownames(train)<-train$date
train <- train[,-c(1,8,31,34,35,36)]
test <- analysis_data[-train_idx, -c(8,31,34,35,36)]
rownames(test) <- test$date
real_val <- test$종합대기상태
test <- select(test, -c("date","종합대기상태"))

raw_model <- naiveBayes(종합대기상태~., data=train)
pred <- predict(raw_model, test)
confusionMatrix(pred, real_val)

# laplace 추정량
raw_model2 <- naiveBayes(종합대기상태~., data=train, laplace=0.02)
pred2 <- predict(raw_model2, test)
confusionMatrix(pred2, real_val)

raw_model2

raw_model3 <- multinom(종합대기상태~., data=train)
pred3 <- predict(raw_model3, test)
confusionMatrix(pred3, real_val)
