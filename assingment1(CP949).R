library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(e1071)
library(corrplot)
library(caret)
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# install.packages("corrplot")
knitr::opts_chunk$set(echo = TRUE, warning = F, message = F)
weather <- read.csv("./weather/airquality/final_data6.csv")
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")
stocks <- weather[,c(1,seq(34,38))]
weather <- weather[,-seq(34,38)]

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


analysis_data <- data.frame(weather2)
analysis_data$종합대기지수 <- lag(analysis_data$종합대기지수)
analysis_data$china_PM25 <- lag(analysis_data$china_PM25)
analysis_data <- analysis_data[-1,]

colvec2  <- c("평균기온", "강수.계속시간", "일강수량", "평균.풍속", "풍정합", "평균.이슬점온도", "평균.상대습도", "평균.증기압",
              "평균.현지기압", "합계.일조.시간","시간당.최다일사량", "합계.일사", "일.최심적설", "합계.대형증발량", "안개.계속시간", 
              "PM10", "PM2.5", "O3","NO2", "CO", "SO2", "종합대기지수", "china_PM25")

corr_data <- cor(analysis_data[,colvec2])
corrplot(corr_data, method= "square", type="upper", tl.cex = 0.7, cl.cex = 0.5)

p1<- analysis_data %>% ggplot(aes(종합대기상태, 평균기온, fill=종합대기상태)) +
  geom_boxplot(position = position_dodge(width = 1), outlier.alpha = T)
p2 <- analysis_data %>% group_by(years, months, 종합대기상태) %>% summarise(종합대기빈도=n()) %>%
  ggplot(aes(x=months, y=종합대기빈도, fill=종합대기상태)) + geom_bar(stat="identity", position = position_dodge(width = 1)) +
  facet_wrap(종합대기상태~.)

grid.arrange(p1, p2, nrow=2)

summary_cai <- analysis_data %>% group_by(years, months, 종합대기상태) %>% summarise(빈도수=n())
p3 <- summary_cai[which(summary_cai$종합대기상태 == "나쁨"),] %>%
  ggplot(aes(x=months, y=빈도수, fill=종합대기상태)) + geom_bar(stat="identity")

p4 <- summary_cai[which(summary_cai$종합대기상태 == "매우 나쁨"),] %>%
  ggplot(aes(x=months, y=빈도수)) + geom_bar(stat="identity", fill='#c159d6')
grid.arrange(p3, p4, nrow=2)

group_direction3 <- analysis_data %>% group_by(years, months, 최대.순간.풍속.풍향, 종합대기상태) %>% summarise(count = n())
group_direction3 <- as.data.frame(group_direction3)

group_direction4 <- analysis_data %>% group_by(years, months,최대.풍속.풍향, 종합대기상태) %>% summarise(count = n())
group_direction4 <- as.data.frame(group_direction4)

p5 <- group_direction3 %>% ggplot(aes(x=최대.순간.풍속.풍향, y=count, fill=종합대기상태)) + geom_bar(stat="identity") + facet_wrap(종합대기상태~.)
p6 <- group_direction3[which(group_direction3$종합대기상태 == "매우 나쁨"),] %>% 
  ggplot(aes(x=최대.순간.풍속.풍향, y=count)) + 
  geom_bar(stat="identity", fill='#14e5d7')+
  ggtitle("나쁨과 매우나쁨일대 풍향")
p7 <- group_direction3[which(group_direction3$종합대기상태 == "나쁨"),] %>% 
  ggplot(aes(x=최대.순간.풍속.풍향, y=count)) + 
  geom_bar(stat="identity", fill='#3214e0')
grid.arrange(p6, p7, nrow=2)
p7 <- group_direction3 %>% ggplot(aes(x=months, y=count, fill=종합대기상태)) + geom_bar(stat="identity",position = "dodge") + facet_wrap(최대.순간.풍속.풍향~.)
p7


p8 <- analysis_data %>% ggplot(aes(종합대기상태, china_PM25,  fill=종합대기상태)) + geom_boxplot(show.legend = T)
p9 <- analysis_data[-which(analysis_data$PM10 >= 0.75),] %>% ggplot(aes(최대.풍속.풍향, PM10,  fill=종합대기상태)) + geom_boxplot(outlier.alpha = T)
p9 <- analysis_data %>% ggplot(aes(최대.풍속.풍향, PM10,  fill=종합대기상태)) + geom_boxplot()
p10 <- analysis_data[-which(analysis_data$PM2.5 >= 0.75),] %>% ggplot(aes(최대.순간.풍속.풍향, PM2.5,  fill=종합대기상태)) + geom_boxplot()
grid.arrange(p8, p9, p10, ncol=1)

p11<- analysis_data %>% ggplot(aes(PM2.5, china_PM25, col=종합대기상태)) + 
  geom_point() + stat_smooth(method = lm, se = TRUE, color = "red") 

p12 <- analysis_data[-which(analysis_data$PM10 >= 0.75|analysis_data$china_PM25 >= 0.75),] %>% ggplot(aes(PM10, china_PM25, col=종합대기상태)) +  
  geom_point() + stat_smooth(method = lm, se = TRUE, color = "red") 
grid.arrange(p11, p12, ncol=2)
levels(analysis_data$종합대기상태) <- c("좋음","보통","나쁨","나쁨")


# 분석을 위해서 데이터 셋을 7대 3으로 분할

analysis_data$index <- seq(2,(dim(analysis_data)[1]+1))
set.seed(1234)
analysis_data2 <- data.frame(analysis_data)
rownames(analysis_data2) <- analysis_data2$date
analysis_data2 <- analysis_data2[,-1]
train <- sample_frac(analysis_data2, 0.7)
train_idx <- as.vector(train$index)
test <- analysis_data2[-train_idx, ]
real_val <- test$종합대기상태
test <- dplyr::select(test, -c("종합대기상태"))

#일단 가장 기본적인 모형으로 분석

raw_model <- naiveBayes(종합대기상태~., data=train)
pred <- predict(raw_model, test)
confusionMatrix(pred, real_val)


# 서로 상관성이 큰 변수들을 제거하고 다시 분석
colnames(train)
train1 <- train[,-c(2,3,6,7,8,13,18,30,33,34,35)]
test1 <- test[, -c(2,3,6,7,8,13,18,29,32,33,34)]
nb_model1 <- naiveBayes(종합대기상태~., data=train1)
pred1 <- predict(nb_model1, test1)
confusionMatrix(pred1, real_val)

#### 대기 오염 지수를 범주화

analysis_data3 <- data.frame(analysis_data2)
china_grade <- cut(weather$china_PM25, breaks=c(0, 15, 25, 50, Inf), labels=c("good", "normal", "bad", "very bad"))
PM25_grade <- cut(weather$PM2.5, breaks=c(0, 15, 25, 50, Inf), labels=c("good", "normal", "bad", "very bad"))
PM10_grade <- cut(weather$PM10, breaks=c(0, 30, 50, 100, Inf), labels=c("good", "normal", "bad", "very bad"))

analysis_data3$china_PM25 <- china_grade[-1]
analysis_data3$PM10 <- PM10_grade[-1]
analysis_data3$PM2.5 <- PM25_grade[-1]

train2 <- analysis_data3[train_idx, -c(2,3,6,7,8,13,18,30,33,34,35)]
test2<- analysis_data3[-train_idx, -c(2,3,6,7,8,13,18,30,33,34,35)]
test2 <- dplyr::select(test2, -c("종합대기상태"))
nb_model2 <- naiveBayes(종합대기상태~., data=train2)
pred2 <- predict(nb_model2, test2)
confusionMatrix(pred2, real_val)

#### laplace smoothing

nb_laplace_model <- naiveBayes(종합대기상태~., data=train1, laplace=0.1)
pred3 <- predict(nb_laplace_model, test1)
confusionMatrix(pred3, real_val)

ml_model <- multinom(종합대기상태~., data=train1, trace=F)
pred4 <- predict(ml_model, test1)
confusionMatrix(pred4, real_val)

### DNN

set.seed(1234)
dnn_model <- nnet(종합대기상태~., data = train1, size=7, maxit=1000, trace=F)
plot.nnet(dnn_model)
pred5 <- predict(dnn_model, test1, type="class")
confusionMatrix(factor(pred5, levels = c("좋음","보통","나쁨")), real_val)


### 교차검증

cv_allmodel <- function(x, y){
  set.seed(1234)
  folds <- createFolds(analysis_data2$종합대기상태, k=x)
  nb_acc <- c()
  mn_acc <- c()
  nn_acc <- c()
  for (f in folds) {
    train_cv <- analysis_data2[-f,]
    test_cv <- analysis_data2[f, ]
    cv.real_val <- test_cv$종합대기상태
    test_cv <- dplyr::select(test_cv, -c("종합대기상태"))
    cv.nb_model <- naiveBayes(종합대기상태~., data=train_cv, laplace=1)
    cv.mn_model <- multinom(종합대기상태~., data=train_cv, trace=F)
    cv.nn_model <- nnet(종합대기상태~., data = train_cv, size=y, maxit=1000, trace=F)
    cv.pred_nb <- predict(cv.nb_model, test_cv, type="class")
    cv.pred_mn <- predict(cv.mn_model, test_cv, type="class")
    cv.pred_nn <- predict(cv.nn_model, test_cv, type="class")
    nb_f_acc <- sum(cv.pred_nb==cv.real_val)/NROW(cv.real_val)
    mn_f_acc <- sum(cv.pred_mn==cv.real_val)/NROW(cv.real_val)
    nn_f_acc <- sum(cv.pred_nn==cv.real_val)/NROW(cv.real_val)
    nb_acc <-append(nb_acc, nb_f_acc)
    mn_acc <-append(mn_acc, mn_f_acc)
    nn_acc <-append(nn_acc, nn_f_acc)
  }
  cv.df <- data.frame(
    k = seq(1,x),
    naive_Bayes = nb_acc,
    multinomial = mn_acc,
    neuralnet = nn_acc
  )
  return(cv.df)
}

nb_cv_acc <- cv_allmodel(8,8)
nb_cv_acc
mean_cv <- apply(nb_cv_acc,2,mean)
mean_cv
nb_cv_acc %>% ggplot() +
  geom_line(aes(x=k, y=naive_Bayes, colour="darkblue"),size=1) +
  geom_line(aes(x=k, y=multinomial, colour="green"),size=1) +
  geom_line(aes(x=k, y=neuralnet, colour="red"),size=1) +
  scale_color_discrete(name = "model", labels = c("naive_Bayes", "multinomial", "neuralnet")) +
  geom_hline(yintercept=mean_cv[2], linetype="dashed", color = "red") + 
  geom_hline(yintercept=mean_cv[3], linetype="dashed", color = "green") + 
  geom_hline(yintercept=mean_cv[4], linetype="dashed", color = "blue") +
  ylab("accuracy")
```

### 5. 개선방안

stock_corr <- cor(stocks[,-c(1)])
corrplot(stock_corr, method= "square", type="upper", tl.cex = 0.7, cl.cex = 0.5)

stock_ts <- ts(stocks, start=c(2015,1,1), frequency =12)
stock_ts <- 1+(stock_ts/stats::lag(stock_ts,-1) - 1)
stock_ts <- data.frame(stock_ts)
stock_ts$date <- stocks$date[2:1538]
stock_ts <- stock_ts[,-c(1)]
colnames(stock_ts)

stock_ts_corr <- cor(stock_ts[,-c(1,8)])
corrplot(stock_ts_corr, method= "square", type="upper", tl.cex = 0.7, cl.cex = 0.5)

# "stock_ts.PM10"    "stock_ts.PM2.5"   "stock_ts.X187750" "stock_ts.X045060" "stock_ts.X083550" "stock_ts.X045520" "stock_ts.X022100"
stock_ts %>% ggplot(aes(x=date))+
  geom_line(aes(y=stock_ts.PM10,colour="darkblue"))+
  geom_line(aes(y=stock_ts.X187750, colour="green"))+
  geom_line(aes(y=stock_ts.X045060, colour="red"))+
  scale_color_discrete(name = "variable", 
                       labels = c("PM2.5", "X187750", "X045060"))+
  ylim(c(-0,4))


# 향후에 네이버 트렌드 데이터와 구글 트렌드 데이터, 관련 상품 판매량 데이터를 추가하여 분석을 수행하고자 함

naver_pm25 <- subset(stocks, date >= as.Date("2017-08-01"))
naver_trend <- read.csv("./weather/naver_trend.csv")
naver_trend[which(is.na(naver_trend$공기청정기)),'공기청정기'] <- 0 
naver_trend[which(is.na(naver_trend$미세먼지)),'미세먼지'] <- 0 
naver_trend[which(is.na(naver_trend$마스크)),'마스크'] <- 0 
naver_trend$date <- as.Date(naver_trend$date, format = "%Y-%m-%d")
naver_trend$PM25 <- naver_pm25$PM2.5
naver_trend %>% ggplot() +
  geom_line(aes(x=date, y=공기청정기, colour="darkblue"),size=1) +
  geom_line(aes(x=date, y=미세먼지, colour="green"),size=1) +
  geom_line(aes(x=date, y=마스크, colour="red"),size=1) +
  geom_line(aes(x=date, y=PM25, colour="#ef17cf")) +
  scale_color_discrete(name = "검색어", labels = c("공기청정기", "미세먼지", "마스크", "PM2.5")) +
  ylab("조회량&미세먼지농도")





# cv_nbmodel <- function(x){
#   set.seed(124)
#   folds <- createFolds(analysis_data$종합대기상태, k=x)
#   acc <- c()
#   for (f in folds) {
#     train_cv <- analysis_data2[-f,]
#     test_cv <- analysis_data2[f, ]
#     cv.real_val <- test_cv$종합대기상태
#     test_cv <- dplyr::select(test_cv, -c("종합대기상태"))
#     cv.nb_model <- naiveBayes(종합대기상태~., data=train_cv, laplace=1)
#     cv.pred = predict(cv.nb_model, test_cv, type="class")
#     xtabs(~cv.pred+cv.real_val)
#     f_acc <- sum(cv.pred==cv.real_val)/NROW(cv.real_val)
#     acc <-append(acc, f_acc)
#   }
#   return(acc)
# }
# 
# nb_cv_acc <- cv_nbmodel(6)
# mean(nb_cv_acc)
# 
# cv_mnmodel <- function(x){
#   set.seed(1234)
#   folds <- createFolds(analysis_data$종합대기상태, k=x)
#   acc <- c()
#   for (f in folds) {
#     train_cv <- analysis_data2[-f,]
#     test_cv <- analysis_data2[f, ]
#     cv.real_val <- test_cv$종합대기상태
#     test_cv <- dplyr::select(test_cv, -c("종합대기상태"))
#     cv.mn_model <- multinom(종합대기상태~., data=train_cv, trace=F)
#     cv.pred = predict(cv.mn_model, test_cv, type="class")
#     xtabs(~cv.pred+cv.real_val)
#     f_acc <- sum(cv.pred==cv.real_val)/NROW(cv.real_val)
#     acc <-append(acc, f_acc)
#   }
#   return(acc)
# }
# 
# mn_cv_acc <- cv_mnmodel(8)
# mean(mn_cv_acc)
# 
# 
# cv_nnet_model <- function(x){
#   set.seed(1234)
#   folds <- createFolds(analysis_data$종합대기상태, k=x)
#   acc <- c()
#   for (f in folds) {
#     train_cv <- analysis_data2[-f,]
#     test_cv <- analysis_data2[f, ]
#     cv.real_val <- test_cv$종합대기상태
#     test_cv <- dplyr::select(test_cv, -c("종합대기상태"))
#     cv.nn_model <- nnet(종합대기상태~., data = train_cv, size=6, maxit=1000, trace=F)
#     cv.pred = predict(cv.nn_model, test_cv, type="class")
#     xtabs(~cv.pred+cv.real_val)
#     f_acc <- sum(cv.pred==cv.real_val)/NROW(cv.real_val)
#     acc <-append(acc, f_acc)
#   }
#   return(acc)
# }
# 
# nn_cv_acc <- cv_mnmodel(8)
# mean(nn_cv_acc)


