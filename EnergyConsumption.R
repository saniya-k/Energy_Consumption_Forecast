library(fpp2)
library(dplyr)
library(xts)
library(tsbox)
library(urca)
library(xts)
library(tseries)
library(lubridate)
library(caret)
library(dygraphs)
library(prophet)

#function to calcuate rmse
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

## Reading the data
duq_pc <- read.csv("DUQ_hourly.csv",stringsAsFactors = F)
head(duq_pc)
tail(duq_pc)
str(duq_pc)



## Storing the data in a time series object
duq_pc$Datetime <- ymd_hms(duq_pc$Datetime) #datetime format
ts_train<-duq_pc$DUQ_MW %>% ts(freq= 24) #specifying the number of times that data was collected



## Estimating the trend component and seasonal component of our data - on a subset of last 5 months in the dataset
ts_train %>% 
  tail(24*7*5) %>% 
  decompose() %>% 
  autoplot()

ts_train %>%  tail(24*7*5) %>% mstl() %>% autoplot()


##-----Forecasting-------

#For ease of visualisation and minimising the processing time, restricting data to 2013-2017

duq_new <- duq_pc[duq_pc$Datetime >= '2013-01-01 00:00:00' & duq_pc$Datetime <= '2017-09-30 00:00:00',]

#Dividing our data into train and test
duq_train=duq_new[duq_new$Datetime < '2017-01-01',]
duq_test=duq_new[duq_new$Datetime >= '2017-01-01',]
#max date= 30th Sept 2017
max(duq_new$Datetime)


#Electricity demand can vary by time of day, day in week or annually, hence this data has multiple seasonalities
# define an multi-seasonal time series for train and test data
msts_train=msts(duq_train$DUQ_MW,seasonal.periods = c(24,24*7,24*365.25), start=decimal_date(as.POSIXct('2013-01-01 00:00:00', tz = "EST")),end=decimal_date(as.POSIXct('2017-01-01 00:00:00', tz = "EST")))


#plot the time series
plot(msts_train,main="Electricity demand at DUQ powerstation",xlab="Year",ylab="Demand(MW)")

#Method 1 : Establish the baseline for our forecast - the mean forecast
baseline <- meanf(msts_train,h=24*273.9375) #273.9375 is the days in 9 months of our test data

autoplot(baseline)+geom_line(color = "#00CFB3", size = 0.5) + ggtitle('Mean Forecast :Duquesne Power - Consumption, 2014')+
  xlab('Date') + ylab('Consumption in MW')
#accuracy of the mean forecast
accuracy(baseline,duq_test$DUQ_MW)
#RMSE:303.47 on test data

#Method 2 :forecasting using MSTL/STLF for the next 9 months

#decomposiiton using MSTL
autoplot(mstl(msts_train)) +geom_line(color = "#00CFB3") + ggtitle('Duquesne Power - Consumption, 2012-16')+
  xlab('Date') + ylab('Consumption in MW')

#forecast
fcast_stfl <- msts_train %>% stlf(h=24*273.9375) 

autoplot(fcast_stfl)+geom_line(color = "#00CFB3", size = 0.5) + ggtitle('STLF forecast : Duquesne Power - Consumption')+
  xlab('Date') + ylab('Consumption in MW')
#accuracy of stlf
accuracy(fcast_stfl,duq_test$DUQ_MW)
#RMSE :  464.29 on test data

#Method 3: forecasting using Dynamic harmonic regression

#In this we add a fourier term for each seasonal period (seasonal period in this case are 24,168,8766)
xreg<-fourier(msts_train , K=c(1,1,1))
xreg
fit <- auto.arima(msts_train, seasonal=FALSE, lambda=0,xreg=xreg)
fcast_fourier<-fit %>%
  forecast(xreg=fourier(msts_train, K=c(1,1,1), h=24*273.9375)) 

#plot the forecast
fcast_fourier %>%   autoplot(include=24*273.9375) +
  ggtitle('Dynamic Harmonic forecast : Duquesne Power - Consumption')+
  ylab("Predicted Consumption in MW") + xlab("Date")

#accuracy of dynamic fourier
accuracy(fcast_fourier,duq_test$DUQ_MW)
#RMSE 380.76 on test data

#Method 4: forecasting with TBATS

tbats1 <-tbats(msts_train %>% tail(24*365.25*5))
fcast_tbats <- forecast(tbats1,h=24*273.9375)
autoplot(fcast_tbats) +ggtitle('TBATS forecast : Duquesne Power - Consumption')+
  xlab('Date') + ylab('Consumption in MW')
accuracy(fcast_tbats,duq_test$DUQ_MW)
#RMSE :469.89881 on test data


#Method 5: Bootstrapping and bagging

#simulating 5 series using boosting
bootseries <- bld.mbb.bootstrap(msts_train,5) %>%
  as.data.frame() %>% msts(seasonal.periods = c(24,24*7,24*365.25), start=decimal_date(as.POSIXct('2013-01-01 00:00:00')),end=decimal_date(as.POSIXct('2017-01-01 00:00:00')))
autoplot(msts_train) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(msts_train, colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")

#using bagged ets vs ets
etsfc <- msts_train %>% ets() %>% forecast(h=24*273.9375)
baggedfc <- msts_train %>% baggedETS() %>% forecast(h=24*273.9375)
autoplot(msts_train) +
  autolayer(baggedfc, series="BaggedETS", PI=FALSE) +
  autolayer(etsfc, series="ETS", PI=FALSE) +
  guides(colour=guide_legend(title="Bagged forecast"))

accuracy(baggedfc,duq_test$DUQ_MW)
#RMSE 428.86 on test


#Method 6: As electricity consumption is strongly related to temperature, using covariates to predict temperature

temp_df<-read.csv("temperature.csv")
temp_df<-subset(temp_df,select=c('datetime','Pittsburgh'))
temp_df$datetime<- as.POSIXct(temp_df$datetime, '%Y-%m-%d %H:%M:%S', tz = "EST")
#restrict data to 2013-01 to 2017-09
temp_df<-  temp_df[temp_df$datetime >= '2013-01-01 00:00:00' & temp_df$datetime < '2017-09-30 00:00:00',]
#3 missing values

nrow(temp_df[!complete.cases(temp_df$Pittsburgh),])#mean imputation, (num missing is low)
temp_df[!complete.cases(temp_df$Pittsburgh),]$Pittsburgh<-mean(temp_df[complete.cases(temp_df$Pittsburgh),]$Pittsburgh)

time_index <- seq(from = as.POSIXct('2013-01-01 00:00:00', tz = "EST"), 
                  to = as.POSIXct('2017-09-30 00:00:00', tz = "EST"), by = "hour")

#to see he relationship b/w electricity usage and temperature, plotting them on the same graph
ts_temp=as.xts(temp_df$Pittsburgh,order.by = temp_df$datetime)


#create test and train temp datasets
temp_df_train <- temp_dfc[temp_df$datetime < '2017-01-01',]
temp_df_test <- temp_dfc[temp_df$datetime >= '2017-01-01',]

duq_new <- duq_pc[duq_pc$Datetime >= '2013-01-01 00:00:00' & duq_pc$Datetime <= '2017-09-30 00:00:00',]
duq_train=duq_new[duq_new$Datetime >= '2013-01-01 00:00:00'& duq_new$Datetime < '2017-01-01',]
duq_test=duq_new[duq_new$Datetime >= '2017-01-01',]

ts_elec=as.xts(duq_new$DUQ_MW,order.by = duq_new$Datetime)
ts_combined<-cbind(ts_temp,ts_elec)
#we can see peaks and troughs in temperature series, replicate those in elec. demand
dygraph(ts_combined) %>% 
  dySeries("ts_elec",axis="y2")


ts_combined %>% as.data.frame() %>% ggplot(aes(x=ts_temp,y=ts_elec))+geom_point()

#create test and train dataset (combination of temp and elec data)

data_train <- inner_join(duq_train,temp_df_train,by=c('Datetime'='datetime'))
data_test <- inner_join(duq_test,temp_df_test,by=c('Datetime'='datetime'))

#adding dummy variables fpr hour of the day and day of the week
data_train$hour_of_day <- strftime(data_train$Datetime,'%H')
data_train$day_of_week <- strftime(data_train$Datetime,'%u')
head(data_train)
data_test$hour_of_day <- strftime(data_test$Datetime,'%H')
data_test$day_of_week <- strftime(data_test$Datetime, '%u')
fit_reg <- lm(DUQ_MW~.,data=data_train)
summary(fit_reg)
plot(fit_reg)
predicted_MW <- predict(fit_reg,data_test)

sprintf('The RMSE for the model is: %f',rmse(predicted_MW, data_test$DUQ_MW))
# "The RMSE for the model is: 221.961718"



#Method 7: Prophet

colnames(duq_train) <- c('ds','y')
fit_prophet <- prophet(duq_train)
future_times <- data.frame("ds"=duq_test$Datetime)
fcast_prophet <- predict(fit_prophet,future_times)
plot(fit_prophet,fcast_prophet)

#decomposition by prophet
prophet_plot_components(fit_prophet,fcast_prophet)

#performance
sprintf('The RMSE for the model is: %f',rmse(fcast_prophet$yhat, duq_test$DUQ_MW))

#"The RMSE for the model is: 498.091704"