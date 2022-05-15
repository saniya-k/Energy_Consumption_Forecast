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
library(segmented)

set.seed(110)

#function to calcuate rmse
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

## Reading the data
duq_pc <- read.csv("data/DUQ_hourly.csv",stringsAsFactors = F)
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
duq_train=duq_new[duq_new$Datetime <= '2016-12-31',]
duq_test=duq_new[duq_new$Datetime >= '2017-01-01',]
#max date= 30th Sept 2017
max(duq_new$Datetime)
min(duq_new$Datetime)

#Electricity demand can vary by time of day, day in week or annually, hence this data has multiple seasonalities
# define an multi-seasonal time series for train and test data
msts_train=msts(duq_train$DUQ_MW,seasonal.periods = c(24,24*7,24*365.25), start=decimal_date(as.POSIXct('2013-01-01 00:00:00', tz = "EST")),end=decimal_date(as.POSIXct('2017-01-01 00:00:00', tz = "EST")))
#dominant frequency
findfrequency(msts_train)

#plot the time series
plot(msts_train,main="Electricity demand at DUQ powerstation",xlab="Year",ylab="Demand(MW)")

#Method 1 : Establish the baseline for our forecast - Using seasonal Naive model -- Previous seasonal period's value(day) used as the baseline forecast
set.seed(110)
baseline <- snaive(msts_train,h=24*273.75) #273.75 is the days in 9 months of our test data
autoplot(baseline)+geom_line(color = "#00CFB3", size = 0.5) + ggtitle('Mean Forecast :Duquesne Power - Consumption, 2014')+
  xlab('Date') + ylab('Consumption in MW')
#accuracy of the seasonal naive forecast
accuracy(baseline,duq_test$DUQ_MW)
#RMSE:441.13 on test data

#Method 2 :forecasting using MSTL/STLF for the next 9 months
set.seed(110)
#decomposiiton using MSTL
autoplot(mstl(msts_train)) +geom_line(color = "#00CFB3") + ggtitle('Duquesne Power - Consumption, 2013-16')+
  xlab('Date') + ylab('Consumption in MW')

#forecast
fcast_stfl <- msts_train %>% stlf(h=24*273.75) 
summary(fcast_stfl)
autoplot(fcast_stfl)+geom_line(color = "#00CFB3", size = 0.5) + ggtitle('STLF forecast : Duquesne Power - Consumption')+
  xlab('Date') + ylab('Consumption in MW')
#accuracy of stlf
accuracy(fcast_stfl,duq_test$DUQ_MW)
#RMSE :  431.45 on test data

#Method 3: forecasting using Dynamic harmonic regression
set.seed(110)
#In this we add a fourier term for each seasonal period (seasonal period in this case are 24,168,8766)
xreg<-fourier(msts_train , K=c(1,1,1))
xreg
fit <- auto.arima(msts_train, seasonal=FALSE, lambda=0,xreg=xreg)
fcast_fourier<-fit %>%
  forecast(xreg=fourier(msts_train, K=c(1,1,1), h=24*273.75)) 


#plot the forecast
fcast_fourier %>%   autoplot(include=24*273.75) +
  ggtitle('Dynamic Harmonic forecast : Duquesne Power - Consumption')+
  ylab("Predicted Consumption in MW") + xlab("Date")

#accuracy of dynamic fourier
accuracy(fcast_fourier,duq_test$DUQ_MW)
#RMSE 483.07 on test data

#Method 4: forecasting with TBATS
set.seed(110)
tbats1 <-tbats(msts_train %>% tail(24*365.25*5))
summary(tbats1)

fcast_tbats <- forecast(tbats1,h=24*273.75)
autoplot(fcast_tbats) +ggtitle('TBATS forecast : Duquesne Power - Consumption')+
  xlab('Date') + ylab('Consumption in MW')
accuracy(fcast_tbats,duq_test$DUQ_MW)

#RMSE :431.83  on test data

#Method 5: Bootstrapping and bagging
set.seed(110)
#simulating 5 series using boosting
bootseries <- bld.mbb.bootstrap(msts_train,5) %>%
  as.data.frame() %>% msts(seasonal.periods = c(24,24*7,24*365.25), start=decimal_date(as.POSIXct('2013-01-01 00:00:00')),end=decimal_date(as.POSIXct('2017-01-01 00:00:00')))
autoplot(msts_train) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(msts_train, colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")

#using bagged ets vs ets
etsfc <- msts_train %>% ets() %>% forecast(h=24*273.75)
baggedfc <- msts_train %>% baggedETS() %>% forecast(h=24*273.75)
autoplot(msts_train) +
  autolayer(baggedfc, series="BaggedETS", PI=FALSE) +
  autolayer(etsfc, series="ETS", PI=FALSE) +
  guides(colour=guide_legend(title="Bagged forecast"))

accuracy(etsfc,duq_test$DUQ_MW)
#RMSE 527.39 on test
accuracy(baggedfc,duq_test$DUQ_MW)
#RMSE 412.19 on test

checkresiduals(baggedfc)


#Method 6: Prophet
set.seed(110)
colnames(duq_train) <- c('ds','y')
fit_prophet <- prophet(duq_train)
future_times <- data.frame("ds"=duq_test$Datetime)
fcast_prophet <- predict(fit_prophet,future_times)
plot(fit_prophet,fcast_prophet)

#decomposition by prophet
prophet_plot_components(fit_prophet,fcast_prophet)

#performance
sprintf('The RMSE for the model is: %f',rmse(fcast_prophet$yhat, duq_test$DUQ_MW))

#"The RMSE for the model is: 497.56"

#Method 7: As electricity consumption is strongly related to temperature, using covariates to predict temperature
set.seed(110)
temp_df<-read.csv("data/temperature.csv")
temp_df<-subset(temp_df,select=c('datetime','Pittsburgh'))
temp_df$datetime<- as.POSIXct(temp_df$datetime, '%Y-%m-%d %H:%M:%S', tz = "EST")
#restrict data to 2013-01 to 2017-09
temp_df<-  temp_df[temp_df$datetime >= '2012-12-31 23:00:00' & temp_df$datetime < '2017-09-30 00:00:00',]
#fixing missing values

nrow(temp_df[!complete.cases(temp_df$Pittsburgh),])#mean imputation, (num missing is low)
temp_df[!complete.cases(temp_df$Pittsburgh),]$Pittsburgh<-mean(temp_df[complete.cases(temp_df$Pittsburgh),]$Pittsburgh)

time_index <- seq(from = as.POSIXct('2013-01-01 00:00:00', tz = "EST"), 
                  to = as.POSIXct('2017-09-30 00:00:00', tz = "EST"), by = "hour")

#to see he relationship b/w electricity usage and temperature, plotting them on the same graph
ts_temp=as.xts(temp_df$Pittsburgh,order.by = temp_df$datetime)


#create test and train temp datasets

temp_df_train <- temp_df[temp_df$datetime <= '2016-12-31',]
temp_df_test <- temp_df[temp_df$datetime >= '2017-01-01',]

duq_new <- duq_pc[duq_pc$Datetime >= '2013-01-01 00:00:00' & duq_pc$Datetime <= '2017-09-30 00:00:00',]
duq_train=duq_new[duq_new$Datetime >= '2013-01-01 00:00:00'& duq_new$Datetime <= '2016-12-31 23:00:00',]
duq_test=duq_new[duq_new$Datetime >= '2017-01-01',]


# temp_df_train <- temp_df[temp_df$datetime <= '2017-08-31',]
# temp_df_test <- temp_df[temp_df$datetime >= '2017-09-01',]
# 
# duq_new <- duq_pc[duq_pc$Datetime >= '2013-01-01 00:00:00' & duq_pc$Datetime <= '2017-09-30 00:00:00',]
# duq_train=duq_new[duq_new$Datetime >= '2013-01-01 00:00:00'& duq_new$Datetime <= '2017-08-31',]
# duq_test=duq_new[duq_new$Datetime >= '2017-09-01',]

ts_elec=as.xts(duq_new$DUQ_MW,order.by = duq_new$Datetime)
#combine temperature lagged by an hour with the electricity time series
#using lagged temperature values as realtime temp. cannot be used as a predictor to predict electrcity for the same hour
ts_combined<-cbind(lagged_temp=stats::lag(ts_temp,1),hourly_elec=ts_elec)
#we can see peaks and troughs in temperature series, replicate those in elec. demand
dygraph(ts_combined) %>% 
  dySeries("hourly_elec",axis="y2",strokeWidth = 0.2) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, 'Set1'),fillAlpha = 0.4) %>% 
  dyAxis('y',label='Temperature (K)') %>%
  dyAxis('y2',label='Electricity Consumption (MW)', independentTicks = TRUE) 

ts_combined <- fortify(ts_combined)
ts_combined <- na.omit(ts_combined)
colnames(ts_combined)[colnames(ts_combined) == 'Index'] <- 'Datetime'

#create test and train dataset (combination of temp and elec data)
#filter with these dates:

data_train <- ts_combined[ts_combined$Datetime <= '2016-12-31',]
data_test <- ts_combined[ts_combined$Datetime >= '2017-01-01',]

# data_train <- ts_combined[ts_combined$Datetime <= '2017-08-31',]
# data_test <- ts_combined[ts_combined$Datetime >= '2017-09-01',]



#adding dummy variables fpr hour of the day and day of the week
data_train$hour_of_day <- strftime(data_train$Datetime,'%H')
data_train$day_of_week <- strftime(data_train$Datetime,'%u')
head(data_train)
data_test$hour_of_day <- strftime(data_test$Datetime,'%H')
data_test$day_of_week <- strftime(data_test$Datetime, '%u')
fit_reg <- lm(hourly_elec~.,data=data_train)
summary(fit_reg)
plot(fit_reg)
predicted_MW <- predict(fit_reg,data_test)

sprintf('The RMSE for the model is: %f',rmse(predicted_MW, data_test$hourly_elec))
# "The RMSE for the model is: 255.87"

#Method 8: Piecewise Regression model

#Correlation reveals there is a negative relationship between electricity consumption and lagged temperature till ~ 285K 
#After ~285K , electricity consumption has a positive correlation with temperature.

ts_combined %>% as.data.frame() %>% ggplot(aes(x=lagged_temp,y=hourly_elec))+geom_point()
#

#using segmentation package to alter the previous correlation
set.seed(110)
segmented.fit <- segmented(fit_reg, seg.Z = ~lagged_temp)
summary(segmented.fit)
plot(segmented.fit)
predicted_seg_MW <- predict(segmented.fit,data_test)

sprintf('The RMSE for the model is: %f',rmse(predicted_seg_MW, data_test$hourly_elec))

#"The RMSE for the model is: 186.08"


#Model 9: Dynamic Harmonic Regression + Covariates (Piecewise)
set.seed(110)
#Fit a regression model with a piecewise linear function of temperature (containing a knot at 283.76K), and harmonic regression terms to allow for the daily seasonal pattern.
cooling <- pmax(data_train$lagged_temp, 283.76)
fit_drp <- auto.arima(data_train$hourly_elec,
                  xreg = cbind(fourier(msts(data_train$hourly_elec,seasonal.periods = c(24,24*7,24*365.25)), K=c(1,1,1)),
                               heating=data_train$lagged_temp,
                               cooling=cooling))

fcast_drp<-fit_drp %>%
  forecast(xreg=cbind(fourier(msts(data_test$hourly_elec,seasonal.periods = c(24,24*7,24*365.25)), K=c(1,1,1)),
                      heating=data_test$lagged_temp, cooling=pmax(data_test$lagged_temp,18)), h=24*273.75) 


#plot the forecast
fcast_drp %>%   autoplot(include=24*273.75) +
  ggtitle('Dynamic Harmonic forecast : Duquesne Power - Consumption')+
  ylab("Predicted Consumption in MW") + xlab("Date")

accuracy(fcast_drp,data_test$hourly_elec)
#RMSE : 436.31 on test

#Check residuals for the best model

checkresiduals(fcast_drp)
