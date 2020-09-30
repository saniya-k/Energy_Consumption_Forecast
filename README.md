# Energy Consumption Forecast
Forecasting energy consumption for Pittsburgh

INTRODUCTION
Machine learning models like Time series produce accurate energy consumption forecasts and they can be
used by facilities managers, utility companies and building commissioning projects to implement energysaving policies. We believe that efforts towards estimating energy consumption and developing tools for
researchers to advance their research in energy consumption are necessary for a more scalable and
sustainable future.

DATA OVERVIEW
[link : https://www.kaggle.com/robikscube/hourly-energy-consumption, https://www.kaggle.com/selfishgene/historical-hourly-weather-data]
The dataset is obtained from PJM Interconnection which is a regional transmission organization in the
United States. PJM is part of the Eastern Interconnection grid operating an electric transmission system
serving all or parts of Delaware, Illinois, Indiana, Kentucky, Maryland, Michigan, New Jersey, North
Carolina, Ohio, Pennsylvania, Tennessee, Virginia, West Virginia and the District of Columbia. The
hourly power consumption data are in megawatt. Here, we are just selecting the power consumption for
the Duquesne Light Company, which operates primarily in Pittsburgh and surrounding areas for our
project.

DATA DESCRIPTION
Data set contains complete power consumption hourly data through 2005 Dec – 2018 Jan. The file has
119069 observations (hourly data) with two variables as shown below:
Date (type time): time frame at which energy was consumed.
Megawatt Energy consumption (type integer): Energy consumption of a particular region.
For simplicity, I've considered data in the timeframe : [2013-01-01,2016-12-31] as training dataset and [2017-01-01,2017-09-30] as testing data

PROBLEM STATEMENT 
Analysis and forecast of electricity consumption using various models.

PERFORMANCE
1. Mean Forecast (Baseline) : RMSE - 303.47 on test data
2. MSTL/STLF : RMSE - 464.29 on test data
3. Dynamic Harmonic regerssion: RMSE - 380.76 on test data
4. TBATS: RMSE - 469.89881 on test data
5. Bagging and boosting : RMSE - 428.86 on test data
6. Covariates (using external weather data) : RMSE - 221.961718 on test data
7. Prophet : RMSE - 498.091704 on test data

RESULT
Forecasting using covariates beats the mean forecast, hence is the best model for this dataset.

