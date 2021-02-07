# Energy Consumption Forecast

Forecasting energy consumption for Pittsburgh

![banner](/images/banner.jpg)

## INTRODUCTION

Machine learning models like Time series produce accurate energy consumption forecasts and they can be used by facilities managers, utility companies and building commissioning projects to implement energy saving policies. We believe that efforts towards estimating energy consumption and developing tools for researchers to advance their research in energy consumption are necessary for a more scalable and sustainable future.

## DATA OVERVIEW

The [dataset](https://www.kaggle.com/robikscube/hourly-energy-consumption) is obtained from PJM Interconnection which is a regional transmission organization in the United States. PJM is part of the Eastern Interconnection grid operating an electric transmission system serving all or parts of Delaware, Illinois, Indiana, Kentucky, Maryland, Michigan, New Jersey, North Carolina, Ohio, Pennsylvania, Tennessee, Virginia, West Virginia and the District of Columbia. The hourly power consumption data are in megawatt. Here, we are just selecting the power consumption for the Duquesne Light Company, which operates primarily in Pittsburgh and surrounding areas for our project.

## DATA DESCRIPTION

Data set contains complete power consumption hourly data through 2005 Dec – 2018 Jan. The file has 119069 observations (hourly data) with two variables as shown below:

- Date (type time): time frame at which energy was consumed.
- Megawatt Energy consumption (type integer): Energy consumption of a particular region.
For simplicity, I've considered data in the timeframe : [2013-01-01,2016-12-31] as training dataset and [2017-01-01,2017-09-30] as testing data

## PROBLEM STATEMENT

Analysis and forecast of electricity consumption using various models. 

## APPROACH

- The data in hand is hourly power consumption. Such a high frequency data has multiple seasonality. To deal with that I've used **msts** class object to store the data.
- The decomposed time series is as follows :

![prophet decomp](/images/dcomp_prophet.png)

- I've evaluated my performance against the mean forecast.
- In addition, in the covariates models I've included weather data for the region in the same time period. (Source :[https://www.kaggle.com/selfishgene/historical-hourly-weather-data](https://www.kaggle.com/selfishgene/historical-hourly-weather-data))

## RESULTS & CONCLUSION

[Model Performance](https://www.notion.so/cd67a2f3eb4e45d9ba48515c51d7089a)

Forecasting using covariates beats the mean forecast, hence is the best model for this dataset.