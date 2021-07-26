library(readxl)
library(dplyr)
library(tidyr)
library(fpp2)
library(tseries)
library(data.table)
library(Amelia)
library(forecast)
library(TSstudio)
library(urca)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(reprex)
library(GGally)


heating <- read.csv('C:/Users/Jaswinder Singh/Downloads/Statistics/Data/Time Series/Heating.csv', header = FALSE)

heating <- heating[-c(1),]
head(heating)
colnames(heating) <- c("Time", "HDD_Index")
heating_ts <- ts(as.numeric(heating[,-1]), start=c(2001,1), end=c(2019,12), frequency=12)

train_heating = window(heating_ts, start=c(2001,1), end=c(2018,12))
test_heating = window(heating_ts, start = c(2019,1))
# heating_2 <- window(heating_ts, start=2001,end=c(2018,12))
Snaive_train <- snaive(train_heating , h=10)
Naive_train <- naive(train_heating, h = 10)
clrs <- c("blueviolet", "red")

autoplot(heating_ts) + 
  autolayer(Snaive_train, series = "Seasonal Naive", PI = FALSE)+ 
  autolayer(Naive_train, series = "Naive", PI = FALSE) + 
  xlab("Year") + 
  ylab("HDD Index") + 
  ggtitle("Naive and Seasonal Naive forecasts")+
  guides(colour=guide_legend(title="Forecast")) +
  scale_color_manual(values=clrs)
