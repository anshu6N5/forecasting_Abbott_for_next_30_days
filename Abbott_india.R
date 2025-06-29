install.packages("MASS")
# loading of necessary libraries 
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(WDI)
library(nlme)
library(lmerTest)
library(performance)
library(quantmod)
library(forecast)

# Predicting The price of Abbott India Limited 

#Get the price data from Yahoo Finance 
data <- getSymbols("ABBOTINDIA.NS", src = "yahoo", start = 2025-06-27)
view(data)

#closing prices of Abbott India Limited
prices <- Cl(ABBOTINDIA.NS)


#converting in the prices into the time series 
ts_prices <- ts(prices)

#model 
model <- auto.arima(ts_prices)
summary(model)

#forecast the prices 
future <- forecast(model, h = 30)


#create a data frame 
last_day <- tail(index(prices), 1)
future_df30 <- data.frame(
  date = seq(from = last_day + 1, by = "days", length.out = 30),
  mean = as.numeric(future$mean),
  lower = as.numeric(future$lower),
  upper = as.numeric(future$upper)
)



#graphical representation 
ggplot(future_df30, aes(x = date)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.6, color = "darkblue") + 
  geom_point(aes(y = mean), size = 1.2 , color = "red") +
  labs(
    title = "The Forecasted Price of Abbott India (for next 30 days )",
    x = "Date", 
    y = "Price (INR)"
  )

theme_minimal()