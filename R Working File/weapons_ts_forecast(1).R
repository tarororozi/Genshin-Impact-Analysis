library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling
library(xts)

ts_weapons$Start <- as.Date(ts_weapons$Start)
ts_weapons$End <- as.Date(ts_weapons$End)

xts_data <- xts(ts_weapons$Total, order.by = ts_weapons$Start)

ts_data <- ts(xts_data, start = 2021, frequency = 17)  # By Amount of Events a Year
ts_data
library(ggplot2)
plot(ts_data)

ggAcf(x = ts_data)
ts_data
ggseasonplot(ts_data)


acf(x = ts_data,plot=F)

### SPLIT DATA
train = window(ts_data,end=c(2023,08))
test = window(ts_data, start=c(2023,09))
length(test)

### AVERAGE MODEL
average_model = meanf(train,h = 12)
average_model
accuracy(average_model,x = test)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(test)

### NAIVE MODEL
naive_model = naive(train,h=12)
naive_model
accuracy(naive_model,x = test)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(test)

### SEASONAL MODEL
seasonal_naive_model = snaive(train,h=12)
seasonal_naive_model
accuracy(seasonal_naive_model,x = test)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(seasonal_naive_model,PI=F,size=1.1,series='Seasonal Naive Model')+
  autolayer(test)


### DRIFT MODEL
drift_model = rwf(train,h=12,drift = T)
accuracy(drift_model,x = test)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(seasonal_naive_model,PI=F,size=1.1,series='Seasonal Naive Model')+
  autolayer(drift_model,PI=F,size=1.1,series='Drift Model')+
  autolayer(test)

### SIMPLE EXPONENTIAL SMOOTHING
ses_model = ses(train,h = 12)
accuracy(ses_model,x = test) 

ses_final_model = ses(ts_data, h = 17)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(test)

### HOLT MODEL
holt_model = holt(train,h=12)
accuracy(holt_model,x = test)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(test)

### HOLT DAMPENED MODEL
holt_damped_model = holt(train,h=12,damped = T)
accuracy(holt_damped_model,x=test)

holt_damped_final_model = holt(ts_data,h=17,damped = T)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(holt_damped_model,series="Holt's Method with Damping",PI=F,size=1.1)+
  autolayer(test)

### HOLTS WINTER ADDITIVE
hw_additive = hw(train,h=12,seasonal = 'additive', damped=T)
accuracy(hw_additive,x = test)

autoplot(train)+
  autolayer(hw_additive,series="Holt Winter's Method - Additive",PI=F)+
  autolayer(test)

### HOLTS WINTER MULTIPLICATIVE
hw_multiplicative = hw(train,h=12,seasonal = 'multiplicative', damped=T)
accuracy(hw_multiplicative,x = test)

autoplot(train)+
  autolayer(hw_multiplicative,series="Holt Winter's Method - Multiplicative",PI=F)+
  autolayer(test)

### ETS AAA MODEL
ets_aaa = ets(train,model = 'AAA')
checkresiduals(ets_aaa)

ets_aaa_forecast = forecast(ets_aaa,h=12)
ets_aaa_forecast

accuracy(ets_aaa_forecast,x = test)

autoplot(train)+
  autolayer(ets_aaa_forecast,series="ETS - AAA",PI=F)+
  autolayer(test)

### ETS AUTO MODEL
ets_auto = ets(train)
summary(ets_auto)

ets_auto_final = ets(ts_data)

ets_auto_forecast = forecast(ets_auto_final,h=17)
accuracy(ets_auto_forecast,x = test)

autoplot(train)+
  autolayer(ets_auto_forecast,series="ETS - MAM (auto)",PI=F)+
  autolayer(test)


### AUTO ARIMA MODEL
kpss.test(ts_data)

model_auto = auto.arima(y = train,d = 1,D = 1,stepwise = F,approximation = F)
model_auto

arima_forecast = forecast(model_auto, h = 12)

### AUTO ARIMA FULL MODEL
arima_cons_var = BoxCox(ts_data,lambda = BoxCox.lambda(ts_data))
autoplot(arima_cons_var)

arima_final = auto.arima(y = ts_data,d = 1,D = 1,stepwise = F,approximation = F)
arima_final

arima_final_forecast = forecast(arima_final,h=17)


autoplot(train, color='black')+
  autolayer(test,size=1.05,color='gray')+
  autolayer(arima_forecast,series = 'ARIMA',PI=F, color = 'blue')+
  autolayer(ets_auto_forecast,series="ETS - MAM (auto)",PI=F, color = 'red')

rbind(average_model = accuracy(f = average_model,x = test)[2,],
      naive_model = accuracy(f = naive_model,x = test)[2,],
      seasonal_naive_model = accuracy(f = seasonal_naive_model,x = test)[2,],
      drift_model = accuracy(f = drift_model,x = test)[2,],
      ses_model = accuracy(f = ses_model,x = test)[2,],
      holt_model = accuracy(f = holt_model,x = test)[2,],
      holt_damped_model = accuracy(f = holt_damped_model,x = test)[2,],
      hw_additive_model = accuracy(f = hw_additive,x = test)[2,],
      hw_multiplicative = accuracy(f = hw_multiplicative,x = test)[2,],
      ets_aaa = accuracy(ets_aaa_forecast,x = test)[2,],
      ets_auto = accuracy(ets_auto_forecast,x = test)[2,],
      arima = accuracy(arima_forecast,x= test)[2,]
)

rbind(ses_model = accuracy(f = ses_model,x = test)[2,],
      holt_damped_model = accuracy(f = holt_damped_model,x = test)[2,],
      ets_auto = accuracy(ets_auto_forecast,x = test)[2,])



autoplot(train, color='black')+
  autolayer(test,series = 'Test', color= 'gray')+
  autolayer(ses_model,series = 'Seasonal Naive Model',PI=F, color = 'green')+
  autolayer(holt_damped_model,series = 'Holt',PI=F, color = 'orange')+
  autolayer(ets_auto_forecast,series = 'ETS Auto',PI=F, color = 'red')+
  autolayer(arima_forecast,series = 'ARIMA',PI=F, color = 'blue')
  


autoplot(train, color = 'black') +
  autolayer(test, series = 'Test', color = 'gray') +
  autolayer(ses_model, series = 'Simple Exponential Smoothing', PI = FALSE, color = 'green') +
  autolayer(holt_damped_model, series = 'Holt', PI = FALSE, color = 'orange') +
  autolayer(ets_auto_forecast, series = 'ETS Auto', PI = FALSE, color = 'red') +
  autolayer(arima_forecast, series = 'ARIMA', PI = FALSE, color = 'blue')

autoplot(ts_data, color = 'black') +
  autolayer(arima_final_forecast, series = 'ARIMA', PI = FALSE, color = 'blue') +
  autolayer(ses_final_model, series = 'Simple Exponential Smoothing', PI = FALSE, color = 'green') +
  autolayer(holt_damped_final_model, series = 'Holt', PI = FALSE, color = 'orange') +
  autolayer(ets_auto_forecast, series = 'ETS Auto', PI = FALSE, color = 'red')
  

