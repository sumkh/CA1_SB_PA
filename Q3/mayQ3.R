pacman::p_load(forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest, lubridate)

setwd("C:/Users/May Khoo/Desktop/NUS/EBA5002 Business Analytics Practice/2 Predictive Analytics/CA/data")

rider = read.csv("AmtrakBig_CA_Question-3.csv", stringsAsFactors = F)

glimpse(rider)
head(rider, n=4)

#check the data type for a vaild ts object

is.ts(rider$Ridership)

ridership = ts(rider$Ridership, frequency = 12, start = c(2005, 1))

ridership
is.ts(ridership)

start(ridership)
end(ridership)

frequency(ridership) #the number of samples per unit time
deltat(ridership)  #time interval between observations
cycle(ridership) #gives the positions in the cycle of each observation

#Plot TS
ts.plot(ridership)

#lag plots
#each graph shows y(t) plotted against y(t-k)
#for different values of k

ridership2 = window(ridership, start = 2005)
forecast::gglagplot(ridership2)

#relationship is strongly positive at Lags 12 reflecting strong seasonality

#------------------------------------------------------
#          Autoregressive Model
#------------------------------------------------------

#AR using Arima
#no differencing
arima_100 = arima(ridership,order = c(1,0,0))
arima_100

#forecast the values
(forecasted_values = forecast::forecast(arima_100, 12))

accuracy(forecasted_values)
autoplot(forecasted_values)
#does not look very good

#Moving Average (MA) using Arima
arima_001 = arima(ridership, order = c(0,0,1))
arima_001

#forecast the values
forecasted_values = forecast(arima_001, 12)
forecasted_values
accuracy(forecasted_values)
autoplot(forecasted_values, lwd = 2)
#does not look very good

#we know from earlier that the data does not look stationary

#When AR or MA does not give desirable results

#------------------------------------------------------
#            Split Training/Test set
#------------------------------------------------------

training = subset(ridership, end = length(ridership)-27)
test = subset(ridership, start = length(ridership)-26)

cycle(training)
cycle(test)

#------------------------------------------------------
#                 Decomposition
#------------------------------------------------------

#visualize the various components of TS
amtrakdecom = decompose(training)
plot(amtrakdecom, lwd=4, col='blue')

#------------------------------------------------------
#             Is it stationary?
#------------------------------------------------------

fUnitRoots::adfTest(training)
#pvalue is 0.5575, cannot reject H0
#H0 = non-stationary
#H1 = stationary

training2 %>%
  ggtsdisplay(smooth = TRUE)

training2 %>%
  diff() %>%
  ggtsdisplay(smooth = TRUE)
#data now looks stationary
#ACF/PACF plots do not look ok and suggests seasonality
#Note that lag 0 is not shown on R

#------------------------------------------------------
#              Seasonal Arima
#------------------------------------------------------

#take an additional seasonal difference
#order 1

training %>%
  diff() %>%
  diff(lag = 12) %>%
  ggtsdisplay()
#there is a rather large spike over confidence interval at lag 12

#try order 2

training %>%
  diff() %>%
  diff(lag = 12) %>%
  diff(lag = 12) %>%
  ggtsdisplay()
#does not look any better
#makes it worse so go back to 1 seasonal diff

#There is a decline period in the original dataset which ends around Dec 1995 and the steady incline starts Jan 1996.
#try arima on the period beginning Jan 1996.

#creating a subset
rider2 = rider[-c(1:60),]

#renumbering the rows
row.names(rider2) <- 1:nrow(rider2)

ridership3 = ts(rider2$Ridership, frequency = 12, start = c(2010, 1))
ridership3

ts.plot(ridership3)

#------------------------------------------------------
#            Split Training/Test set
#------------------------------------------------------

dim(rider2) #99 observations

training2 = subset(ridership3, end = length(ridership3)-27)
test2 = subset(ridership3, start = length(ridership3)-26)

cycle(training2)
cycle(test2)

#------------------------------------------------------
#                 Decomposition
#------------------------------------------------------

#visualize the various components of TS
amtrakdecom2 = decompose(training2)
plot(amtrakdecom2, lwd=4, col='blue')



#------------------------------------------------------
#             Is it stationary?
#------------------------------------------------------

fUnitRoots::adfTest(training2)
#pvalue is 0.6682, cannot reject H0
#H0 = non-stationary
#H1 = stationary

training2 %>%
  diff() %>%
  ggtsdisplay()
#data now looks stationary
#ACF/PACF plots do not look ok and suggests seasonality
#Note that lag 0 is not shown on R

#------------------------------------------------------
#              Seasonal Arima
#------------------------------------------------------

#take an additional seasonal difference
#order 1

training2 %>%
  diff() %>%
  diff(lag = 12) %>%
  ggtsdisplay()

#------------------------------------------------------
#              Auto.Arima
#------------------------------------------------------

#what does auto.arima suggests?

(fitAutoArima = auto.arima(training2))

#pvalues
coeftest(fitAutoArima)

f4 = forecast(fitAutoArima, 12)
accuracy(f4)
plot(f4, lwd = 2)

#ar1 and sma1 statistically significant (nonseasonal (p) and seasonal(Q))

#------------------------------------------------------
#          Autoregressive Model
#------------------------------------------------------

#AR using Arima
#no differencing
arima2_100 = arima(ridership3,order = c(1,0,0))
arima2_100
#compare to arima_100 (aic = 2049.47): aic is lower at 1274.91, which suggest higher predictive/explanatory ability

#forecast the values
(forecasted_values2 = forecast::forecast(arima2_100, 12))

accuracy(forecasted_values2)
autoplot(forecasted_values2)
#does not look very good

#------------------------------------------------------
#          Moving Average Model
#------------------------------------------------------

#Moving Average (MA) using Arima
arima2_001 = arima(ridership3, order = c(0,0,1))
arima2_001

#forecast the values
forecasted_values3 = forecast(arima2_001, 12)
forecasted_values3
accuracy(forecasted_values3)
autoplot(forecasted_values3, lwd = 2)
#does not look very good

#try AR-1 and MA-1 for both normal and seasonal

model_train_sea = Arima(training2, order = c(1,1,1), seasonal = c(1,1,1))

summary(model_train_sea)

#p-values are missing from summary output so use coeftest()

lmtest::coeftest(model_train_sea)
#observe that ar1,sar1,sma1 are all statistically insignificant
#only ma1 is statistically significant (nonseasonal (q))

#check how many differencing needed

ndiffs(ridership3)
nsdiffs(ridership3)

#modeltuning: left with ma1

model_train_sea2 = Arima(training2, order = c(0,1,1), seasonal = c(0,1,0))
summary(model_train_sea2)
coeftest(model_train_sea2)
#AIC same as above

#try 011,011

model_train_sea3 = Arima(training2, order = c(0,1,1), seasonal = c(0,1,1))
summary(model_train_sea3)
coeftest(model_train_sea3)
#AIC lowest so far
#Nelson ran a forloop and found this too

#------------------------------------------------------
#              Exponential Time Series
#------------------------------------------------------

rider_ets = ets(training2)
summary(rider_ets)
checkresiduals(rider_ets)

rider_ets %>%
  forecast(h=26) %>%
  autoplot() + autolayer(test)

pred_test_ets = forecast(test, model = rider_ets)
accuracy(pred_test_ets)

#------------------------------------------------------
#             Holt-Winters
#------------------------------------------------------

rider_hw = hw(training2)
summary(rider_hw)
checkresiduals(rider_hw)

rider_hw %>%
  forecast(h=24) %>%
  autoplot() + autolayer(test)

pred_test_hw = forecast(test, model = rider_hw)
accuracy(pred_test_hw)
