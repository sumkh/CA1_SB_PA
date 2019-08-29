# CA1_SB_PA: Question 3

# Load library
pacman::p_load(forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest)

#Testing 12345

rider = read.csv("AmtrakBig_CA_Question-3.csv", stringsAsFactors = F)

glimpse(rider)

rider$Date = strptime(paste("01", rider$Month), "%d %b-%y")
rider$Year = format(rider$Date, "%Y")

table(rider$Year, rider$Season)
# There is no missing gap in the dataset

rider_ts = ts(rider$Ridership, frequency = 12, start = c(2005, 1))
is.ts(rider_ts)
rider_ts

# Split TS data for Train and Test (year 2016, 2017 & 2018 Jan-Mar)
train = subset(rider_ts, end=length(rider_ts)-27)
test = subset(rider_ts, start=length(rider_ts)-26)

cycle(train); cycle(test)

autoplot(rider_ts)

# Lag plots
gglagplot(rider_ts)
# There is evidence of seasonality at lag 12.
# Try seasonal differencing at lag 12

# Visualise the various components of TS
decompose(rider_ts) %>% plot()
# There is a downward trend from yer 2006 to 2010 and turned positive trend
# Try differencing by the order of d=1. Also try D=1 for seasonal effect
# Try AR(1)
# Try MA(1)


# Train dataset
# Making Trend Stationary with Differencing
train %>% 
  diff() %>%
  ggtsdisplay(smooth = T)

train %>% 
  diff() %>%
  diff(lag = 12) %>% # additional seasonal differencing at lag 12
  ggtsdisplay(smooth = T)
# ACF improved with additional seasonal differencing at lag 12.
# Have d=1 and D=1 for ARIMA

# Keeping the TS after Trend is made stationary with differencing
train010_010 = train %>% diff() %>% diff(lag = 12)

plot(train010_010)


# Augmented Dickey-Fuller Test
# Prior to Differencing
adfTest(train)

# After Differencing
adfTest(train010_010)
# p-value <0.05, 
# we reject the NULL hypothesis that it is no-stationary time series


# AR: Using arima p=1 (non-seasonal)
model_100 = Arima(train010_010, order=c(1,0,0))
checkresiduals(model_100)
tsdisplay(residuals(model_100)) # showing the PACF
# The PACF spike in lag 1 is reduced, but the spike at lag 12 still exist.


# Next do MA: Using arima p=1, q=1 (non-seasonal)
model_101 = Arima(train010_010, order=c(1,0,1))
checkresiduals(model_101)
tsdisplay(residuals(model_101)) # showing the PACF
# The ACF reduced. PACF also reduced, but the spike at lag 12 still exist,
# Try add a seasonal component at lag 12 (D=1)


#Add Seasonal Components: Using arima Q=1
model_101_001 = Arima(train010_010, order=c(1,0,1), seasonal = c(0,0,1))
checkresiduals(model_101_001)
tsdisplay(residuals(model_101_001)) # showing the PACF
# The spike at lag 12 dropped below the threshold.

#Add Seasonal Components: Using arima Q=2
model_101_002 = Arima(train010_010, order=c(1,0,1), seasonal = c(0,0,2))
checkresiduals(model_101_002)
tsdisplay(residuals(model_101_002)) # showing the PACF
# There is no much changes to ACF and PCF, try P=1 instead.


#Add Seasonal Components: Using arima P=1, Q=1
model_101_101 = Arima(train010_010, order=c(1,0,1), seasonal = c(1,0,1))
checkresiduals(model_101_101)
tsdisplay(residuals(model_101_101)) # showing the PACF
# There is no much changes to ACF and PCF.

# Therefore, the ARIMA parameters for non-stationary train set wil be
# arima (p=1, d=1, q=1) * (P=0, D=1, Q=1)
model_111_011 = Arima(train, order=c(1,1,1), seasonal = c(0,1,1))
checkresiduals(model_111_011)
tsdisplay(residuals(model_111_011)) # showing the PACF
summary(model_111_011)
coeftest(model_111_011)
plot(model_111_011$x, col="red") ; lines(fitted(model_111_011), col="blue")

# Test against Forecast for 27 months
model_111_011 %>%
  forecast(h=27) %>%
  autoplot() + autolayer(test)


# Try to reduce the range of confidence interval for the forecast 
# by increasing the AR by an additional seasonal order
# Therefore, the ARIMA parameters for non-stationary train set wil be
# arima (p=1, d=1, q=1) * (P=1, D=1, Q=1)
model_111_111 = Arima(train, order=c(1,1,1), seasonal = c(1,1,1))
checkresiduals(model_111_111)
tsdisplay(residuals(model_111_111)) # showing the PACF
summary(model_111_111)
coeftest(model_111_111)
plot(model_111_111$x, col="red") ; lines(fitted(model_111_111), col="blue")


# Test against Forecast for 27 months
model_111_111 %>% 
  forecast(h=27) %>%
  autoplot() + autolayer(test)








