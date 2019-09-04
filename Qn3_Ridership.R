# CA1_SB_PA: Question 3

# Load library
# install.packages("TSPred")
pacman::p_load(forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest, lubridate)
install.packages("lubridate")
library(lubridate)

rider = read.csv("AmtrakBig_CA_Question-3.csv", stringsAsFactors = F)

glimpse(rider)

rider$Date = strptime(paste("01", rider$Month), "%d %b-%y")
rider$Year = format(rider$Date, "%Y")

table(rider$Year, rider$Season)
# There is no missing gap in the dataset

rider_ts = ts(rider$Ridership, frequency = 12, start = c(2005, 1))
is.ts(rider_ts)
rider_ts
# acf(rider_ts) ; pacf(rider_ts)
#ggplot(NULL, aes(y = rider_ts, x = seq_along(rider_ts))) + geom_line()+geom_point() 


# Split TS data for Train and Test (year 2016, 2017 & 2018 Jan-Mar)
train = subset(rider_ts, end=length(rider_ts)-27)
test = subset(rider_ts, start=length(rider_ts)-26)

autoplot(rider_ts) ; cycle(rider_ts)
autoplot(train) ; cycle(train)
autoplot(test) ; cycle(test)

# Lag plots
gglagplot(rider_ts)
# There is evidence of seasonality at lag 12.
# Try seasonal differencing at lag 12

# Visualise the various components of TS
decompose(rider_ts) %>% plot()
# or plot(stl(rider_ts, s.window="periodic"))
# There is a downward trend from yer 2006 to 2010 and turned positive trend
# Try differencing by the order of d=1. Also try D=1 for seasonal effect
# Try AR(1)
# Try MA(1)

ndiffs(train)
nsdiffs(train)
# Simulate the order of differencing

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

# ggfortify::ggtsdiag(model_100)
# Ljung-Box Statistic still showing the residuals p-value <0.05, 
# rejecting the NULL Hypothesis,
# implying residual data are not independently distributed; they exhibit serial correlation.
# Note: ggfortify cause conflict with autoplot()

# Next do MA: Using arima p=1, q=1 (non-seasonal)
model_101 = Arima(train010_010, order=c(1,0,1))
checkresiduals(model_101)
tsdisplay(residuals(model_101)) # showing the PACF
# The ACF reduced. PACF also reduced, but the spike at lag 12 still exist,
# Try add a seasonal component at lag 12 (D=1)

# ggfortify::ggtsdiag(model_101)
# Ljung-Box Statistic now showed the residuals p-value > 0.05, 
# do not reject the NULL Hypothesis,
# implying residual data are independently distributed. 
# Note: ggfortify cause conflict with autoplot()


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
  autoplot()+autolayer(test)



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


# Try auto.arima
fitautoarima = auto.arima(train)
checkresiduals(fitautoarima)
tsdisplay(residuals(fitautoarima)) # showing the PACF
summary(fitautoarima)
coeftest(fitautoarima)
plot(fitautoarima$x, col="red") ; lines(fitted(fitautoarima), col="blue")
# Result of auto.arima is ARIMA(1,1,1)(0,1,1)[12]
# Same as manually tune model_111_011


# Test against Forecast for 27 months
fitautoarima %>%
  forecast(h=27) %>%
  autoplot() + autolayer(test)


# Try ETS
model_ETS = ets(train)
checkresiduals(model_ETS)
tsdisplay(residuals(model_ETS)) # showing the PACF
summary(model_ETS)
plot(model_ETS$x, col="red") ; lines(fitted(model_ETS), col="blue")


# Test against Forecast for 27 months
model_ETS %>%
  forecast(h=27) %>%
  autoplot() + autolayer(test)
# It is noted that the forecast did not track the test result closely


# Comparing the TS models, excluding fitautoarima which is same as model_111_011
# Predicting against Test data
pred_model_111_011 = predict(test, model = model_111_011)
pred_model_111_111 = predict(test, model = model_111_111)
pred_model_ETS = predict(test, model = model_ETS ,use.initial.values=TRUE)


accuracy(pred_model_111_011)
accuracy(pred_model_111_111)
accuracy(pred_model_ETS)

model_list = list(model_111_011=model_111_011, model_111_111=model_111_111, model_ETS=model_ETS) %>%
  enframe(name = 'modelName',value = 'model')

model_list$pred = list(pred_model_111_011, pred_model_111_111, pred_model_ETS)

#accuracy(model_list$pred[[1]])
#map(model_list$pred, accuracy)
#summary(model_list$model[[3]])


# pre_model_111_011 has the lowest errors measures except for RMSE
# which is higher than pre_model_ETS. 

# Ex Ante Forecast for 6 months ----------------------------------------------#

# Train the model based on the full set of data
modelF_111_011 = Arima(rider_ts, order = c(1,1,1), seasonal = c(0,1,1))
modelF_ETS = ets(rider_ts)

# Ex Ante Forecast for 6 months
fcst_111_011 = forecast(modelF_111_011, h=6)
fcst_ETS = forecast(modelF_ETS, h=6)


# save Ex Ante Forecast of the model in the new data frame 
# together with variable you want to plot against
F_111_011 = data.frame(fcst_111_011) %>% select(-2,-3)
F_111_011$Time = dmy(paste(1, rownames(F_111_011)))
F_111_011$Model = "ARIMA_111_011"
F_111_011 = F_111_011[,c(4,5,1,2,3)]
#F_111_011 = gather(F_111_011, "Type", "Forecast", 3:5)

F_ETS = data.frame(fcst_ETS) %>% select(-2,-3)
F_ETS$Time = dmy(paste(1, rownames(F_ETS)))
F_ETS$Model = "ETS"
F_ETS = F_ETS[,c(4,5,1,2,3)]
#F_ETS = gather(F_ETS, "Type", "Forecast", 3:5)

df = rbind(F_111_011, F_ETS, deparse.level = 0)

# Extract source ts as data frame
ts = window(rider_ts, start = 2017)
ds = data.frame(ts)
names(ds) = "Point.Forecast"
ds$Time = as.Date(ts)
ds$Point.Forecast = as.numeric(ds$Point.Forecast)
ds$Model = "Actual"
ds = ds[,c(2,3,1)]

ds_df = full_join(ds,df)


ds_df %>% ggplot(aes(x = Time, y=Point.Forecast, col = Model)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=Lo.95,ymax=Hi.95, fill = Model), linetype = 0, alpha=.2) +
  
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black")
  ) +
  
  labs(title="Ridership Analysis",
       subtitle= "6 months forecast of Ridership",
       y = "Count of Riders",
       x = "Date")

  
# Tabulate the Ridership estimate forecast for 6 months based on average of Point.Forecast of all forecast models
# For the maximum limit, obtain the highest of upper limit of 95% confidence internal of all forecast models
# For the minimum limit, obtain the highest of lower limit 95% confidence internal of all forecast models
x_table = df %>% 
  group_by(Time) %>%
  summarise(Estimates = round(mean(Point.Forecast)), 
            Min = round(max(Lo.95)), 
            Max = round(max(Hi.95)))

x_table$Time = format(x_table$Time, "%Y %b")

library(ggpubr)
table1 <- ggtexttable(x_table, 
                      theme = ttheme(
                        tbody.style = tbody_style(color = "black", face = "plain", size = 12, 
                                                  fill = "transparent", linewidth = 1, linecolor = "grey"),
                        colnames.style = colnames_style(color = "black", face = "bold", size = 12,
                                                        fill = "transparent", linewidth = 1, linecolor = "grey"),
                        rownames.style = colnames_style(color = "black", face = "plain", size = 12,
                                                        fill = "transparent", linewidth = 1, linecolor = "grey")))

table1




