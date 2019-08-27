# CA1_SB_PA: Question 3

# Load library
pacman::p_load(forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest)
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

# Making Trend Stationary with Differencing
rider_ts %>% 
  diff() %>%
  ggtsdisplay(smooth = T)

rider_ts %>% 
  diff() %>%
  diff(lag = 12) %>% # additional seasonal differencing at lag 12
  ggtsdisplay(smooth = T)
# ACF improved with additional seasonal differencing at lag 12.
# Have d=1 and D=1 for ARIMA

# Keeping the TS after Trend is made stationary with differencing
TSrider010_010 = rider_ts %>% diff() %>% diff(lag = 12)

plot(TSrider010_010)
adfTest(TSrider010_010)
# Augmented Dickey-Fuller Test, p-value <0.05, 
# we reject the NULL hypothesis that it is no-stationary time series


