pacman::p_load(data.table,forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest, readxl, ggpubr, reshape2)

#set wd to this R file's current folder.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

amtrak = read.csv("AmtrakBig_CA_Question-3.csv")

head(amtrak)
#convert to ts

amtrakts = ts(amtrak$Ridership, start = c(2005,1), frequency = 12)
amtrakts
autoplot(amtrakts) + labs(y="Ridership") +
  annotate("rect", xmin = 2008, xmax = 2011, ymin = 1200, ymax = 2500,
           alpha = .2, fill = "yellow") +
  annotate("rect", xmin = 2010, xmax = 2016, ymin = 1200, ymax = 2500,
           alpha = .1, fill = "blue") + 
  annotate("rect", xmin = 2016, xmax = 2018.25, ymin = 1200, ymax = 2500,
           alpha = .2, fill = "blue") + 
  scale_x_continuous(breaks = seq(2005, 2019, by = 1)) +
  annotate("segment", x = 2010, y = 2500, xend = 2018.25, yend = 2500, arrow = arrow(ends = "both", type = "open")) +
  annotate("segment", x = 2008, y = 1800, xend = 2011, yend = 1500, color = "red", arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  annotate("text", x = c(2009.5,2013,2017.15, 2014), y = c(2400,2400,2400,2550), label = c("Declining Trend","Training Data","Test Data","Modelling Data"), size = 8)
plot(decompose(amtrakts))
#there is yearly seasonality. and uptrend starts at 2010.

uptrend = window(amtrakts, start = c(2010,1))
frequency(amtrakts)
gglagplot(amtrakts)
ggseasonplot(uptrend) + labs(y="Ridership") 

#plot tsdisplay
ggtsdisplay(uptrend, smooth = TRUE)

#non-stationarity, try 1st order differencing.
uptrend %>%
  diff() %>%
  ggtsdisplay(smooth = TRUE)

#ACF Lag plots show seasonality at 12.

uptrend %>%
  diff() %>%
  diff(lag = 12) %>%
  ggtsdisplay(smooth = TRUE)

#try ndiffs and NSdiffs
ndiffs(uptrend)
nsdiffs(uptrend)

training = window(uptrend, end = c(2015,12))
test = window(uptrend, start = c(2016,1))

#compute errors and return as a vector for comparison.
errorcalc = function(fit,test){
  #test is the ts you want to test against
  if (missing(test)) {
    #compute accuracy and return vector
    acc = accuracy(fit)
    return(acc[1,])
  } else {
    fc = forecast(fit,length(test))
    acc = accuracy(fc,test)
    return(acc[2,])
  }
}
## Run all arima models for selection
# runarima 
runarima = function(training, a, d){
  if (!missing(d)) {
    fit = Arima(training, c(a[1],d,a[2]), seasonal = c(a[3],d,a[4]))
    return(fit)
  } else {
    fit = Arima(training, c(a[1],a[2],a[3]), seasonal = c(a[4],a[5],a[6]))
    return(fit)
  }
}

# Create an arima model for each of them, given that d,D = 1.
#run total of 81 sets of parameters for p,q,P,Q %in% 0:2
arima_para = expand.grid(rep(list(0:2), 4))
arimalist = list()
for (i in 1:nrow(arima_para)) {
  tryCatch( {
    arimalist[[i]] <- runarima(training,as.numeric(arima_para[i,]),1)
  },
  error = function(cond) {
    return(NA)
  }
  )
}

#Check null models. These are models that might be unstable and couldn't be built (no suitable parameters found)
nullvals = !sapply(arimalist, is.null)
fitlist_nonull = arimalist[nullvals]
para_nonull = arima_para[nullvals,]

aicc = list()
for (i in 1:length(fitlist_nonull)) {
  aicc[[i]] <- fitlist_nonull[[i]][["aicc"]]
}

#visualize AICC
aicc = as.numeric(aicc)
fcerrors = as.data.frame(t(sapply(fitlist_nonull,'errorcalc',test)))

#checkparameters of optimized model, selected based on comparison with test set or AICC.
#RMSE, MAE, AICC
para_nonull[which.min(fcerrors$RMSE),]
para_nonull[which.min(fcerrors$MAE),]
para_nonull[which.min(aicc),]

plotarima = list(fitlist_nonull[[which.min(fcerrors$RMSE)]],fitlist_nonull[[which.min(fcerrors$MAE)]],fitlist_nonull[[which.min(aicc)]])

plotarimaerror = as.data.frame(t(sapply(plotarima,'errorcalc',test)))
fitrows = c('Min RMSE','Min MAE','Min AICc')
plotarimaerror$Fits <- as.factor(fitrows)

plotarimaerror %>%
  melt(id.vars = "Fits") %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_bar(aes(fill = Fits), stat = 'identity', position = 'dodge') + 
  labs(title = "Errors on test set") + facet_wrap(~variable, scale = "free")

#Compare to test set.
autoplot(window(amtrakts, start = c(2014,1))) + labs(y="Ridership") +
  autolayer(forecast(plotarima[[1]],length(test)), series = 'Min RMSE', PI = FALSE) +
  autolayer(forecast(plotarima[[2]],length(test)), series = 'Min MAE', PI = FALSE) +
  autolayer(forecast(plotarima[[3]],length(test)), series = 'Min AICc', PI = FALSE) +
  annotate("rect", xmin = 2016, xmax = 2018.5, ymin = 1500, ymax = 2300,
           alpha = .1, fill = "yellow") +
  annotate("text", x = 2016.5, y = 2250, label = c("Test Period"), size = 10)

#Selection of Min MAE (111,011) and Min RMSE (212,212) as model.
selectedmodels = list(fitlist_nonull[[which.min(fcerrors$RMSE)]],fitlist_nonull[[which.min(fcerrors$MAE)]])

#try out ets model. Recalling previous decomposition plots, we should expect additive seasonality and trend components.
#i.e our ets model should yield ets(-,A,A)

fitets = ets(training)
fitets = ets(training, model = "ZAA")
#model chose AAA ets model. 
#try ZMM, ZAM, ZMA models, AIC did not improve.

ets(training, model = "ZMM")
ets(training, model = "ZMA")

selectedmodels[[length(selectedmodels)+1]] <- fitets

# Plot forecast from ETS(A,A,A)
autoplot(window(amtrakts, start = c(2010,1))) + labs(y="Ridership") +
  autolayer(forecast(fitets,length(test)), PI = TRUE) +  
  labs(title = "Forecast from ETS(A,A,A)")

#use tsCV to check our models. Start with ets.
myforecast = function(y,h) {
  forecast(ets(y, model = "ZAA"),h = h)
}

RMSEcalc = function(a) {
  sqrt(mean(a^2, na.rm = TRUE))
}

MAEcalc = function(a) {
  mean(abs(a), na.rm = TRUE)
}

NAcalc = function(a) {
  sum(is.na(a))
}
tsCVets = tsCV(uptrend, myforecast, h = 12)

# compute RMSE/MAE/NA for forecast window. We also remove the first 14 figures as arima cannot build a model without 1 year of seasonality.
tsCVets = window(tsCVets, start = c(2011,3))

RMSEets = apply(tsCVets, 2, RMSEcalc)
MAEets = apply(tsCVets, 2, MAEcalc)
NAets = apply(tsCVets, 2, NAcalc)

#tsCV on arima(212,212)
myforecast = function(y,h){
  forecast(arima(y, order = c(2,1,2), seasonal = c(2,1,2)),h = h)
}

tsCVarima212212 = tsCV(uptrend, myforecast, h = 12)
tsCVarima212212 = window(tsCVarima212212, start = c(2011,3))

RMSEarima2 = apply(tsCVarima212212, 2, RMSEcalc)
MAEarima2 = apply(tsCVarima212212, 2, MAEcalc)
NAarima2 = apply(tsCVarima212212, 2, NAcalc)

myforecast = function(y,h){
  forecast(arima(y, order = c(1,1,1), seasonal = c(0,1,1)),h = h)
}

tsCVarima111011 = tsCV(uptrend, myforecast, h = 12)
tsCVarima111011 = window(tsCVarima111011, start = c(2011,3))

RMSEarima1 = apply(tsCVarima111011, 2, RMSEcalc)
MAEarima1 = apply(tsCVarima111011, 2, MAEcalc)
NAarima1 = apply(tsCVarima111011, 2, NAcalc)

RMSE = as.data.frame(cbind(RMSEets,RMSEarima2,RMSEarima1))
colnames(RMSE) = c("etsAAA","arima212212","arima111011")
RMSE = RMSE %>%
  rownames_to_column("h") %>%
  mutate(error = "RMSE")

MAE = as.data.frame(cbind(MAEets,MAEarima2,MAEarima1))
colnames(MAE) = c("etsAAA","arima212212","arima111011")
MAE = MAE %>%
  rownames_to_column("h") %>%
  mutate(error = "MAE")

NAs = as.data.frame(cbind(NAets,NAarima2,NAarima1))
colnames(NAs) = c("etsAAA","arima212212","arima111011")
NAs = NAs %>%
  rownames_to_column("h") %>%
  mutate(error = "NAs")

errors = rbind(RMSE,MAE,NAs) %>%
  melt(id.vars = c("h","error"), variable.name = "model")

errors$h %>%
  str_extract("\\d{1,2}") %>%
  as.integer() -> errors$h

errors %>%
  filter(error == "NAs") %>% filter(h == 1) %>%
  ggplot(aes(x = model)) +
  geom_col(aes(y=value, fill=model)) + facet_wrap(~error) +
  labs(title = "TS Cross Validation Results") + xlab("Model") + ylab("Number of Unstable Models") +
  scale_fill_manual(values = c("#22b8d6", "#4922d6","#228b75"))

errors %>%
  filter(error != "NAs") %>% filter(model != "arima212212") %>%
  ggplot(aes(x = h, y=value, color=model)) +
  geom_smooth(se = FALSE, size = 1.5) +
  geom_point(size = 2) + 
  facet_wrap(~error, nrow = 3) +
  labs(title = "TS Cross Validation Results") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_color_manual(values = c("#22b8d6", "#228b75"))

#retrain complete model to forecast 6 months forward

fitarima_complete = arima(uptrend, order = c(1,1,1), seasonal = c(0,1,1))
fitets_complete = ets(uptrend, model = "ZAA")


autoplot(window(amtrakts, start = c(2016,1))) + labs(y="Ridership") +
  autolayer(forecast(fitarima_complete,12), series = 'Arima 111011', PI = FALSE, alpha = 0.5) +
  autolayer(forecast(fitets_complete,12), series = 'ETS "AAA"', PI = FALSE, alpha = 0.5) +
  scale_color_manual(values = c("red","green"))+
  annotate("rect", xmin = 2018.25, xmax = 2018.75, ymin = 1700, ymax = 2400,
           alpha = .2, fill = "blue") +
  annotate("rect", xmin = 2018.75, xmax = 2019.25, ymin = 1700, ymax = 2400,
           alpha = .1, fill = "yellow") +
  annotate("text", x = c(2018.5,2019), y = c(1800,1800), label = c("Apr-Sep '18","Oct-Mar '19"), size = 6) + 
  labs(title = "Amtrak ridership forecast")

p1 = autoplot(window(amtrakts, start = c(2014,1))) + labs(y="Ridership") +
  autolayer(forecast(fitarima_complete,12), series = 'Arima 111011', PI = TRUE, colour = "red") + 
  labs(title = "forecast with Arima111011")

p2 = autoplot(window(amtrakts, start = c(2014,1))) + labs(y="Ridership") +
  autolayer(forecast(fitarima_complete,12), series = 'ets_AAA', PI = TRUE, colour = "green") + 
  labs(title = "forecast with ets_AAA")

forecastarima = forecast(fitarima_complete, 12)
forecastets = forecast(fitets_complete,12)

ggarrange(p1,p2,nrow = 2, ncol = 1)
