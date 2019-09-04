pacman::p_load(data.table,forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest, readxl, ggpubr, reshape2)

setwd("C:/Users/nelso/Documents/Masters/EBA5002/CA Doc/data")

amtrak = read.csv("AmtrakBig_CA_Question-3.csv")

head(amtrak)
#convert to ts

amtrakts = ts(amtrak$Ridership, start = c(2005,1), frequency = 12)
amtrakts
autoplot(amtrakts)

#there is yearly seasonality. and uptrend starts at 2010.

uptrend = window(amtrakts, start = c(2010,1))
frequency(amtrakts)
gglagplot(amtrakts)
ggseasonplot(uptrend)

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

## Create an arima model for each of them, given that d,D = 1.
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

nullvals = !sapply(arimalist, is.null)
fitlist_nonull = arimalist[nullvals]
para_nonull = arima_para[nullvals,]

aicc = list()
for (i in 1:length(fitlist_nonull)) {
  aicc[[i]] <- fitlist_nonull[[i]][["aicc"]]
}

aicc = as.numeric(aicc)
fcerrors = as.data.frame(t(sapply(fitlist_nonull,'errorcalc',test)))

#checkparameters of optimized model
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

autoplot(window(amtrakts, start = c(2014,1))) +
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

selectedmodels[[length(selectedmodels)+1]] <- fitets

#use tsCV to check our models. Start with ets.
myforecast = function(y,h) {
  forecast(ets(y),h = h)
}
tsCVets = tsCV(amtrakts, myforecast, h = 12)

RMSEcalc = function(a) {
  sqrt(mean(a^2, na.rm = TRUE))
}

MAEcalc = function(a) {
  mean(abs(a), na.rm = TRUE)
}

# compute RMSE for forecast window. seems like only checking h=1 is enough.
apply(b, 2, RMSEcalc)

#tsCV on arima(212,212)
myforecast = function(y,h){
  forecast(arima(y, order = c(2,1,2), seasonal = c(2,1,2)),h = h)
}

tsCVarima212212 = tsCV(amtrakts, myforecast, h = 1)

myforecast = function(y,h){
  forecast(arima(y, order = c(1,1,1), seasonal = c(0,1,1)),h = h)
}

tsCVarima111011 = tsCV(amtrakts, myforecast, h = 1)
RMSE = list()
RMSE[["ets"]]<- RMSEcalc(tsCVets[,"h=1"])
RMSE[["arima212212"]]<- RMSEcalc(tsCVarima212212)
RMSE[["arima111011"]]<- RMSEcalc(tsCVarima111011)

MAE = list()
MAE[["ets"]]<- MAEcalc(tsCVets[,"h=1"])
MAE[["arima212212"]]<- MAEcalc(tsCVarima212212)
MAE[["arima111011"]]<- MAEcalc(tsCVarima111011)

no.NAs = list()
no.NAs[["ets"]]<- sum(is.na((tsCVets[,"h=1"])))
no.NAs[["arima212212"]]<- sum(is.na((tsCVarima212212)))
no.NAs[["arima111011"]]<- sum(is.na((tsCVarima111011)))

errors = as.data.frame(cbind(RMSE,MAE,no.NAs))
colnames(errors) = c("RMSE","MAE","no. of NAs")
errors = errors %>%
  rownames_to_column("model") %>%
  gather("errortype","value",-model)

errors %>%
  ggplot(aes(x = model)) +
  geom_col(aes(y=value, fill=model)) +facet_wrap(~errortype) +
  labs(title = "ts Cross Validation results")

#retrain complete model to forecast 6 months forward

fitarima_complete = arima(amtrakts, order = c(1,1,1), seasonal = c(0,1,1))
fitets_complete = ets(amtrakts)

autoplot(window(amtrakts, start = c(2014,1))) +
  autolayer(forecast(fitarima_complete,9), series = 'Arima 111011', PI = FALSE, colour = "red") +
  autolayer(forecast(fitets_complete,9), series = 'ETS "AAA"', PI = FALSE, colour = "green")

p1 = autoplot(window(amtrakts, start = c(2014,1))) +
  autolayer(forecast(fitarima_complete,9), series = 'Arima 111011', PI = TRUE, colour = "red") + 
  labs(title = "forecast with Arima111011")

p2 = autoplot(window(amtrakts, start = c(2014,1))) +
  autolayer(forecast(fitarima_complete,9), series = 'Arima 111011', PI = TRUE, colour = "green") + 
  labs(title = "forecast with ets_AAA")

ggarrange(p1,p2,nrow = 2, ncol = 1)
