pacman::p_load(data.table,forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest, readxl, ggpubr, reshape2)

setwd("C:/Users/nelso/Documents/Masters/EBA5002/CA Doc/data")

amtrak = read.csv("AmtrakBig_CA_Question-3.csv")

head(amtrak)
#convert to ts

amtrakts = ts(amtrak$Ridership, start = c(2005,1), frequency = 12)
amtrakts
autoplot(amtrakts)

frequency(amtrakts)
gglagplot(amtrakts)
#there is yearly seasonality. and uptrend starts at 2010.

uptrend = window(amtrakts, start = c(2010,1))

#setup training data
training = window(amtrakts, end = end(amtrakts)-11/12)
test = window(amtrakts, start = end(amtrakts)-11/12)                  

fitAutoArima_all = auto.arima(training)
fitAutoArima_all

#only on uptrend

training2 = window(uptrend, end = c(2015,12))
test2 = window(uptrend, start = c(2016,1))

fitAutoArima_up = auto.arima(training2)
fitAutoArima_up

fitArima011011 = arima(training2, c(0,1,1), seasonal = c(0,1,1))
fitArima011011

fitets = ets(training2)
fitets

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

#Plot the fits we have based on error.
fitlist = list(fitAutoArima_up,fitArima011011, fitets)
error_list = as.data.frame(t(sapply(fitlist,'errorcalc')))
fitrows = c('AutoArima','Arima011011','ets')
error_list$Fits <- as.factor(fitrows)

#plot the errors
error_list %>%
  melt(id.vars = "Fits") %>%
  ggplot(aes(x=variable, y = value)) + 
  geom_bar(aes(fill = Fits), stat = 'identity', position = 'dodge') + 
  labs(title = "Errors on train set")

autoplot(window(amtrakts, start = c(2010,1))) +
  autolayer(forecast(fitAutoArima_up,length(test2)), series = "AutoArima", PI=FALSE) +
  autolayer(forecast(fitArima011011,length(test2)), series = "Arima011011", PI = FALSE) +
  autolayer(forecast(fitets,length(test2)), series = "ets", PI = FALSE)

fcerrors = as.data.frame(t(sapply(fitlist,'errorcalc',test2)))
fcerrors$Fits <- as.factor(fitrows)

fcerrors %>%
  melt(id.vars = "Fits") %>%
  ggplot(aes(x=variable, y = value)) + 
  geom_bar(aes(fill = Fits), stat = 'identity', position = 'dodge') + 
  labs(title = "Errors on test set")

## Run all arima models for selection
runarima = function(training, a, d){
  fit = Arima(training, c(a[1],a[2],a[3]), seasonal = c(a[4],a[5],a[6]))
  return(fit)
}

## Create an arima model for each of them
arimalist = list()
for (i in 1:nrow(arima_para)) {
  tryCatch( {
    arimalist[[i]] <- runarima(training2,as.numeric(arima_para[i,]),1)
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
chosenmodel = fitlist_nonull[[ind]]

fc1 = forecast(chosenmodel, 27)
fcerrors = as.data.frame(t(sapply(fitlist_nonull,'errorcalc',test2)))

#checkparameters of optimized model
para_nonull[which.min(fcerrors$RMSE),]
para_nonull[which.min(fcerrors$MASE),]
