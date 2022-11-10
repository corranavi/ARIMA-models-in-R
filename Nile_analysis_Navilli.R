rm(list=ls())

#import library forecast
library(forecast)

library(urca)

set.seed(42)

#import dataset Nile
data(Nile)

#Visualize infos about the TS
str(Nile.describe)
ggtsdisplay(Nile)

#hypothesis testing
summary(ur.kpss(Nile))
#checking the differencing degree 
ndiffs(Nile)

#differencing
Nile.diff<-diff(Nile,1)
ggtsdisplay(Nile.diff)

#verifiy stationarity with hypothesis testing
summary(ur.kpss(Nile.diff))
#______________________________________________________________________________

#buid the models MA(1),AR(1),AR(2),ARMA(1,1)

ma1<-arima(Nile.diff, order=c(0,0,1), include.mean = FALSE)
ma1
autoplot(ma1)
checkresiduals(ma1)
autoplot(forecast(ma1,5))

ar1<-arima(Nile.diff, order=c(1,0,0), include.mean = FALSE)
ar1
autoplot(ar1)
checkresiduals(ar1)
autoplot(forecast(ar1,5))

ar2<-arima(Nile.diff, order=c(2,0,0), include.mean = FALSE)
ar2
autoplot(ar2)
checkresiduals(ar2)
autoplot(forecast(ar2,5))

arma11<-arima(Nile.diff, order=c(1,0,1), include.mean = FALSE)
arma11
autoplot(arma11)
checkresiduals(arma11)
autoplot(forecast(arma11,5))

#________________END MODEL SELECTION_________________________________________