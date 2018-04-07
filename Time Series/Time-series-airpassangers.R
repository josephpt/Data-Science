data("AirPassengers")
plot(AirPassengers)
View(AirPassengers)
air=AirPassengers
View(air)
class(air)
# the data set is in ts format so moving to step 2.
#step 2: differentiating the time series to get the stationarity in data

airdiff1<-diff(air,differences = 1)
plot.ts(airdiff1)
mean(airdiff1)
airdiff2<-diff(air,differences = 2)
plot.ts(airdiff2)
mean(airdiff2)
airdiff3<-diff(air,differences = 3)
plot.ts(airdiff3)
mean(airdiff3)

#we take difference=2

acf(airdiff2,lag.max = 40)
#acf(0,1,2,4,8,10,12,14)

pacf(airdiff2,lag.max = 40)

#pacf(1,2,4,7,9,10,11,12)

#we try each combination
#Note: p=8,q=9 indicates the effects are due to 8 and 9 months before.
airseriesarima<-arima(air,order=c(8,2,9))
airseriesarima

library(forecast)
airseriesarimaforecast<-forecast(airseriesarima,h=24)
airseriesarimaforecast
plot(airseriesarimaforecast)

#or
airseriesarima<-arima(air,order=c(10,2,11))
airseriesarima

library(forecast)
airseriesarimaforecast<-forecast(airseriesarima,h=24)
airseriesarimaforecast
plot(airseriesarimaforecast)
######OR with sasonality

airseriesarima<-arima(air,order=c(1,2,1),seasonal = list(order=c(1L,2L,1L)))
airseriesarima

library(forecast)
airseriesarimaforecast<-forecast(airseriesarima,h=24)
airseriesarimaforecast
plot(airseriesarimaforecast)

###### Auto.Arima is used to do ARIMA automatically

airseriesarima1<-auto.arima(air)

library(forecast)
airseriesarimaforecast<-forecast(airseriesarima1,h=24)
airseriesarimaforecast
plot(airseriesarimaforecast)









