library(fpp)
aus<-ausbeer
str(aus)
class(aus)
plot(aus)

ausdiff1<-diff(aus,differences = 1)
plot(ausdiff1)
mean(ausdiff1)

ausdiff2<-diff(aus,differences = 2)
plot(ausdiff2)
mean(ausdiff2)

ausdiff3<-diff(aus,differences = 3)
plot(ausdiff3)
mean(ausdiff3)

acf(ausdiff3,lag.max = 40)
#(0,1,3,4,5,7,8,9,11,12,13)

pacf(ausdiff3,lag.max = 40)
#(1,2,3,4,5,6,10)

ausseriesarima<-arima(aus,order=c(11,3,10))
ausseriesarima

library(forecast)
ausseriesarimaforecast<-forecast(ausseriesarima,h=24)
ausseriesarimaforecast
plot(ausseriesarimaforecast)


###using seasonal valused

#P=(1,2,3)
#Q=(1,2,3)

ausseriesarima1<-arima(aus,order=c(1,1,2),seasonal = list(order=c(0L,1L,1L)))
ausseriesarima1

library(forecast)
ausseriesarimaforecast1<-forecast(ausseriesarima1,h=24)
ausseriesarimaforecast1
plot(ausseriesarimaforecast1)

ausseriesarima2<-auto.arima(aus)

library(forecast)
ausseriesarimaforecast2<-forecast(ausseriesarima2,h=24)
ausseriesarimaforecast2
plot(ausseriesarimaforecast2)





