data("gas")
G <- gas
ts(G, frequency = 12, start= c(1956,1))
attributes(G)   
plot(G)

?gas
#Decomposition 
decom <- decompose(G)
plot(decom)

#Seasonally adjusting
gasSeasonallyAdjusted <- G - decom$seasonal
plot(gasSeasonallyAdjusted, main= "Seasonlly Adjusted")

#PART-B

GSA <- gasSeasonallyAdjusted
ts(GSA, start= c(1949))
plot(GSA, main= "Graph for AirPassengers data")
pacf(GSA, main="Partial Autocorrelation plot")
acf(GSA, main="Autocorrelation plot")

#create ARIMA Model
arima <- arima(AP, order = c(1,1,1))
arima

f<-forecast(arima, h=15)
f

plot(f, main="Forecast for ARIMA Model")
