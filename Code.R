install.packages("forecast")
install.packages("quantmod")
install.packages("tseries")
install.packages("timeSeries")

library(forecast)
library(quantmod)
library(tseries)
library(timeSeries)
library(xts)

# Variable plug-ins
stockname = 'AMZN'
startdate = '2015-01-01'
enddate = '2019-01-01'

# Pull data from Yahoo finance
stockvar = getSymbols(c(stockname) , src = 'yahoo', from = startdate, to = enddate, auto.assign = FALSE)
stockvar = na.omit(stockvar)
#Chart time series
chartSeries(stockvar, theme = "black", name = c(stockname)[1])
# Pull close price series at Column 4 of the Yahoo Finance dataset
price = stockvar[, 4]

# Decompose data
stockvar.ts = ts(price, start=2015-01-02, frequency=120)
stockvar.de = decompose(stockvar.ts )
plot (stockvar.de)

par (mfrow = c(2,2))
#Logarithmic Returns
logprice = log(price)
plot(logprice, type = 'l', xlab = 'Time', ylab = 'Log (Price)', main = 'Logarithmic Price Returns')
#Square Root Values
sqrtprice = sqrt(price)
plot(sqrtprice, type = 'l', xlab = 'Time', ylab ='Sqrt(Price)', main = 'Square Root Price Returns')

#Differenced Logarithmic Price Returns
dlogprice = diff(log(price), lag=1)
dlogprice = dlogprice [!is.na (dlogprice)]
plot(dlogprice, type = 'l', xlab = 'Time', ylab = 'Log(Price)', main = 'Differenced Logarithmic Price Returns')

#Differenced Square Root Price Returns
dsqrtprice = diff(sqrt(price), lag=1)
dsqrtprice = dsqrtprice[!is.na(dsqrtprice)]
plot(dsqrtprice, type = 'l', xlab = 'Time', ylab = 'Sqrt(Price)', main = 'Differenced Square Root Price Returns')

#ADF Test
print(adf.test(logprice))
print(adf.test(sqrtprice))
print(adf.test(dlogprice))
print(adf.test(dsqrtprice))

par(mfrow=c(1,2))
#ACF and PACF for log data
acf(dlogprice, main = 'ACF for Logarithmic Price Returns')
pacf(dlogprice, main = 'PACF for Logarithmic Price Returns')

par (mfrow=c(1,2))
#ACF and PACF for log data
acf (dsqrtprice, main = 'ACF for Square Root Price Returns')
pacf (dsqrtprice, main = 'PACF for Square Root Price Returns')

# Initialize real log returns via xts
realreturn = xts(0,as.Date("2018-11-25","%Y-%m-%d"))
# Initialize forecasted returns via dataframe
forecastreturn = data.frame (Forecasted = numeric())

split = floor(nrow (dlogprice)*(2.9/3))
for (s in split:(nrow (dlogprice)-1)) 
  {
    dlogprice_training = dlogprice [1:s,]
    dlogprice_testing = dlogprice [(s+1):nrow(dlogprice), ]
    fit = arima(dlogprice_training, order = c(2, 0, 2), include.mean=FALSE) 
    summary (fit)
    
    arima.forecast = forecast (fit, h = 1) 
    summary(arima.forecast)
    
    Box.test(fit$residuals, lag=1, type='Ljung-Box')
    
    forecastreturn = rbind (forecastreturn, arima.forecast$mean[1])
    colnames (forecastreturn) = c("Forecasted")
    returnseries = dlogprice[(s+1),]
    realreturn = c(realreturn, xts(returnseries))
    rm(returnseries)
}

realreturn = realreturn [-1]
forecastreturn = xts(forecastreturn, index(realreturn))
plot (realreturn, type = 'l', main = 'Actual Returns (Black) Vs Forecast Returns (Red)')
lines (forecastreturn, lwd = 2, col = 'red')
?lines
comparison = merge (realreturn, forecastreturn)
comparison$Accuracy = sign(comparison$realreturn) == sign(comparison$Forecasted) 
print(comparison)

Accuracy_percentage = sum(comparison$Accuracy == 1)*100/ length(comparison$Accuracy) 
print(Accuracy_percentage)
