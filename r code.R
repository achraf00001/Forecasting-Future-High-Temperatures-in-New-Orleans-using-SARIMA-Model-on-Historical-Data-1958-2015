# install.packages("forecast")
library(forecast)

# Time series data of Airline Passengers--Many TS textbooks have illustrated this data
data("AirPassengers")
APorig <- AirPassengers
APorig
# View(AP) # Data has 144 entries
str(APorig) # Data from 1949 to 1961, a total of 144 monthly observations
head(APorig)
ts(APorig, frequency = 12, start=c(1949,1))

plot(APorig) # Note that the TS is not stationary in Mean and also in variance

# Log transform
APlog <- log(APorig)
plot(APlog) # Notice the seasonality and non-stationarity in mean

# ACF and PACF plots
acf(APlog, main='Autocorrelations', ylab='',
    ylim=c(-1, 1), ci.col = "black") # Note the non-stationality--also evident from plot of TS above
pacf(APlog, main='Partial Autocorrelations', ylab='',
     ylim=c(-1, 1), ci.col = "black")

# Decomposition of additive time series
decomp <- decompose(APlog)
decomp$figure # This gives monthly decomposition of percentage higher or lower compared to average
# The plot below shows the seasonal variation more clearly-- November has the least airline passengers
plot(decomp$figure,
     type = 'b',
     xlab = 'Month',
     ylab = 'Seasonality Index',
     col = 'blue',
     las = 2)

plot(decomp) 
#This gives the additive decomposition of the log transformed TS--trend, seasonal and random components

# Differencing to achieve stationarity
APlogd1 <- lag(APlog, 1) - APlog
plot(APlogd1)
acf(APlogd1, main='Autocorrelations', ylab='', ylim=c(-1, 1), ci.col = "black") # stationary?

acf(APlogd1, main='Autocorrelations', ylab='', lag.max=36,
    ylim=c(-1, 1), ci.col = "black") # non-stationary in seasonality is evident

pacf(APlogd1, main='Partial Autocorrelations', ylab='', lag.max=36, ylim=c(-1, 1), ci.col = "black")


# Once-difference data
APL11 <- diff(APlog, differences = 1) # Another way to do differencing
plot(APL11) 

# Seasonal difference the differenced Airline data
APd1D12 <- diff(APlogd1, lag = 12)
acf(APd1D12, main='Autocorrelations', ylab='',lag.max=36,
    ylim=c(-1, 1), ci.col = "black") # Note the seasonal non-stationality has been eliminated

pacf(APd1D12, main='Partial Autocorrelations', ylab='', lag.max=36,
     ylim=c(-1, 1), ci.col = "black")

#Identify a few potential Time series models based on ACF and PACF of APd1D12 data  
# Model1 could be ARIMA(0,1,1)X(0,1,1)12
# Model2 could be ARIMA(1,1,1)X(0,1,1)12
# Model3 could be ARIMA(2,1,1)X(0,1,1)12
# Model4 could be ARIMA(0,1,1)X(1,1,1)12
#Model5 could be ARIMA(2,1,1)(0,1,0)[12] 

# AUTO FITTING ARIMA - Autoregressive Integrated Moving Average Model
# It is also based on ACF and PACF plots
model5 <- auto.arima(APlog)
model5 # This is same as Model1 abvove

model6 <- auto.arima(APorig) # What if we autofit without doing the homework about stationarity?
model6 # This model violates the stationarity assumption.

# How to get online help with the ARIMA function
?Arima()

# Fit Model1 could be ARIMA(0,1,1)X(0,1,1)12 to first few years of AirPassengers data
Model1 <- Arima(window(APorig,end=1956+11/12),order=c(0,1,1),
                seasonal=list(order=c(0,1,1),period=12),lambda=0)
# Lambda is Box-Cox transformation parameter. If lambda="auto", then a transformation is automatically selected using BoxCox.lambda. The transformation is ignored if NULL. Otherwise, data transformed before model is estimated.
# Note that Lambda=0 means log transformation under Box-Cox transformations.
Model1
plot(forecast(Model1,h=48))
lines(AP)

# Illustration: How to apply fitted model to later data
# You can use below to create Training and Testing Data
Model1Fitted <- Arima(window(AirPassengers,start=1958),model=Model1)
Model1Fitted

attributes(Model1Fitted)
Model1Fitted$fitted
length(Model1Fitted$x)


Model1Fitted$fitted
Model1Fitted$x
length(Model1Fitted$x)
APorig

RMSE_calculated <- ((sum((Model1Fitted$fitted - Model1Fitted$x)^2))/length(Model1Fitted$x))^0.5
MAE_calculated <- (sum(abs(Model1Fitted$fitted - Model1Fitted$x)))/length(Model1Fitted$x)

# in-sample one-step forecasts.
accuracy(Model1)
accuracy(Model1Fitted)
# Forecast accuracy measures on the log scale.
#accuracy(forecast(Model1,h=48,lambda=NULL), log(window(APorig,start=1957))) # call this eq (1)

# Arima Model output Details
summary(Model1) # Note the training set error measure
Model1$coef[1] # Gives the first fitted coefficient

#Residual Analysis
acf(fit$resid)
# ACF and PACF plots
acf(Model1$residuals, main = 'Correlogram')
pacf(Model1$residuals, main = 'Partial Correlogram' )

# Ljung-Box test 
# The Ljung-Box test is a type of statistical test of whether any of a group of autocorrelations of a time series are different from zero.
Box.test(Model1$residuals, lag=12, type = 'Ljung-Box')

# Residual plot
hist(Model1$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)
lines(density(Model1$residuals))

# In case you want to learn more about the hist command
?hist()

plot(Model1$residuals) # We will use this to look for volatility in future

# Forecast
f <- forecast(Model1, 48)
library(ggplot2)
autoplot(f)
accuracy(f) # Should be same as what we saw above in eq (1)

