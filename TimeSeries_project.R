library(astsa)
library(tseries)
library(TSA)
library(forecast)
# Data Exploration
data <- read.csv("teleco_time_series .csv",TRUE,",")
plot(data, type = "l")
head(data)
# Data Cleaning
colSums(is.na(data))
anyDuplicated(data$Day)
data$Day
summary(data)

ts_data <- ts(data$Revenue, frequency = 731/24)
ploting_ts_data <- ts(data$Revenue, frequency = 1)
# Checking Stationary
adf.test(ts_data)
acf2(ts_data)

# Train Test Set
train <- ts(ts_data[1:548])
test <- ts(ts_data[549:length(ts_data)], start = 549)
write.csv(ts_data, "TimeSeries_Clean.csv")
write.csv(train, "Train.csv")
write.csv(test, "Test.csv")

test
#Decomposed time series
seasonal <- ts(train, frequency = 12)
decomp <- decompose(seasonal)
plot(decomp)

# Auto Correlation Function
acf2(train)

#Spectral Analysis
spectrum(train)

#Prediction
sarima_model <- Arima(train, 
                      order = c(1, 0, 0),            
                      seasonal = list(order = c(0, 1, 0), period = 12))
sarima_model
summary(sarima_model)
checkresiduals(sarima_model)


#Plot
sarima.for(train, n.ahead = 183,plot.all = TRUE, p = 1,d = 0, q =0, P =0, D = 1, Q = 0, S = 12)
lines(test, col = "blue")
legend("topleft", legend = c("Forecast","Test","Training"), 	col=c("red", 	"blue","black"), lty = c(1,1))

# Future
sarima.for(ploting_ts_data, n.ahead = 365, plot.all = TRUE, p = 1 , d= 0, q = 0, P = 0, D = 1, Q = 0, S = 12)
lines(test, col = "blue")
legend("topleft", legend = c("Forecast","Test","Training"), 	col=c("red", 	"blue","black"), lty = c(1,1))
                                                                                                      




