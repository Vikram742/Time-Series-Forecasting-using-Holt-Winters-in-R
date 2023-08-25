# set working directory and read the data
data<- read.csv("MonthlySales.csv")
dim(data)
str(data)
#data cleaning
sum(is.na(data))
str(data)
#month column needs to be converted to date from character vector
library(lubridate)
data$month<-parse_date_time(data$month, orders = c('%Y-%m-%d', '%Y/%m/%d'))
str(data$month)
data$month<-as.Date(data$month)
#convert month column into a time series format
library(forecast)
?ts
y<-ts(data$sales, frequency = 12, start = c(2013,1))
#plot the data
plot.ts(y)
# there seems to be a clear seasonal pattern with higher sales being recorded in November and December followed
# by a fall in January. This is also visible with a jump in sales in March as compared to Feb. 

# let's split the data into train and test
training<- window(x = y , end = c(2016,2))
test<-window(x = y , start = c(2016,3))

#use holt winters function
model<-HoltWinters(training, seasonal = "additive")
model

# forecasting
predictions<-forecast(model, h=12)
predictions
# we predict the sales data from Mar'2016 to Feb'2017 
plot(predictions)
# The blue line indicates the predicted sales value with the darker grey shade indicating the sales value
# at lower and upper levels of 80% prediction level and lighter grey shade indicating the sales value at lower
# and upper levels of 95% prediction level 

accuracy(predictions$mean,test)
# RMSE is 14532.99 and MAPE is 18.69 (Accuracy is 81.31%)
# Here we try to forecast the sales without any external regressors and used only historical data to forecast
# the sales