#######
#######
#######
#######
#######
#######
#######
####### Time Series Model for S&P 500
#######
#######
#######
#######
#######
#######
#######



##
####
######
#     LOADING DATA, PACKAGES
######
####
##
print("Time Series Model for S&P 500")
# Add the directory path of data_1/data_master_1.csv file
wd <- getwd()
setwd("../../")
parent <- getwd()
setwd(wd)
print(parent)
# Load the data file
dataMaster <- read.csv(file.path(parent, "data_1/data_master_1.csv"))


attach(dataMaster)

# install.packages() the following packages, run this on the terminal

# forecast
# astsa



# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("astsa")
# install.packages("car")
# install.packages("MTS")

# load the packages
require(forecast)
require(astsa)

# outputting work

pdf("timeSeries_sp_500.pdf")


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     TESTING that data loads properly
######
####
##

head(dataMaster)
str(dataMaster)

# assigning data column to variable container

sp_500_nominal <- dataMaster$sp_500
sp_500_nominal

# printing string and mean of gdp_us_nominal

mean(sp_500_nominal)


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     CREATING TIME-SERIES OBJECTS WITH FINANCIAL DATA 
######
####
##


sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################


##
####
######
#     TIME SERIES STUFF
######
####
##

sp_500
# First we plot the time series plot to get an understanding of the necessary modeling
plot.ts(sp_500, main = "Time Series Plot for S&P 500")
# Looking at the time series we notice there's obvious trend so there will be issues with stationary 
# but with the plotting of the acf and pacf plot of the original data set we can make sure!
acf2(sp_500)
# So these plots confirm our time series object is not stationary, so we will make the appropriate 
# transformation; differencing. 

diff1 <- diff(sp_500)

# Now we plot the transformed time series object to confirm it is stationary!

plot(diff1, main = "Time Series plot of the First Difference")

# The plot suggests stationarity so we look at the acf and pacf plot to estimate our Box-Jenkins model!

acf2(diff1)

# Our plots show characteristics of a moving average model with one significant lag so it would be a MA(1)
# But since we took the first difference we have to take that into consideration so it will be an ARIMA(0, 1, 1)!

# Before we start our diagnostics and in depth time series analysis, we are going to create a training set and 
# test set. The test set will consist of the year 2015, so we can forecast that year and see how well our model 
# did with real time data. 

dataMaster_TS <- dataMaster[-c(1:240), ]
dataMaster_2014df <- dataMaster[-c(241:252), ]
dataMaster_TR <- ts(dataMaster_2014df, start = c(1995, 1), freq = 12)
sp500_TR <- dataMaster_TR[, 'sp_500']
sp500_TR

# Now that the sets are created we fit the model with the training set that contains all but 2015!
# We also used the auto.arima function in the forecast package to confirm that the model we chose was the best!
auto.arima(sp500_TR)

# The auto.arima function gave us the same function we decided except it included drift which makes sense so we
# included that in our final function of ARIMA(0, 1, 1) with Drift!

fit <- Arima(sp500_TR, order = c(0, 1, 1), include.drift = TRUE)

# We create an object with the residuals to use for our analysis on the residuals

resArima <- residuals(fit)

# We now look at residuals to make sure they are idependent and identically distributed white noise
# So we should get residuals that appear random and are normally distributed!

tsdisplay(resArima)

# Here we can see that the residuals appear to be white noise so our model is good to go!
# But to visualize it more we decided to create a histogram of the residuals and the histogram should look 
# like a normal distribution (i.e. bell shaped curve)

hist(resArima, prob = TRUE, main = "Histogram of the ARIMA residuals")
lines(density(resArima))

# Not perfect but we do see they follow a normal distribution so we can continue onto forecasting!

# The first step we took was creating a time series object that has the actual 2015 values!
act_sp500_2015_ts <- ts(dataMaster_TS$sp_500, start = c(2015, 1), freq = 12)
act_sp500_2015_ts

# Now we forecast our fitted model using the forecast function!!

plot(forecast(fit, h = 12), xlab = "Years",
     ylab = "S&P Values")
lines(act_sp500_2015_ts, col = "red")

# So it looks like it predicted it pretty well, but the graph is not zoomed in enough to get a good visual aid
# Our next step was to zoom in on the plot which can be done by adding limits to the x axis and y axis

plot(forecast(fit, h = 12), xlim = c(2015, 2016), ylim = c(1600, 2400), xlab = "Years",
     ylab = "S&P Values")
lines(act_sp500_2015_ts, col = "red")

# Thus we can see that the forecast follows the actual values pretty well!

# So our final step was comparing our model to other models to see which had the lowest (best) accuracy measument 
# criterion. So we used several other time series models and compared their Mean Absolute Error and their 
# Mean Absolute Percentage Error using the accuracy function in the forecast package.

accuracy(forecast(fit))
accuracy(forecast(meanf(sp500_TR)))
accuracy(forecast(naive(sp500_TR)))
accuracy(forecast(snaive(sp500_TR)))
accuracy(forecast(ets(sp500_TR)))

# Thus we can see our ARIMA Model had the best results so we conclude that our ARIMA(0, 1, 1) with Drift has the best forecast abilities!
# Now we plot the forecast for the year 2016 to see 
fit2015 <- Arima(sp_500, order = c(0, 1, 1), include.drift = TRUE)
plot(forecast(fit2015))
plot(forecast(fit2015), xlim = c(2015, 2017), ylim = c(1300, 2600))

# Thus we can see that the forecast predicts more increasing trend for the year 2016!! 
# There is much literature over the unreliability of using statistical tools to predict the stock market because of its uncertainability 
# We feel like with a time series analysis we can still gain an understanding of maybe how the U.S. economy can go. 
# We have also provided another iteration of this project that goes into more detail and is catered to people who have more experience with R
# The second iteration employs ggplot2 and other forecasting methods, but we felt this iteration was an appropriate project for people with some
# experience with R and time series. 
