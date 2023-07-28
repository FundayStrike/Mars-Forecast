# comment out the next two code after installing the packages

# to run in other IDEs other than RStudio
# install.packages("languageserver")

# for time series analysis
# install.packages("forecast")

library(forecast)

# importing data from csv file  
mars <- read.csv("mars-weather.csv", header = TRUE, sep = ",")

# output initial number of rows in the data
num_rows <- nrow(mars)
print(sprintf("Number of rows: %d", num_rows))

# including important columns
mars <- mars[, c("terrestrial_date", "min_temp", "max_temp", "pressure", "atmo_opacity")]

# dropping rows with missing values
mars <- na.omit(mars)
print(sprintf("Number of rows after dropping missing values: %d", nrow(mars)))

# convert terrestrial_date (character) to date format
mars$terrestrial_date <- as.Date(mars$terrestrial_date)

# reversing the data set such that the head contains the oldest date
mars <- mars[seq(dim(mars)[1],1),]

# checking first 6 rows for sanity check
print(head(mars))

# seperate each plots into different rows
par(mfrow=c(2, 1))

# plotting min_temp (blue) and max_temp (red)
#plot(mars$terrestrial_date, mars$min_temp, type = "l", col = "blue", xlab = "Date", ylab = "Temperature (Celsius)", main = "Temperature of Mars over Time", ylim = c(-90, 10))
#lines(mars$terrestrial_date, mars$max_temp, col = "red")

# plotting pressure
#plot(mars$terrestrial_date, mars$pressure, type="l", col = "blue", xlab = "Date", ylab = "Pressure (Pa)", main = "Pressure of Mars over Time")

# partioning data into training and testing set (70/30)
train <- mars[1:round(0.7*nrow(mars)), ]
test <- mars[(round(0.7*nrow(mars))+1):nrow(mars), ]

print('Training set:')
print(head(train))

print('Testing set:')
print(head(test))

print(sprintf("Train count: %d", nrow(train)))
print(sprintf("Test count: %d", nrow(test)))

# print(train)

# using time series analysis to forecast min_temp
min_temp_ts <- ts(train$min_temp, frequency = 365, start = c(2012, 228))
# print(min_temp_ts)
# forecasting future min temperature values using ARIMA model
min_temp_forecast <- forecast(min_temp_ts, h = (365/300) * nrow(test), method = "arima")
# print(min_temp_forecast)


# using time series analysis to forecast max_temp
max_temp_ts <- ts(train$max_temp, frequency = 365, start = c(2012, 220))
# forecasting future max temperature values using ARIMA model
max_temp_forecast <- forecast(max_temp_ts, h = (365/300) * nrow(test), method = "arima")

# plotting forecasted min_temp
plot(min_temp_forecast, main = "Forecasted Minimum Temperature of Mars")

# plotting forecasted max_temp
plot(max_temp_forecast, main = "Forecasted Maximum Temperature of Mars")

# save train and test sets as a csv file
write.csv(train, file = "train.csv")
write.csv(test, file="test.csv")