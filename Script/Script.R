install.packages("fracdiff")
library(fracdiff)
pacman::p_load("RMySQL", "forecast", "urca", "dplyr", "chron", "lubridate", "plotly", "padr", "imputeTS", "ggplot2", "ggfortify")

# Getting the data using SQL ####

# Start by establishing a connection to the database
con = dbConnect(MySQL(), user = 'deepAnalytics', password = 'Sqltask1234!',
                dbname = 'dataanalytics2018', 
                host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# list tables contained in the database
dbListTables(con)
dbListFields(con,'iris')
irisALL<- dbGetQuery(con, "SELECT * FROM iris")
irisSELECT<- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
            # tested how it worked with iris

dbListFields(con, 'yr_2006')

yr2006<- dbGetQuery(con, "SELECT Date, Time, Global_Active_Power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr2007<- dbGetQuery(con, "SELECT Date, Time, Global_Active_Power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr2008<- dbGetQuery(con, "SELECT Date, Time, Global_Active_Power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr2009<- dbGetQuery(con, "SELECT Date, Time, Global_Active_Power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr2010<- dbGetQuery(con, "SELECT Date, Time, Global_Active_Power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

# Investigate 2006. Contains only 16/12-2006 to 31/12-2006
summary(yr2006)

str(yr2006)
head(yr2006)
tail(yr2006)

# Investigate 2007. 365 days = 525600 obs. Not all is included, some missing observations
summary(yr2007)
str(yr2007)
head(yr2007)
tail(yr2007)

# Investigate 2008. 366 days = 527040 obs. Not all included, some missing observations
summary(yr2008)
str(yr2008)
head(yr2008)
tail(yr2008)

# Investigate 2009. Not all included, some missing observations
summary(yr2009)
str(yr2009)
head(yr2009)
tail(yr2009)

# Investigave 2010. Not all included. Only until 26/11/2010
summary(yr2010)
str(yr2010)
head(yr2010)
tail(yr2010)

# Creating a dataframe with the 3 almost complete datasets. The dates are in the correct order.

workingDf<- bind_rows(yr2007, yr2008, yr2009, yr2010)
workingDf$Global_Active_Power<- ((workingDf$Global_Active_Power)*1000)/60

summary(workingDf)
str(workingDf)
head(workingDf)
tail(workingDf)



# Preprocessing ####
dateAndTime<- cbind(workingDf, dateTime = paste(workingDf$Date, workingDf$Time), stringsAsFactors = FALSE)

dateAndTime<- dateAndTime[, c(ncol(dateAndTime), 1:(ncol(dateAndTime)-1))]

# Changing the format of the DateTime column
dateAndTime$dateTime<- as.POSIXct(dateAndTime$dateTime, "%Y/%m/%d %H:%M:%S")

# Add time zone
attr(dateAndTime$dateTime, "tzone")<- "Europe/Paris"

str(dateAndTime)

dateAndTime<- dateAndTime %>% pad(break_above = 3)
dateAndTime<- na_interpolation(dateAndTime, option = "stine")

# Using lubridate to create an attribute for "year"

dateAndTime$year<- year(dateAndTime$dateTime)
dateAndTime$quarter<- quarter(dateAndTime$dateTime)
dateAndTime$month<- month(dateAndTime$dateTime)
dateAndTime$week<- week(dateAndTime$dateTime)
dateAndTime$weekday<- weekdays(dateAndTime$dateTime)
dateAndTime$hour<- hour(dateAndTime$dateTime)
dateAndTime$day<- day(dateAndTime$dateTime)
dateAndTime$minute<- minute(dateAndTime$dateTime)

dateAndTime$month<- as.integer(dateAndTime$month)
dateAndTime$week<- as.integer(dateAndTime$week)
# Perform initial exploration of the data ####
summary(dateAndTime)


houseWeek<- filter(dateAndTime, year == 2008 & week == 2)
plot(houseWeek$Sub_metering_1)


houseDay<- filter(dateAndTime, year == 2008 & month == 1 & day == 9 &
                    (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40
                     | minute == 50))

plot_ly(houseDay, x = ~houseDay$dateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', 
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~ houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th 2008", 
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (Watt-Hours)"))


houseJune2009<- filter(dateAndTime, year == 2009 & month == 6 & week == 22) 


plotly(houseJune2009, x=~ houseJune2009$dateTime, y= ~houseJune2009$Sub_metering_1, name = 'Kitchen',
       type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~ houseJune2009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~ houseJune2009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption June 2009 week 1",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (Watt-hours"))  # FIGURE OUT WHY IT DOES NOT WORK



# More plots


# Prepare to analyze the data, making time series in a list ####

group<- c("year", "month", "week", "day")
granularity<- list()

data_year2 <-dateAndTime %>% select(c("dateTime","Global_Active_Power","Sub_metering_1",
                                      "Sub_metering_2","Sub_metering_3")) %>% as_tibble() 
data_year3 <- data_year2

for (i in group) {

  data_year3$dateTime <- floor_date(x = data_year2$dateTime, unit = i)
  
  granularity[[i]] <- data_year3 %>% group_by(dateTime) %>% 
    summarise(active_power= sum(Global_Active_Power), sub1 = sum(Sub_metering_1),sub2 = sum(Sub_metering_2),sub3 = sum(Sub_metering_3))

}

lapply(granularity, function(x) ggplot(x, aes(sub2)) + geom_density())


# Choosing granularity and parameters ####

TS<- ts(granularity[["month"]][["active_power"]], frequency = 12, start = c(2007,01)) #Change parameters
decomposedTS<- decompose(TS)
decomposedTS # It is additive
plot(decomposedTS) # Notice that it has a clear seasonal pattern

  

adjustForSeasonality<- TS - decomposedTS$seasonal
train<- window(adjustForSeasonality, end = c(2009, 12))
test<- window(adjustForSeasonality, start = c(2010, 01))

# Start modelling with the HoltWinters ####

forecasting<- train %>% HoltWinters()
forecasting
plot(forecasting)

forecastingPred<- forecast(forecasting, h=10)
forecastingPred$fitted
plot(forecastingPred)
accuracy(forecastingPred, test)     # MAPE for HW with MonthACTIVE = 7

checkresiduals(forecasting)
forecasting$x
forecastingPred$residuals
# Modelling ARIMA #### 
arimaModel<- auto.arima(train)
arimaModel
plot(arimaModel)

arimaPred<- forecast(arimaModel, h=10)
plot(arimaPred)
accuracy(arimaPred, test)
checkresiduals(arimaModel)


# Forecasting for time period out of my dataset ####

forecastingOutOfDataset<- TS %>% HoltWinters()
forecastingOutOfDataset
plot(forecastingOutOfDataset)

forecastingIntoTheFuture<- forecast(forecastingOutOfDataset, h=12)
plot(forecastingIntoTheFuture)

forecastingIntoTheFuture<- as.data.frame(forecastingIntoTheFuture)
forecastingIntoTheFuture$kWh<- forecastingIntoTheFuture$`Point Forecast`/1000
forecastingIntoTheFuture$cost<- forecastingIntoTheFuture$kWh*0.1472

forecastingIntoTheFuture$ID<- seq(1:nrow(forecastingIntoTheFuture))



# Write csv-file ####
write.csv(forecastingIntoTheFuture, file = "activePowerMonth.csv")
