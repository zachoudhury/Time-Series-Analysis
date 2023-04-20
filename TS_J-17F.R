library(zoo)
library(xts)
library(imputeTS)

setwd("E:/Machine Learning/AssignTS")

data <- read.csv("J-17.csv")

#Extract begin and end dates
bgn <- as.Date(data$DailyHighDate[1],format='%m/%d/%Y')
print(bgn)
end <- as.Date(data$DailyHighDate[length(data$DailyHighDate)],format='%m/%d/%Y')
print(end)
datex <- seq.Date(bgn,end,"-1 day")#Generate sequence of dates, increment -1 day because of reverse chronology
pdatex <- as.Date(data$DailyHighDate,format='%m/%d/%Y')


#Check to see if there are missing values
theo <- length(datex)
actu <- length(data$DailyHighDate)
if(theo > actu) print("Some values are missing!!")

###Create the time series graph
#Create a zoo object
WL.zoo <- zoo(data$WaterLevelElevation,pdatex)
dum.zoo <- zoo(,datex) #Dummy data-set with time alone
WL.zoo_m <- merge(dum.zoo, WL.zoo)
plot(WL.zoo_m,xlab='Year', ylab='Water Level (ft)',main="Water Level in J-17 (before imputation)")#Look for visible periods with missing values
summary(WL.zoo_m)#Check how many NAs

#Interpolation for missing water-level values
WL.ts <- as.ts(WL.zoo_m) #Conversion to ts object
WL.ts_p <- na_kalman(WL.ts,model= "StructTS") #Missing values imputation by Kalman smoothing
WL.zoo_p <- zoo(WL.ts_p,datex) #convert back to zoo object
plot(WL.zoo_p,xlab='Year',ylab='WaterLevel(ft)', main="Water Level in J-17 after Imputation") 
summary(WL.zoo_p)

#perform 7-,30-,90-,365-days moving average
WL7d <- rollmean(WL.zoo_p,7,align='right')
plot(WL7d,xlab='Year',ylab='WaterLevel(ft)',main="7-days moving Average")
WL30d <- rollmean(WL.zoo_p,30,align='right')
plot(WL30d,xlab='Year',ylab='WaterLevel(ft)',main="30-days moving Average")
WL90d <- rollmean(WL.zoo_p,90,align='right')
plot(WL90d,xlab='Year',ylab='WaterLevel(ft)',main="90-days moving Average")
WL365d <- rollmean(WL.zoo_p,365,align='right')
plot(WL365d,xlab='Year',ylab='WaterLevel(ft)',main="365-days moving Average")


#Calculation of autocorrelation 
acf(WL.zoo_p,lag.max=NULL,main='ACF for J-17', type = c('correlation'))
pacf(WL.zoo_p,main='PACF for J-17',type="o")

