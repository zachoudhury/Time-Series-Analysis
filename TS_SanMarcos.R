library(zoo)
library(xts)
library(imputeTS)

setwd("E:/Machine Learning/AssignTS")

data <- read.csv("SanMarcos_csv.csv")

#Extract begin and end dates
bgn <- as.Date(data$datetime[1],format='%m/%d/%Y')
print(bgn)
end <- as.Date(data$datetime[length(data$datetime)],format='%m/%d/%Y')
print(end)
datex <- seq.Date(bgn,end,"day")#Generate sequence of dates 
pdatex <- as.Date(data$datetime,format='%m/%d/%Y')


#Check to see if there are missing values
theo <- length(datex)
actu <- length(data$datetime)
if(theo > actu) print("Some Values Are Missing")

#Create the time series graph
#Create a zoo object
WD.zoo <- zoo(data$discharge_cfps,pdatex)
dum.zoo <- zoo(,datex) #Dummy data-set with time alone
WD.zoo_m <- merge(dum.zoo, WD.zoo)
plot(WD.zoo_m,xlab='Year', ylab='Discharge (Cft/s)', main="San Marcos discharge (before imputation)")#Any visible periods of missing values?
summary(WD.zoo_m)#Check how many NAs

#interpolate for missing discharge values
WD.ts <- as.ts(WD.zoo_m) #convert to ts object
WD.ts_p <- na_kalman(WD.ts,model= "StructTS") #missing values imputation by Kalman smoothing
WD.zoo_p <- zoo(WD.ts_p,datex) #imputed object
plot(WD.zoo_p,xlab='Year',ylab='Discharge(cft/s)') 
summary(WD.zoo_p)

#perform 7-,30-,90-,365-days moving average
WD7d <- rollmean(WD.zoo_p,7,align='right')
plot(WD7d,xlab='Year',ylab='Discharge(cft/s)',main="7-days moving Average")
WD30d <- rollmean(WD.zoo_p,30,align='right')
plot(WD30d,xlab='Year',ylab='Discharge(cft/s)',main="30-days moving Average")
WD90d <- rollmean(WD.zoo_p,90,align='right')
plot(WD90d,xlab='Year',ylab='Discharge(cft/s)',main="90-days moving Average")
WD365d <- rollmean(WD.zoo_p,365,align='right')
plot(WD365d,xlab='Year',ylab='Discharge(cft/s)',main="365-days moving Average")

#calculate autocorrelation 
acf(WD.zoo_p,lag.max=NULL,main='ACF for SanMarcos', type = c('correlation'))
pacf(WD.zoo_p,main='PACF for SanMarcos',type="o")

