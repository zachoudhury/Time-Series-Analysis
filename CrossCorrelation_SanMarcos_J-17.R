##Cross-correlation 

library(zoo)
library(xts)
library(imputeTS)

setwd("E:/Machine Learning/AssignTS")

data1 <- read.csv("J-17.csv")

#Extract begin and end dates
bgn <- as.Date(data1$DailyHighDate[1],format='%m/%d/%Y')
print(bgn)
end <- as.Date(data1$DailyHighDate[length(data1$DailyHighDate)],format='%m/%d/%Y')
print(end)
datex <- seq.Date(bgn,end,"-1 day")#Generate sequence of dates, increment -1 day because of reverse chronology
pdatex <- as.Date(data1$DailyHighDate,format='%m/%d/%Y')

#Create a zoo object
WL.zoo <- zoo(data1$WaterLevelElevation,pdatex)
dum.zoo <- zoo(,datex) #Dummy data-set with time alone
WL.zoo_m <- merge(dum.zoo, WL.zoo)
plot(WL.zoo_m,xlab='Year', ylab='Water Level (ft)')#Look for visible periods with missing values
summary(WL.zoo_m)#Check how many NAs

#Interpolation for missing water-level values
WL.ts <- as.ts(WL.zoo_m) #Conversion to ts object
WL.ts_p <- na_kalman(WL.ts,model= "StructTS") #Missing values imputation by Kalman smoothing
WL.zoo_p <- zoo(WL.ts_p,datex) #convert back to zoo object
plot(WL.zoo_p,xlab='Year',ylab='WaterLevel(ft)') 
summary(WL.zoo_p)

##San Marcos
data2 <- read.csv("SanMarcos_csv.csv")

#Extract begin and end dates
bgn <- as.Date(data2$datetime[1],format='%m/%d/%Y')
print(bgn)
end <- as.Date(data2$datetime[length(data2$datetime)],format='%m/%d/%Y')
print(end)
datex <- seq.Date(bgn,end,"day")#Generate sequence of dates 
pdatex <- as.Date(data2$datetime,format='%m/%d/%Y')


#Create a zoo object
WD.zoo <- zoo(data2$discharge_cfps,pdatex)
dum.zoo <- zoo(,datex) #Dummy data-set with time alone
WD.zoo_m <- merge(dum.zoo, WD.zoo)
plot(WD.zoo_m,xlab='Year', ylab='Discharge (Cft/s)')#see if any visible periods of missing values
summary(WD.zoo_m)#Check how many NAs

#interpolate for missing discharge values
WD.ts <- as.ts(WD.zoo_m) #convert to ts object
WD.ts_p <- na_kalman(WD.ts,model= "StructTS") #missing value imputation by Kalman smoothing
WD.zoo_p <- zoo(WD.ts_p,datex) #imputed object
plot(WD.zoo_p,xlab='Year',ylab='Discharge(cft/s)') 
summary(WD.zoo_p)

#Find cross-correlation
ccf(WL.zoo_p, WD.zoo_p)







