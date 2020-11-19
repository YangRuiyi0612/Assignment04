# Set working dir
setwd("D:/Assignment/R_Assignment04")
# Load libraries
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

# 2.1
# Read the hourly data 
Met_Data       <- read.csv(file = "2281305.csv", header = T)
Data           <- as_tibble(Met_Data)
# Get variable names
names(Data)

Useful_Data  <- Data              %>%
   mutate(tem=substr(TMP,2,5))     %>%
   mutate(quality=substr(TMP,7,7)) %>%
   filter(tem!=9999)               %>%
   filter(quality==1)              

# Temperature
BaoAn_T        <- Useful_Data    %>%
   select(DATE,TMP)               %>%
   mutate(date=as.Date(paste(substr(DATE,1,7),"01",sep="-")))   %>%
   select(date,TMP)               %>%
   mutate(tem=as.numeric(substr(TMP,3,5))/10)                   %>%
   group_by(date)                 %>%
   summarize(mon_tem=mean(tem))   %>%
   mutate(year=substr(date,1,4))
tail(BaoAn_T)

# Time series
Tmp <- ts(BaoAn_T$mon_tem,start=c(2010,1),frequency=12,end=c(2020,8))
# Quick plot
plot(Tmp, type="o")

#2.2
Tmp_components <- decompose(Tmp)
plot(Tmp_components)

# Plot hist
hist(Tmp_components$random, prob=TRUE,ylim=c(0,0.4))

# Add pdf
curve(dnorm(x, mean=mean(Tmp_components$random,na.rm=T),
            sd=sd(Tmp_components$random,na.rm=T)),
      add=TRUE, col="red")

#2.3
data_adjusted  <-  Tmp_components$seasonal
plot(data_adjusted)
# Check acf and pacf
tsdisplay(data_adjusted)
#忽略季节性的影响
data_tmp1=diff(Tmp,12)
tsdisplay(data_tmp1)


tmp1_components <- decompose(data_tmp1)
plot(tmp1_components)
# Automated forecasting using an ARIMA model
auto.arima(Tmp,trace=T)

# Series: Tmp
# ARIMA(0,0,2) with drift 
fit <- Arima(Tmp,order=c(0,0,2),seasonal=c(1,1,1))

#2.4
# Make predictions
days_forecast  <- 2   #向前预测2月
days_in_plot   <- 24  #画多少个月，最后一月前24个月
forecast_2months <- forecast(fit, days_forecast)

# Plot predictions along with real values
plot(forecast(fit, days_forecast), include = days_in_plot, xlab="Time", 
     ylab="log(global cases)",type="o",lwd=2) #灰色95置信区间，浅灰80置信区间

#Real value
Met_Data1       <- read.csv(file = "5949.csv", header = T)
Data1           <- as_tibble(Met_Data1)
# Get variable name

Useful_Data1  <- Data1              %>%
   mutate(tem=substr(TMP,2,5))     %>%
   mutate(quality=substr(TMP,7,7)) %>%
   filter(tem!=9999)               %>%
   filter(quality==1)              

# Temperature
BaoAn_T1        <- Useful_Data1    %>%
   select(DATE,TMP)               %>%
   mutate(date=as.Date(paste(substr(DATE,1,7),"01",sep="-")))   %>%
   select(date,TMP)               %>%
   mutate(tem=as.numeric(substr(TMP,3,5))/10)                   %>%
   group_by(date)                 %>%
   summarize(mon_tem=mean(tem))   %>%
   mutate(year=substr(date,1,4))


Real_value_Sep <- BaoAn_T1 %>%
   filter(date=="2020-09-01"|date=="2020-10-01")

# Relative bias:0.03 %(Sep)and0.09%(Oct)
Relative_bias_9=(28.8-28.72)/28.8
Relative_bias_10=(25.8-26.04)/25.8



  