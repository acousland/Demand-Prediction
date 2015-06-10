############################################
# Demand Prediction Example
# - Prediction
#
# Aaron Cousland
# 9/6/2015
############################################

rm(list=ls())

Sys.setenv(TWO_TASK="//WPRORA001:1521/RTV")

require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates
#require (TTR)          
require (reshape)      # Data preperation
require (dygraphs)     # For nice time series graphs
require (forecast)     # Forecasting
#require (nnet)         # Nerual Network Package
#require (caret)

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
energy.log <- sqlQuery(local.connection,"SELECT * from dw.METER_REG_ACTIVE_READ_48@CODSUP1.WORLD
                       where METER_REG_ACTIVE_READING_DT between to_date('07/06/2015','DD/MM/YYYY') and to_date('01/12/2015','DD/MM/YYYY')
                       AND NMI_ID = '6407580165';")
odbcCloseAll()

# Transform results
energy.log <- subset(energy.log,select = -c(METER_REG_ACTIVE_READ_48_SK,SERVICE_POINT_SK,METER_ID,METER_SK,REGISTER_ID,REGISTER_SK,ESTIMATE_READ_BITMASK_IND,MTR_RG_ACT_NET_ENGY_MAXDLY_KWH,ROW_INSERT_DTM,ROW_UPDATE_DTM,ROW_EFFECTIVE_DTM,ROW_EFFECTIVE_UTC_OFFSET_VAL,ROW_EXPIRATION_DTM,ROW_EXPIRATION_UTC_OFFSET_VAL,ROW_CURRENT_IND,ETL_PROCESS_RUN_SK,MTR_REG_ACT_NET_ENGY_DAILY_KWH))
energy.log <- melt(energy.log, id=c("NMI_ID","METER_REG_ACTIVE_READING_DT"))
energy.log$TS <- ymd(energy.log$METER_REG_ACTIVE_READING_DT)+hours(trunc(as.integer(substr(energy.log$variable,start=20,stop=21))/2))+minutes(((as.integer(substr(energy.log$variable,start=20,stop=21))/2)%%1)*60)-minutes(30) 
energy.log <- subset(energy.log,select = -c(METER_REG_ACTIVE_READING_DT,variable))

# Change energy to demand
power.log <- energy.log
rm(energy.log)
power.log$value <- power.log$value*2
colnames(power.log) <- c("NMI","Demand","TS")

#ts(subset(power.log,select = -c(NMI,TS)),start=c(2015,365),end=c(2015,365))

# Change results into a time series
power.log.xts <- xts(power.log, order.by = power.log$TS)
power.log.xts <- subset(power.log,select = -c(NMI,TS))


time_index <- seq(from = force_tz(min(power.log$TS),"AEST"),
                  to = force_tz(max(power.log$TS),"AEST"),
                  by = "30 min")

power.log.xts <- xts(power.log$Demand, order.by = time_index)

power.log.ts <- ts(power.log$Demand, frequency=30)

power.log.ts <- msts(power.log$Demand,seasonal.periods=c(48,336))

# Set plotting parameters
par(mfrow=c(3,1)) 
forecast.periods = 100

# Perform Holt-Winters forecasting
power.forecast.hw <- HoltWinters(power.log.ts, beta=FALSE, gamma=FALSE)
plot.forecast(forecast.HoltWinters(power.forecast.hw,h=forecast.periods))

# Perform TBATS forecasting
#power.forecast.tbats <- tbats(power.log.ts)
#plot.forecast(forecast.bats(power.forecast.tbats,h=forecast.periods))

# Perform ARIMA (Autoregressive integrated moving average) forecasting
power.forecast.arima <- auto.arima(power.log.ts,approximation=FALSE,trace=FALSE)
plot.forecast(forecast.Arima(power.forecast.arima,h=forecast.periods))

# Perform ETS (Exponential smoothing state space) forecasting
power.forecast.ets <- ets(power.log.ts)
plot.forecast(forecast.ets(power.forecast.ets,h=forecast.periods))
