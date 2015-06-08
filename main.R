Sys.setenv(TWO_TASK="//WPRORA001:1521/RTV")

require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates
require (TTR)
require (reshape)
require (dygraphs)
require (forecast)

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
energy.log <- sqlQuery(local.connection,"SELECT * FROM SPOUSTIE.METER_REG_ACTIVE_READ_48 WHERE NMI_ID = '6407101859';")
odbcCloseAll()

# Transform results
energy.log <- subset(energy.log,select = -c(METER_REG_ACTIVE_READ_48_SK,SERVICE_POINT_SK,METER_ID,METER_SK,REGISTER_ID,REGISTER_SK,ESTIMATE_READ_BITMASK_IND,MTR_RG_ACT_NET_ENGY_MAXDLY_KWH,ROW_INSERT_DTM,ROW_UPDATE_DTM,ROW_EFFECTIVE_DTM,ROW_EFFECTIVE_UTC_OFFSET_VAL,ROW_EXPIRATION_DTM,ROW_EXPIRATION_UTC_OFFSET_VAL,ROW_CURRENT_IND,ETL_PROCESS_RUN_SK,MTR_REG_ACT_NET_ENGY_DAILY_KWH))
energy.log <- melt(energy.log, id=c("NMI_ID","METER_REG_ACTIVE_READING_DT"))
energy.log$TS <- ymd(energy.log$METER_REG_ACTIVE_READING_DT)+hours(trunc(as.integer(substr(energy.log$variable,start=20,stop=21))/2))+minutes(((as.integer(substr(energy.log$variable,start=20,stop=21))/2)%%1)*60)-minutes(30) 
energy.log <- subset(energy.log,select = -c(METER_REG_ACTIVE_READING_DT,variable))

# Change energy to demand
power.log <- energy.log
power.log$value <- power.log$value*2

# Change results into a time series
power.log.ts <- xts(power.log, order.by = power.log$TS)
power.log.ts <- subset(power.log.temp,select = -c(NMI_ID,TS))

time_index <- seq(from = as.POSIXct("2015-02-05 00:00"),
                  to = as.POSIXct("2015-02-17 23:30"),
                  by = "30 min")

power.log.ts <- xts(power.log$value, order.by = time_index)

# Set plotting parameters
par(mfrow=c(3,1)) 
forecast.periods = 100

# Perform Holt-Winters forecasting
power.forecast.hw <- HoltWinters(power.log.ts, beta=FALSE, gamma=FALSE)
plot.forecast(forecast.HoltWinters(power.forecast.hw,h=forecast.periods))

# Perform ARIMA (Autoregressive integrated moving average) forecasting
power.forecast.arima <- auto.arima(power.log.ts,approximation=FALSE,trace=FALSE)
plot.forecast(forecast.Arima(power.forecast.arima,h=forecast.periods))

# Perform ETS (Exponential smoothing state space) forecasting
power.forecast.ets <- ets(power.log.ts)
plot.forecast(forecast.ets(power.forecast.ets,h=forecast.periods))

# Graph demand
dygraph(power.log.ts) %>% dyRangeSelector()

