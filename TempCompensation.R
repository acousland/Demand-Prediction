############################################
# Demand Prediction Example
# - Temperature Compensation
#
# Aaron Cousland
# 9/6/2015
############################################

rm(list=ls())

Sys.setenv(TWO_TASK="//WPRORA001:1521/RTV")

require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates
require (dygraphs)     # For nice time series graphs
require (forecast)     # Forecasting
require (nnet)         # Nerual Network Package

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
energy.log <- sqlQuery(local.connection,"SELECT * from dw.METER_REG_ACTIVE_READ_48@CODSUP1.WORLD
                       where METER_REG_ACTIVE_READING_DT between to_date('01/01/2010','DD/MM/YYYY') and to_date('01/12/2015','DD/MM/YYYY')
                       AND NMI_ID = '6407580165';")

temperature.log <- sqlQuery(local.connection,"SELECT * FROM BOM_TEMPERATURES_DAILY;")
odbcCloseAll()

# Transform results
energy.log <- subset(energy.log,select = c(NMI_ID,METER_REG_ACTIVE_READING_DT,MTR_RG_ACT_NET_ENGY_MAXDLY_KWH))
temperature.log$Date <- force_tz(dmy(paste(temperature.log$DAY,"/",temperature.log$MONTH,"/",temperature.log$YEAR,sep="")),"AEST")
temperature.log <- subset(temperature.log,select = c(Date,MAX_TEMP))
temperature.log <- subset(temperature.log,Date > min(power.log$Date))

# Change energy to demand
power.log <- energy.log
rm(energy.log)
power.log$MTR_RG_ACT_NET_ENGY_MAXDLY_KWH <- power.log$MTR_RG_ACT_NET_ENGY_MAXDLY_KWH*2
colnames(power.log) <- c("NMI","Date","Demand")

min(power.log$Date)
par(mfrow=c(2,1)) 

plot(power.log$Date,power.log$Demand, type = "l")
plot(temperature.log$Date,temperature.log$MAX_TEMP, type = "l")



# Change results into a time series
time_index <- seq(from = force_tz(min(power.log$TS),"AEST"),
                  to = force_tz(max(power.log$TS),"AEST"),
                  by = "30 min")

#power.log.ts <- ts(power.log$Demand, frequency=30)
power.log.ts <- msts(power.log$Demand,seasonal.periods=c(48,336))


