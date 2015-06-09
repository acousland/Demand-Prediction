############################################
# Demand Prediction Example
# - Decomposition
#
# Aaron Cousland
# 9/6/2015
############################################

Sys.setenv(TWO_TASK="//WPRORA001:1521/RTV")

require (RODBC)        # Load RODBC package
require (lubridate)    # Required to manipulate dates
require (TTR)          
require (reshape)      # Data preperation
require (dygraphs)     # For nice time series graphs
require (forecast)     # Forecasting
require (nnet)         # Nerual Network Package
require (caret)

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
energy.log <- sqlQuery(local.connection,"SELECT * from dw.METER_REG_ACTIVE_READ_48@CODSUP1.WORLD
                       where METER_REG_ACTIVE_READING_DT between to_date('01/01/2010','DD/MM/YYYY') and to_date('01/12/2015','DD/MM/YYYY')
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

# Change results into a time series

time_index <- seq(from = force_tz(min(power.log$TS),"AEST"),
                  to = force_tz(max(power.log$TS),"AEST"),
                  by = "30 min")

#power.log.ts <- ts(power.log$Demand, frequency=30)
power.log.ts <- msts(power.log$Demand,seasonal.periods=c(48,336))

# Plot decomposition
plot(stl(power.log.ts,s.window=48*365))
plot(decompose(power.log.ts))

