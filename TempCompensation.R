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
require (ggplot2)      # For other fancy graphs
require (forecast)     # Forecasting
require (neuralnet)    # Nerual Network Package

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)


# Query the database and put the results into the local memory
energy.log <- sqlQuery(local.connection,"SELECT * from dw.METER_REG_ACTIVE_READ_48@CODSUP1.WORLD
                       where METER_REG_ACTIVE_READING_DT between to_date('01/01/2010','DD/MM/YYYY') and to_date('01/12/2015','DD/MM/YYYY')
                       AND NMI_ID = '6407580165';")

#6407580165
#6407757806
#6407757805

temperature.log <- sqlQuery(local.connection,"SELECT * FROM BOM_TEMPERATURES_DAILY;")
odbcCloseAll()
rm(local.connection)


# Transform results
energy.log <- subset(energy.log,select = c(NMI_ID,METER_REG_ACTIVE_READING_DT,MTR_RG_ACT_NET_ENGY_MAXDLY_KWH))
colnames(energy.log) <- c("NMI","Date","Demand")
energy.log$Date <- force_tz(energy.log$Date,"AEST")
temperature.log$Date <- force_tz(dmy(paste(temperature.log$DAY,"/",temperature.log$MONTH,"/",temperature.log$YEAR,sep="")),"AEST")
temperature.log <- subset(temperature.log,select = c(Date,MAX_TEMP))
energy.log <- merge(energy.log, temperature.log, by = 'Date')
rm(temperature.log)


# Change energy to demand
power.log <- energy.log
rm(energy.log)
power.log$Demand <- power.log$Demand*2
colnames(power.log) <- c("Date","NMI","Demand","Max_Temp")


# Calculate the day of the week
power.log$Day <- weekdays(power.log$Date)


# Set up graphing parameters and plot graphs
#par(mfrow=c(2,1)) 
par(mfrow=c(1,1))

#plot(power.log$Date,power.log$Demand, type = "l")
#plot(power.log$Date,power.log$Max_Temp, type = "l")


# Plotting demand by weekday
#ggplot(power.log)+
#  geom_point(aes(x=Day, y=Demand),size=3)


# Train the neural network
demand.network <- neuralnet(Demand~Max_Temp, data = power.log, hidden=20, threshold=0.1)

#plot(demand.network)

temp.compensation <- prediction(demand.network)
plot(temp.compensation$rep1)

#plot(temp.compensation$data, type = "l")

temp.range <- 10:50
predicted.demand <- compute(demand.network,temp.range)
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,max(predicted.demand$net.result)))
