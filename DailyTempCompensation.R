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
power.log$DayNumber <- ifelse(power.log$Day == 'Monday', 1,
                              ifelse(power.log$Day == 'Tuesday',2,
                                     ifelse(power.log$Day == 'Wednesday',3,
                                            ifelse(power.log$Day == 'Thursday',4,
                                                   ifelse(power.log$Day == 'Friday',5,
                                                          ifelse(power.log$Day == 'Saturday',6,7))))))


# Set up graphing parameters and plot graphs
#par(mfrow=c(2,1)) 
par(mfrow=c(1,1))

#plot(power.log$Date,power.log$Demand, type = "l")
#plot(power.log$Date,power.log$Max_Temp, type = "l")


# Plotting demand by weekday
#ggplot(power.log)+
#  geom_point(aes(x=Day, y=Demand),size=3)


# Train the neural network
demand.network <- neuralnet(Demand~Max_Temp+DayNumber, data = power.log, hidden=20, threshold=1,rep=2)

#plot.nn(demand.network)

temp.compensation <- prediction(demand.network)

temp.range <- 10:50
day.range <- 1:7



ymax=4
par(mfrow=c(2,4))

predicted.demand <- compute(demand.network,data.frame(temp.range,1))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Monday")

predicted.demand <- compute(demand.network,data.frame(temp.range,2))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Tuesday")

predicted.demand <- compute(demand.network,data.frame(temp.range,3))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Wednesday")

predicted.demand <- compute(demand.network,data.frame(temp.range,4))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Thursday")

predicted.demand <- compute(demand.network,data.frame(temp.range,5))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Friday")

predicted.demand <- compute(demand.network,data.frame(temp.range,6))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Saturday")

predicted.demand <- compute(demand.network,data.frame(temp.range,7))
plot(temp.range,predicted.demand$net.result,type = "l", ylim = c(0,ymax), main = "Sunday")


