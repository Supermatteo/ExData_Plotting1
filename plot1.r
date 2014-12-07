plot1 <- function() {
        
        # load libraries 
        library(data.table)
        library(lubridate)
        
        # read data and place in a variable
        pwrdata <- read.table("household_power_consumption.txt", colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), sep = ";", header = TRUE, na.strings = "?")
        pwrdata <- na.omit(pwrdata) 
        
        
        # merge data and timeinto one column, cleanup data and rename columns
        pwrdata$dt <- paste(pwrdata$Date, pwrdata$Time)
        pwrdata[,1] <- pwrdata$dt
        pwrdata[,2:8] <- pwrdata[,3:9]
        pwrdata[,10] <- NULL
        pwrdata[,9] <- NULL

        names(pwrdata) <- c("dt", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        
       
        
        # date and time column into time and date class
        pwrdata$dt <- strptime(pwrdata$dt, format = "%d/%m/%Y %H:%M:%S")
        print(head(pwrdata))
        pwrdata <- pwrdata[pwrdata$dt >="2007-02-01 00:01:00",]
        #print(head(pwrdata))
        pwrdata <- pwrdata[pwrdata$dt <="2007-02-02 23:59:00",]
        #print(head(pwrdata))
        print(tail(pwrdata,300))
        # print to png
        png(file = "plot1.png")
        plot1 <- hist(pwrdata[,2],main = "Global Active Power", xlab = "Global Active Power (KiloWatts)", col = "red")
        dev.off()       
}