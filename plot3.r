plot3 <- function() {
        
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
        pwrdata <- pwrdata[pwrdata$dt >="2007-02-01 00:01:00",]
        pwrdata <- pwrdata[pwrdata$dt <="2007-02-02 23:59:00",]
        
        # print to png
        png(file = "plot3.png")
        plot3 <- plot(pwrdata$dt, pwrdata$Sub_metering_1, xlab="", ylab="Global Active Power (kilowatts)", type="l", col = "black",width = 480, height = 480, units = "px")
        lines(pwrdata$dt, pwrdata$Sub_metering_2, xlab="", ylab="Global Active Power (kilowatts)", type="l", col = "red")
        lines(pwrdata$dt, pwrdata$Sub_metering_3, xlab="", ylab="Global Active Power (kilowatts)", type="l", col = "blue")
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), col=c("black", "red", "blue"))
        dev.off()       
}