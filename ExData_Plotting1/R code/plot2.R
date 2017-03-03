# data for plot acquired from UC Irvine Machine Learning Repository  "Thu Mar 02 21:39:13 2017"
# "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"


# get the raw data from workspace
data<-read.csv2("..\\household_power_consumption.txt",na.strings = "?")

# get data from the two dates 2007-02-01 and 2007-02-02
data<-data[which(data$Date=="1/2/2007"|data$Date=="2/2/2007"|
        # also get extra samples so axis matches rdpeng's
        (data$Date=="31/1/2007"&data$Time=="23:58:00")|
        (data$Date=="31/1/2007"&data$Time=="23:59:00")|
        (data$Date=="3/2/2007"&data$Time=="00:00:00")|
        (data$Date=="3/2/2007"&data$Time=="00:01:00")
),]


# cast the numerical data here since R seems incapable of reading numerics directly
{
        ## household global minute-averaged active power (in kilowatt)
        data$Global_active_power<-as.numeric(data$Global_active_power)
        
        ## household global minute-averaged reactive power (in kilowatt)
        data$Global_reactive_power<-as.numeric(data$Global_reactive_power)
        
        ## minute-averaged voltage (in volt)
        data$Voltage<-as.numeric(data$Voltage)
        
        ## household global minute-averaged current intensity (in ampere)
        data$Global_intensity<-as.numeric(data$Global_intensity)
        
        ## energy sub-metering No. 1 (in watt-hour of active energy)
        data$Sub_metering_1<-as.numeric(data$Sub_metering_1)
        
        ## Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy)
        data$Sub_metering_2<-as.numeric(data$Sub_metering_2)
        
        ## energy sub-metering No. 3 (in watt-hour of active energy)
        data$Sub_metering_3<-as.numeric(data$Sub_metering_3)
        
        ## add an independant time axis
        data$t<-as.integer(
                as.POSIXct(
                        paste(data$Date,data$Time,sep=" "),
                        format="%d/%m/%Y %H:%M:%S"
                )
        )
}



# generate a mask which tells us where to place x axis tick labels
getWeekDay<-function(dateStr,...)
{
        substr(weekdays(as.Date(dateStr,...)),1,3)
}
xLabNames<-sapply(data$Date,getWeekDay,format="%d/%m/%Y")

# generate a mask which tells us where to place x axis tick labels
xLabSelectM<-c(xLabNames[-1L]!=xLabNames[-length(xLabNames)],F)


png("..\\figure\\plot2.png",width = 480, height = 480, units = "px")
plot(
        x = data$t,
        xlab = "",
        xaxt = "n",
        y = data$Global_active_power,
        ylab = "Global Active Power (kilowatts)",
        type="l"
)
axis(
        side=1,
        at=data$t[which(xLabSelectM)],
        labels=xLabNames[xLabSelectM]
)
dev.off()

warning("Note to self: numbers are out by factor of 5e2")
warning("Note to self: names of week days are wrong")