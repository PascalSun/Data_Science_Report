###################################
# Prepare the working environment #
###################################

Sys.setlocale(category = "LC_ALL", locale = "English")
working_dir = 'D:/Google/MIT/DataScience/Project/Power'
setwd(working_dir)
#install.packages(c("VIM","mice"))
getwd()
rm(list=ls())

###################################
# Load the data from the txt file #
###################################

# load data from the txt file function
load_data <- function(filename){
  Data <- read.csv(file=filename,header=TRUE,sep=";")
  return(Data)
}

# load data in mydata data.frame
raw_data <- load_data("./code/household_power_consumption.txt")
# print('Original Summary Data')
# print(summary(mydata))
raw_data$version <- 0
mydata <- raw_data

####################################
# Assign Proper type to the values #
####################################

# Deal with data to make them the proper data format
format_data <-function(mydata){
  mydata$DateFull <- paste(mydata$Date,mydata$Time)
  mydata$DateFull <- as.POSIXct(mydata$DateFull,"%d/%m/%Y %H:%M:%S",tz=Sys.timezone())
  mydata$Date <- as.Date(mydata$Date,"%d/%m/%Y")
  mydata$DateTimeS <- as.POSIXct(mydata$Time,"%H:%M:%S",tz=Sys.timezone())
  mydata$Global_active_power <- as.numeric(as.character(mydata$Global_active_power))
  mydata$Global_reactive_power <- as.numeric(as.character(mydata$Global_reactive_power))
  mydata$Global_intensity <- as.numeric(as.character(mydata$Global_intensity))
  mydata$Voltage <- as.numeric(as.character(mydata$Voltage))
  mydata$Sub_metering_1 <- as.numeric(as.character(mydata$Sub_metering_1))
  mydata$Sub_metering_2 <- as.numeric(as.character(mydata$Sub_metering_2))
  mydata$Sub_metering_3 <- as.numeric(as.character(mydata$Sub_metering_3))
  return(mydata)
}

mydata = format_data(mydata)
mydata$version <- 1

#====================================================================================#

##########################
# Deal with missing Data #
##########################


#########################################
# I. Analysze Missing Data Distribution #
#########################################

###################
# a. Summary Data #
###################
print('Summary Data')
print(summary(mydata))


####################################
# b. Find Missing Rate and Counts #
####################################

# Find the missing rate of the data
pMiss <- function(x) {sum(is.na(x))/length(x)*100}
print('Missing data rate for each column')
print(apply(mydata,2,pMiss))

# Analysis with mice
library(mice)
print(md.pattern(mydata))

#################################
# c. Visualize the missing data #
#################################

library('VIM')

# show missing data matrix
png('./img/Data-Missing-Matrix.png')
matrixplot(mydata)  
dev.off()

# Data-Missing-Meter-3 check the missing values graph in sub-metering
png('./img/Data-Missing-Time-Meter-3.png')
par(mfrow=c(3,2))
marginplot(mydata[c("DateFull","Sub_metering_3")],col=c("darkgray","red","blue"))
#dev.off()

#png('./img/Data-Missing-Time-Voltage.png')
marginplot(mydata[c("DateFull","Voltage")],col=c("darkgray","red","blue"))
#dev.off()

#png('./img/Data-Missing-Time-Meter-2.png')
marginplot(mydata[c("DateFull","Sub_metering_2")],col=c("darkgray","red","blue"))
#dev.off()

#png('./img/Data-Missing-Time-Intensity.png')
marginplot(mydata[c("DateFull","Global_intensity")],col=c("darkgray","red","blue"))
#dev.off()

#png('./img/Data-Missing-Time-Global_Active.png')
marginplot(mydata[c("DateFull","Global_active_power")],col=c("darkgray","red","blue"))
#dev.off()

#png('./img/Data-Missing-Time-Global_ReActive.png')
marginplot(mydata[c("DateFull","Global_reactive_power")],col=c("darkgray","red","blue"))
dev.off()

#png('./img/Data-Missing-Time-Meter-3.png')
marginplot(mydata[c("DateFull","Sub_metering_3")],col=c("darkgray","red","blue"))
#dev.off()

dev.off()
################################################
# d. Analysis the distribution of Missing data #
################################################

# The missing data can be analyszed from 2 aspects
# 1. The time is random or meaning something
# 2. The data before and after it, cause it? (High Intensity or ?)


missing_data_time =  subset(mydata,is.na(mydata$DateFull))
missing_data_other = subset(mydata,is.na(mydata$Global_active_power))

#########
# Notes #
#########
# 1. The missing of the data is because some unknown of the equipment or network, and most happen in 2007 and 2008, mainly in 3 parts.
# 2. After analysze the data near the missing data, we found that the high voltage is not the reason why the data is missing.
# 3. we infer that it may caused because the outrage nearby
# 4. So we decide to drop the data null

############################
# e. Drop the missing rows #
############################


clean_data <- na.omit(mydata)
clean_data$version <- 2

###################################
# f. add other data to the column #
###################################

clean_data$Sub_metering_other <- (clean_data$Global_active_power*1000/60)-clean_data$Sub_metering_1-clean_data$Sub_metering_2-clean_data$Sub_metering_3
print('Summary for sub meter other')
print(summary(clean_data$Sub_metering_other))

# replace the sub meter other < 0 with 0
clean_data$Sub_metering_other[clean_data$Sub_metering_other < 0 ] <- 0

clean_data$version <- 3
#===========================================================================#

####################
# Explore the data #
####################

# a. summary the data
print('Summary for clean data')
print(summary(clean_data))

png('./img/Data-Range-Global_Activate_Power.png')
par(mfrow=c(4,2))
plot(clean_data$DateFull,clean_data$Global_active_power,main='Global Active Power')
# dev.off()

# png('./img/Data-Range-Global_ReActivate_Power.png')
plot(clean_data$DateFull,clean_data$Global_reactive_power,main='Global ReActive Power')
# dev.off()

#png('./img/Data-Range-Voltage.png')
plot(clean_data$DateFull,clean_data$Voltage,main='Voltage')
#dev.off()

#png('./img/Data-Range-Global_Intensity.png')
plot(clean_data$DateFull,clean_data$Global_intensity,main='Global Intensity')
#dev.off()

#png('./img/Data-Range-Sub_Metering_1.png')
plot(clean_data$DateFull,clean_data$Sub_metering_1,main='Sub Metering 1')
#dev.off()

#png('./img/Data-Range-Sub_Metering_2.png')
plot(clean_data$DateFull,clean_data$Sub_metering_2,main='Sub Metering 2')
#dev.off()

#png('./img/Data-Range-Sub_Metering_3.png')
plot(clean_data$DateFull,clean_data$Sub_metering_3,main='Sub Metering 3')
#dev.off()

#png('./img/Data-Range-Sub_Metering_other.png')
plot(clean_data$DateFull,clean_data$Sub_metering_other,main='Sub Metering Other')
dev.off()


# b. Data Preprocessing
# seperate the year,month,day,hour,minute into different column
clean_data$Year = format(clean_data$DateFull,format="%Y")
clean_data$Month = format(clean_data$DateFull,format="%m")
clean_data$Day = format(clean_data$DateFull,format="%d")
clean_data$Hour = format(clean_data$DateFull,format="%H")
clean_data$Minute = format(clean_data$DateFull,format="%M")
clean_data$DayOfYear = format(clean_data$DateFull,format="%j")

# Total Power
clean_data$Global_Power <- sqrt(clean_data$Global_active_power^2+clean_data$Global_reactive_power^2)
clean_data$Phase <- clean_data$Global_active_power/clean_data$Global_Power

png('./img/Data-Explore-Phase-VS-Voltage.png')
par(mfrow=c(3,1))
plot(clean_data$Phase,clean_data$Voltage)
#dev.off()

#png('./img/Data-Explore-Phase-VS-Intensity.png')
plot(clean_data$Phase,clean_data$Global_intensity)
#dev.off()

# png('./img/Data-Explore-Global_Power-vs-Date.png')
# plot(clean_data$DateFull,clean_data$Global_Power)
# dev.off()

#png('./img/Data-Explore-Voltage-vs-Intensity.png')
plot(clean_data$Voltage,clean_data$Global_intensity)
dev.off()


png('./img/Data-Explore-Pie-Chart-Meters.png')
slice <- c(sum(clean_data$Sub_metering_1),sum(clean_data$Sub_metering_2),sum(clean_data$Sub_metering_3),sum(clean_data$Sub_metering_other))
lbls <- c('Sub_Meter_1','Sub_Meter_2','Sub_Meter_3','Sub_Meter_Other')
piepercent<- round(100*slice/sum(slice), 1)
pie(slice,labels = piepercent,main="Pie Chart of the Power Comsuption",col=rainbow(length(slice)))
legend("topright", c("Meter1","Meter2","Meter3","Meter_other"), cex = 0.8,fill = rainbow(length(slice)))
dev.off()



