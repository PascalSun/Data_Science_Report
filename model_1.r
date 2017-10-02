
#############################################
# Build Model 1: Power Consumption VS Month #
#############################################

############
# For Days #
############

# 2007
year2007 = subset(clean_data,clean_data$Year=='2007')
year2007_Power <- aggregate(year2007$Global_Power,by=list(DayofYear=year2007$DayOfYear),FUN=sum)

png('./img/Data-Build-Model-regression-1-check-2007.png')
par(mfrow=c(3,2))
plot(year2007_Power$DayofYear,year2007_Power$x,xlab="DayofYear",ylab="Power",main='Consumption of Power the whole year 2007')
#dev.off()

# 2008
year2008 = subset(clean_data,clean_data$Year=='2008')
year2008_Power <- aggregate(year2008$Global_Power,by=list(DayofYear=year2008$DayOfYear),FUN=sum)

#png('./img/Data-Build-Model-regression-1-check-2008.png')
plot(year2008_Power$DayofYear,year2008_Power$x,xlab="DayofYear",ylab="Power",main='Consumption of Power the whole year 2008')
#dev.off()

# 2009
year2009 = subset(clean_data,clean_data$Year=='2009')
year2009_Power <- aggregate(year2009$Global_Power,by=list(DayofYear=year2009$DayOfYear),FUN=sum)

#png('./img/Data-Build-Model-regression-1-check-2009.png')
plot(year2009_Power$DayofYear,year2009_Power$x,xlab="DayofYear",ylab="Power",main='Consumption of Power the whole year 2009')
#dev.off()


#############
# For month #
#############

# 2007
year2007 = subset(clean_data,clean_data$Year=='2007')
year2007_Power_Month <- aggregate(year2007$Global_Power,by=list(Month=year2007$Month),FUN=sum)

#png('./img/Data-Build-Model-regression-1-check-2007-month.png')
#par(mfrow=c(3,1))
plot(year2007_Power_Month$Month,year2007_Power_Month$x,xlab="Month",ylab="Power",main='Consumption of Power the whole year 2007')
#dev.off()

# 2008
year2008 = subset(clean_data,clean_data$Year=='2008')
year2008_Power_Month <- aggregate(year2008$Global_Power,by=list(Month=year2008$Month),FUN=sum)

#png('./img/Data-Build-Model-regression-1-check-2008-month.png')
plot(year2008_Power_Month$Month,year2008_Power_Month$x,xlab="Month",ylab="Power",main='Consumption of Power the whole year 2008')
#dev.off()

# 2009
year2009 = subset(clean_data,clean_data$Year=='2009')
year2009_Power_Month <- aggregate(year2009$Global_Power,by=list(Month=year2009$Month),FUN=sum)

#png('./img/Data-Build-Model-regression-1-check-2009-month.png')
plot(year2009_Power_Month$Month,year2009_Power_Month$x,xlab="Month",ylab="Power",main='Consumption of Power the whole year 2009')
dev.off()


############
# Box Plot #
############

png('./img/Data-Build-Model-regression-1-boxplot-2007-day.png')
par(mfrow=c(3,2))
boxplot(year2007_Power$x,main="Year2007Power")
# dev.off()

# png('./img/Data-Build-Model-regression-1-boxplot-2007-Month.png')
boxplot(year2007_Power_Month$x,main="Year2007PowerMonth")
# dev.off()


# png('./img/Data-Build-Model-regression-1-boxplot-2008-day.png')
boxplot(year2008_Power$x,main="Year2008Power")
# dev.off()

# png('./img/Data-Build-Model-regression-1-boxplot-2008-Month.png')
boxplot(year2008_Power_Month$x,main="Year2008PowerMonth")
# dev.off()

# png('./img/Data-Build-Model-regression-1-boxplot-2009-day.png')
boxplot(year2009_Power$x,main="Year2009Power")
# dev.off()

# png('./img/Data-Build-Model-regression-1-boxplot-2009-Month.png')
boxplot(year2009_Power_Month$x,main="Year2009PowerMonth")
dev.off()

###############
# Model Build #
###############


year2007_Power_Month$Month <- as.numeric(as.character(year2007_Power_Month$Month))
m <- nls(x~a*Month^2+b*Month+c,data=year2007_Power_Month,start=list(a=300,b=11,c=7))


png('./img/Data-Build-Model-regression-1-Model-and-Validate.png')
par(mfrow=c(3,1))
plot(year2007_Power_Month$Month,year2007_Power_Month$x,xlab="Month(2007)",ylab="Power")
lines(year2007_Power_Month$Month,predict(m),col="red",lty=2,lwd=3)

legend('top',legend=capture.output(summary(m)),cex=0.6)
#dev.off()

#png('./img/Data-Build-Model-regression-1-Model-Validate-2008.png')
plot(year2008_Power_Month$Month,year2008_Power_Month$x,xlab="Month(2008)",ylab="Power")
lines(year2008_Power_Month$Month,predict(m),col="red",lty=2,lwd=3)
#dev.off()

#png('./img/Data-Build-Model-regression-1-Model-Validate-2009.png')
plot(year2009_Power_Month$Month,year2009_Power_Month$x,xlab="Month(2009)",ylab="Power")
lines(year2009_Power_Month$Month,predict(m),col="red",lty=2,lwd=3)
dev.off()