#############################################################
# Build Model 3: Association Rules between different meters #
#############################################################
# Packages: arules arulesViz


# 1. Sub Meter 1: Kitchen: Dishwasher, Oven, Microwave
# 2. Sub Meter 2: Laundry: Washing Machine, Tumble-Drier, Refrigerator, a light
# 3. Sub Meter 3: AC/Heater

##########################
# Example Plot for a day #
##########################

png('./img/Data-Build-Model-Rule-1-Day-Example-Sub-Meter.png')
par(mfrow=c(2,2))
day2008_01_01 <- subset(clean_data,clean_data$Date == as.Date('2008-03-15'))
plot(day2008_01_01$DateFull,day2008_01_01$Sub_metering_1,col="red",xlab="Time",ylab="Power",type="h",main="March")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_2,type="h",col="blue")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_3,type="l",col="black")
legend('topleft',legend = c('Meter 1-Kitchen','Meter 2-Laundry', 'Meter 3-Heater/AC'),col=c('red','blue','black'),cex=0.8,lty=1:2)

day2008_01_01 <- subset(clean_data,clean_data$Date == as.Date('2008-06-15'))
plot(day2008_01_01$DateFull,day2008_01_01$Sub_metering_1,col="red",xlab="Time",ylab="Power",type="h",main="June")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_2,type="h",col="blue")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_3,type="l",col="black")
legend('topleft',legend = c('Meter 1-Kitchen','Meter 2-Laundry', 'Meter 3-Heater/AC'),col=c('red','blue','black'),cex=0.8,lty=1:2)

day2008_01_01 <- subset(clean_data,clean_data$Date == as.Date('2008-09-15'))
plot(day2008_01_01$DateFull,day2008_01_01$Sub_metering_1,col="red",xlab="Time",ylab="Power",type="h",main="Septmber")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_2,type="h",col="blue")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_3,type="l",col="black")
legend('topleft',legend = c('Meter 1-Kitchen','Meter 2-Laundry', 'Meter 3-Heater/AC'),col=c('red','blue','black'),cex=0.8,lty=1:2)

day2008_01_01 <- subset(clean_data,clean_data$Date == as.Date('2008-12-15'))
plot(day2008_01_01$DateFull,day2008_01_01$Sub_metering_1,col="red",xlab="Time",ylab="Power",type="h",main="December")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_2,type="h",col="blue")
lines(day2008_01_01$DateFull,day2008_01_01$Sub_metering_3,type="l",col="black")
legend('topleft',legend = c('Meter 1-Kitchen','Meter 2-Laundry', 'Meter 3-Heater/AC'),col=c('red','blue','black'),cex=0.8,lty=1:2)

dev.off()

png('./img/Data-Build-Model-Rule-1-Day-Example-Power.png')
plot(day2008_01_01$DateFull,day2008_01_01$Global_active_power,xlab="Time",ylab="Active Power",type='h',main="Activate Power of A Day")
dev.off()

####################################
# Translate into Patterns for days #
####################################

# a. Store Time and Way into pattern
pattern <- data.frame('Time'=character(),'Mark'=character(),stringsAsFactors = FALSE)


Month_March <- subset(clean_data,(clean_data$Date > as.Date('2008-11-07')&(clean_data$Date< as.Date('2009-11-06'))))

# A: When use Dishwasher or Microwave in kitchen, sub_metering_1:31~50, classfiy A

A <- subset(Month_March,Month_March$Sub_metering_1>30&Month_March$Sub_metering_1<=50)

start_time = A[1,]$DateFull
end_time = A[nrow(A),]$DateFull
n <- 2

while(n<nrow(A)){
  n <- n+1
  if(difftime(A[n,]$DateFull,start_time,units="mins")<25){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'A')
    start_time <- A[n,]$DateFull
  }
  
}

# B: When use oven in kitchen, sub_metering_1: >50, classify B
B <- subset(Month_March,Month_March$Sub_metering_1>50)

start_time = B[1,]$DateFull
end_time = B[nrow(B),]$DateFull
n <- 2

while(n<nrow(B)){
  n <- n+1
  if(difftime(B[n,]$DateFull,start_time,units="mins")<45){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'B')
    start_time <- B[n,]$DateFull
  }
  
}

# C: When refrigerator or light is used in laundry, sub_metering_2 <= 10

C <- subset(Month_March,Month_March$Sub_metering_2<=10)

start_time = C[1,]$DateFull
print(start_time)
end_time = C[nrow(C),]$DateFull
n <- 2

while(n<nrow(C)){
  n <- n+1
  if(difftime(C[n,]$DateFull,start_time,units="mins")<480){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'C')
    start_time <- C[n,]$DateFull
  }

}

# D: When refrigerator or light and washing-machine or a tumble-drieris used in laundry, sub_metering_2: 10~50

D <- subset(Month_March,(Month_March$Sub_metering_2>10)&(Month_March$Sub_metering_2<=50))

start_time = D[1,]$DateFull
end_time = D[nrow(D),]$DateFull
n <- 2

while(n<nrow(D)){
  n <- n+1
  if(difftime(D[n,]$DateFull,start_time,units="mins")<35){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'D')
    start_time <- D[n,]$DateFull
  }
  
}


# E: When refrigerator AND washing-machine, a tumble-drier  is used in laundry, sub_metering_2: 50

E <- subset(Month_March,Month_March$Sub_metering_2>50)

start_time = E[1,]$DateFull
end_time = E[nrow(E),]$DateFull
n <- 2

while(n<nrow(E)){
  n <- n+1
  if(difftime(E[n,]$DateFull,start_time,units="mins")<35){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'E')
    start_time <- E[n,]$DateFull
  }
}


# F: heater working 11-15

F <- subset(Month_March,(Month_March$Sub_metering_3)>10&(Month_March$Sub_metering_3)<=15)

start_time = F[1,]$DateFull
end_time = F[nrow(F),]$DateFull
n <- 2

while(n<nrow(F)){
  n <- n+1
  if(difftime(F[n,]$DateFull,start_time,units="mins")<80){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'F')
    start_time <- F[n,]$DateFull
  }
}

# G: AC 16-25

G <- subset(Month_March,(Month_March$Sub_metering_3)>15&(Month_March$Sub_metering_3)<=25)

start_time = G[1,]$DateFull
end_time = G[nrow(G),]$DateFull
n <- 2

while(n<nrow(G)){
  n <- n+1
  if(difftime(G[n,]$DateFull,start_time,units="mins")<480){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'G')
    start_time <- G[n,]$DateFull
  }
}


# H: AC+Heater >25

H <- subset(Month_March,Month_March$Sub_metering_3>25)

start_time = H[1,]$DateFull
end_time = H[nrow(H),]$DateFull
n <- 2

while(n<nrow(H)){
  n <- n+1
  if(difftime(H[n,]$DateFull,start_time,units="mins")<80){
    n <- n+1
  }else{
    # add it to column here
    print(start_time)
    pattern[nrow(pattern)+1,] <- c(as.character(start_time),'H')
    start_time <- H[n,]$DateFull
  }
}

# b. sort pattern to become day pattern

# process to be sorted
pattern$Mark <- as.factor(pattern$Mark)
pattern$Time <- as.POSIXct(pattern$Time,format="%Y-%m-%d %H:%M:%S",tz="Australia/Perth")
pattern$Date <- as.Date(pattern$Time,"%Y-%m-%d %H:%M:%S")
#  sort by time
pattern <- pattern[order(pattern$Time),]
print('here')
# add to vector by date


n =1

this_time <- pattern[n,]$Date
this_vector <- c()
this_list <- list()
while(n < nrow(pattern)-1){
  print(this_time)
  print(n)
  if(!is.na(pattern[n,]$Date) & this_time == pattern[n,]$Date){
    # add the value into vector
    start_mark <-1
    
    while(paste0(pattern[n,]$Mark,as.character(start_mark)) %in% this_vector){
      start_mark <- start_mark+1
    }
      this_vector <- c(this_vector,as.character(paste0(pattern[n,]$Mark,as.character(start_mark))))
    
    n <- n+1
  }else if(is.na(pattern[n,]$Date)){
    n<- n+1
  }
  
  else{
    key <- as.character(this_time)
    this_list[[key]] <- this_vector
    this_vector <- c()
    this_time <-pattern[n,]$Date
  }
}




#####################################
# Apriori to find Association Rules #
#####################################
library(arules)
trData <- as(this_list, 'transactions') 
inspect(trData)

png('./img/Data-Build-Model-Rule-1-Association-Rules-Frequency.png')
itemFrequencyPlot(trData,support=0.1,cex.names=0.8)
dev.off()

rules <- apriori(trData, parameter = list(supp = 0.6, conf =0.6,minlen=3,maxlen=4)) 
inspect(rules)  

library(arulesViz)
png('./img/Data-Build-Model-Rule-1-Rules-Scatterplot.png')
plot(rules, method='scatterplot')
dev.off()

png('./img/Data-Build-Model-Rule-1-Rules-Graph.png')
plot(rules, method='graph', control = list(type='items'))
dev.off()
