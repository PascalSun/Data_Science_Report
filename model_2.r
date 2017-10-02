#########################################################
# Build Model 2: Regression between Rate and sub meters #
#########################################################


###############
# Observation #
###############

build_model_2 <- data.frame(clean_data$Phase,clean_data$Sub_metering_1,clean_data$Sub_metering_2,clean_data$Sub_metering_3)

png('./img/Data-Build-Model-regression-2-Sub-Meter-1-VS-Rate.png')
par(mfrow=c(3,1))
plot(build_model_2$clean_data.Sub_metering_1,build_model_2$clean_data.Phase,xlab="Sub Meter 1",ylab="Sub Meter 2")
# dev.off()

# png('./img/Data-Build-Model-regression-2-Sub-Meter-2-VS-Rate.png')
plot(build_model_2$clean_data.Sub_metering_2,build_model_2$clean_data.Phase,xlab="Sub Meter 1",ylab="Sub Meter 2")
# dev.off()

# png('./img/Data-Build-Model-regression-2-Sub-Meter-3-VS-Rate.png')
plot(build_model_2$clean_data.Sub_metering_3,build_model_2$clean_data.Phase,xlab="Sub Meter 1",ylab="Sub Meter 2")
dev.off()


# we found that when the value is larger than 5, it seems like a linear, so we drop the small values

##############
#  Sampling  #
##############

build_model_2_bt = subset(build_model_2,(build_model_2$clean_data.Sub_metering_1>5)&(build_model_2$clean_data.Sub_metering_2>5)&(build_model_2$clean_data.Sub_metering_3>5))


build_model_2_bt_sample <- build_model_2_bt[sample(nrow(build_model_2_bt),2000),]

build_model_2_test <- build_model_2_bt[sample(nrow(build_model_2_bt),2000),]


###############
# Build Model #
###############


fit <- lm(build_model_2_bt_sample$clean_data.Phase~build_model_2_bt_sample$clean_data.Sub_metering_1+build_model_2_bt_sample$clean_data.Sub_metering_2+build_model_2_bt_sample$clean_data.Sub_metering_3)
print(summary(fit))
png('./img/Data-Build-Model-regression-2-Report.png')
par(mfrow=c(2,2))
plot(fit)
dev.off()

png('./img/Data-Build-Model-regression-2-Fitted.png')
plot(build_model_2_bt_sample$clean_data.Phase,fitted(fit))
dev.off()

pred <- predict(fit,build_model_2_test)
png('./img/Data-Build-Model-regression-2-Test.png')
plot(pred,build_model_2_test$clean_data.Phase-pred)
dev.off()
