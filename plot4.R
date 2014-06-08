p4 <- function() {
  # read in the csv filtering data only for dates 2007-02-01 and 2007-02-02
  hpc <- read.csv(file="data/household_power_consumption.txt", header=T, sep=";")
  ss <- subset(hpc, hpc$Date == '1/2/2007' | hpc$Date == '2/2/2007')
  
  ss <- cbind(ss, paste(sep=" ", as.character(ss[,1]), as.character(ss[,2])))
  colnames(ss)[10]<-"DateTime"
  
  # extract from Global_active_power readings excluding '?' values
  gap <- subset(ss, ss$Global_active_power != '?', select=c("Global_active_power"))

  # extract from Sub_metering_X readings excluding '?' values
  sm_1 <- subset(ss, ss$Sub_metering_1 != '?', select=c("Sub_metering_1"))
  sm_2 <- subset(ss, ss$Sub_metering_2 != '?', select=c("Sub_metering_2"))
  sm_3 <- subset(ss, ss$Sub_metering_3 != '?', select=c("Sub_metering_3"))
  
  # extract from Voltage readings excluding '?' values
  volt <- subset(ss, ss$Voltage != '?', select=c("Voltage"))
  
  # extract from Global_reactive_power readings excluding '?' values
  grp <- subset(ss, ss$Global_reactive_power != '?', select=c("Global_reactive_power"))

  
  # define X-axis and Y-axis data
  xdata=ss$DateTime
  gap_data=as.numeric(as.character(gap[[1]]))
  sm1_data=as.numeric(as.character(sm_1[[1]]))
  sm2_data=as.numeric(as.character(sm_2[[1]]))
  sm3_data=as.numeric(as.character(sm_3[[1]]))
  volt_data=as.numeric(as.character(volt[[1]]))
  grp_data=as.numeric(as.character(grp[[1]]))

  
  # create a vector of weekdays to be used for X-axis tick labels
  wdays<-weekdays(as.Date(c('1/2/2007','2/2/2007','3/2/2007'),'%d/%m/%Y'))

  # create a Global_active_power plot without axes
  plot(xdata, gap_data, axes = FALSE, ann = FALSE, xlab="", ylab="Global Active Power", col="black")
  lines(xdata,gap_data,col="black")
  
  # add axes to the Global_active_power plot 
  axis(1, at=c(1,length(xdata)/2,length(xdata)), labels=substring(wdays,0,3))
  axis(2)
  
  # add box around the Global_active_power plot
  box(which = "plot", lty = "solid")  

  # create a Sub metering plot without axes
  plot(xdata, sm1_data, axes = FALSE, ann = FALSE, ylab="Energy sub metering", col="black")
  lines(xdata,sm1_data,col="black")
  lines(xdata,sm2_data,col="red")
  lines(xdata,sm3_data,col="blue")
  
  # add axes to the Sub metering plot 
  axis(1, at=c(1,length(xdata)/2,length(xdata)), labels=substring(wdays,0,3))
  axis(2) 
  
  # add legend to the Sub metering plot
  legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1), lwd=c(1,1), col=c("black","red","blue"), bty="n")
  
  # add box around the Sub metering plot
  box(which = "plot", lty = "solid")
  
  # create a voltage plot without axes
  plot(xdata, volt_data, axes = FALSE, ann = FALSE, xlab="datetime", ylab="Voltage", col="black")
  lines(xdata,volt_data,col="black")

  # add axes to the voltage plot 
  axis(1, at=c(1,length(xdata)/2,length(xdata)), labels=substring(wdays,0,3))
  axis(2)
  
  # add box around the voltage plot
  box(which = "plot", lty = "solid")  
  
  # create a Global_reactive_power plot without axes
  plot(xdata, grp_data, axes = FALSE, ann = FALSE, xlab="datetime", ylab="Global_reactive_power", col="black")
  lines(xdata,grp_data,col="black")
  
  # add axes to the Global_reactive_power plot 
  axis(1, at=c(1,length(xdata)/2,length(xdata)), labels=substring(wdays,0,3))
  axis(2)
  
  # add box around the Global_reactive_power plot
  box(which = "plot", lty = "solid")  
 
}

# create PNG file
png(file="plot4.png", width = 480, height = 480, units = "px")
# set parameter to output plots into the same graphics device in two columns and two rows
par(mfcol = c(2, 2))
# execute p4 function to generate the plot
p4()
# turn off the graphic device (png file)
dev.off()







