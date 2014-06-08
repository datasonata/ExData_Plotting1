p3 <- function() {
  # read in the csv filtering data only for dates 2007-02-01 and 2007-02-02
  hpc <- read.csv(file="data/household_power_consumption.txt", header=T, sep=";")
  sm <- subset(hpc, hpc$Date == '1/2/2007' | hpc$Date == '2/2/2007', select=c("Date", "Time", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  # extract subset from the CSV data to exclude missing values
  #gapv <- subset(gap, Sub_metering_1 != '?')
  
  sm <- cbind(sm, paste(sep=" ", as.character(sm[,1]), as.character(sm[,2])))
  colnames(sm)[6]<-"DateTime"
  
  # extract from each  Sub_metering readings excluding '?' values
  sm1 <- subset(sm, sm$Sub_metering_1 != '?', select=c("Sub_metering_1"))
  sm2 <- subset(sm, sm$Sub_metering_2 != '?', select=c("Sub_metering_2"))
  sm3 <- subset(sm, sm$Sub_metering_3 != '?', select=c("Sub_metering_3"))
  
  # define X-axis and Y-axis data
  xdata=sm[,6]
  ydata_1=as.numeric(as.character(sm1[[1]]))
  ydata_2=as.numeric(as.character(sm2[[1]]))
  ydata_3=as.numeric(as.character(sm3[[1]]))

  # create a vector of weekdays to be used for X-axis tick labels
  wdays<-weekdays(as.Date(c('1/2/2007','2/2/2007','3/2/2007'),'%d/%m/%Y'))
  
  # create a scatter plot without axes
  plot(xdata, ydata_1, axes = FALSE, ann = FALSE, ylab="Energy sub metering", col="black")
  lines(xdata,ydata_1,col="black")
  lines(xdata,ydata_2,col="red")
  lines(xdata,ydata_3,col="blue")
  
  # add axes to the plot 
  axis(1, at=c(1,length(xdata)/2,length(xdata)), labels=substring(wdays,0,3))
  axis(2) 
  
  # add legend
  legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1), lwd=c(1,1), col=c("black","red","blue"))
  
  # add box around the plot
  box(which = "plot", lty = "solid")
  
}
# create PNG file
png(file="plot3.png", width = 480, height = 480, units = "px")

p3()

# turn off the graphic device (png file)
dev.off()





