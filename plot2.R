p2 <- function() {
  # read in the csv filtering data only for dates 2007-02-01 and 2007-02-02
  hpc <- read.csv(file="data/household_power_consumption.txt", header=T, sep=";")
  gap <- subset(hpc, hpc$Date == '1/2/2007' | hpc$Date == '2/2/2007', select=c("Date", "Time", "Global_active_power"))
  
  # extract subset from the CSV data to exclude missing values
  gapv <- subset(gap, Global_active_power != '?')
  
  gapv <- cbind(gapv, paste(sep=" ", as.character(gapv[,1]), as.character(gapv[,2])))
  colnames(gapv)[4]<-"DateTime"
  # convert values for Global_active_power to numeric
  gapv[,3]<-as.numeric(as.character(gapv[,3]))
  
  # define X-axis and Y-axis data
  xdata=gapv[,4]
  ydata=gapv[,3]
  
  
  # create a vector of weekdays to be used for X-axis tick labels
  wdays<-weekdays(as.Date(c('1/2/2007','2/2/2007','3/2/2007'),'%d/%m/%Y'))
  
  # create a scatter plot without axes
  plot(x=xdata, y=ydata, axes = FALSE, ann = FALSE, ylab="Global Active Power (kilowatts)")
  # fill in the lines 
  lines(x=xdata, y=ydata)
  
  # add axes to the plot 
  axis(1, at=c(1,length(xdata)/2,length(xdata)), labels=substring(wdays,0,3))
  axis(2) 
  
  # add box around the plot
  box(which = "plot", lty = "solid")
  
  
}
# create PNG file
png(file="plot2.png", width = 480, height = 480, units = "px")
# execute p2 function to create the plot
p2()
# turn off the graphic device (png file)
dev.off()





