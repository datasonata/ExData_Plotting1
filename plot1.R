p1 <- function() {
  # read in the csv filtering data only for dates 2007-02-01 and 2007-02-02
  hpc <- read.csv(file="data/household_power_consumption.txt", header=T, sep=";")
  gap <- subset(hpc, hpc$Date == '1/2/2007' | hpc$Date == '2/2/2007', select=c("Date", "Global_active_power"))
  
  # extract subset from the CSV data to exclude missing values
  gapv <- subset(gap, Global_active_power != '?')
  
  # convert values for Global_active_power to numeric
  g<-as.numeric(as.character(gapv[['Global_active_power']]))

  # create a histogram of the data
  hist(g, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red")
  
}

# create PNG file
png(file="plot1.png", width = 480, height = 480, units = "px")
# execute p1 function to create a plot
p1()
# close the device
dev.off()

