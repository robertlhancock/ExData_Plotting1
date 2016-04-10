fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"


wd <- "~/Development/coursera/Exploratory Data Analysis"
wd <- "/Users/robertl/Development/coursera/ExploratoryDataAnalysis/Week1Project"
setwd( wd)

zipfilename = "./data/powerconsumption.zip"
txtfilename = "./data/household_power_consumption.txt"

initialize<- function(){
  install.packages("dplyr")
  library(dplyr)
  packageVersion("dplyr")
  install.packages("tidyr")
  library(tidyr)
  install.packages("matrixStats")
  library(matrixStats)
}

##########################################################################################
## getData function
## 
## get Data from remote repository and unzip it
## 
getData <- function(fname) {
  if (!file.exists("./data")) {
    dir.create("./data")
  }
  if (!file.exists(fname)) {
    download.file(fileUrl, destfile = fname, method = "curl")
    unzip(
      fname,
      files = NULL,
      list = FALSE,
      overwrite = TRUE,
      junkpaths = FALSE,
      exdir = "./data",
      unzip = "internal",
      setTimes = FALSE
    )
  }
  
}
##########################################################################################
## loadFileName function
## 
## load a space delimeted data file into memory and return the contents to the caller
## 
loadFileName <- function ( filename="README.txt" ){
  # Get  the files names
  d<-read.table(filename, sep=";", dec=".", header=TRUE,stringsAsFactors=FALSE) 
  saveit<<-d
  d<-filter( d, Date == "1/2/2007" | Date == "2/2/2007")
  d <- transform( d,
                  Time = strptime( paste0(Date," ", Time), "%d/%m/%Y %H:%M:%S"),
                  Date = as.Date( Date, "%d/%m/%Y"),
                  #Date = Date,
                  #Time = Time,
                  Global_active_power = as.numeric( Global_active_power),
                  Global_reactive_power = as.numeric( Global_reactive_power),
                  Voltage = as.numeric( Voltage),
                  Global_intensity = as.numeric( Global_intensity),
                  Sub_metering_1 = as.numeric( Sub_metering_1),
                  Sub_metering_2 = as.numeric( Sub_metering_2),
                  Sub_metering_3 = as.numeric( Sub_metering_3))
  
  
}

plot1 <- function( data){
  hist(data$Global_active_power, col="red", xlab = "Global Active Power (kilowatts)", main="Global Active Power")
}

png1 <- function(data) {
  png(paste0(wd, "/", "plot1.png"),
      width = 480,
      height = 480)
 
  plot1(data)

  
  dev.off()
}


#initialize()

getData(zipfilename)

power<-loadFileName(txtfilename)
png1( power)