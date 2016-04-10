fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"


wd <- "~/Development/coursera/Exploratory Data Analysis"
wd <- "/Users/robertl/Development/coursera/ExploratoryDataAnalysis/Week1Project"
setwd( wd)

zipfilename = "./data/powerconsumption.zip"
txtfilename = "./data/household_power_consumption.txt"

##########################################################################################
## install_load function
## 
## This function will install and load the list of libraries specified in the call to the function.
## 1. vectorize the list of packages
## 2. for each package:
##    a. if a package is installed load the library
##    b. if not installed then install and load the library
## 
install_load <- function (package1, ...)  {   
  
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  } 
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
  draw<<-d
  d<-filter( d, Date == "1/2/2007" | Date == "2/2/2007")
  dfiltered<<-d
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
  dconverted<<-d
  return(d)
  
}

##########################################################################################
## plot3 function
## 
## draw the graph
## 
plot3 <- function( data){
  
  plot(
    data$Time,
    data$Sub_metering_1,
    type = "l",
    col = "black",
    ylab = "Energy sub metering",
    main = "",
    xlab = ""
  )
  lines(data$Time,
        data$Sub_metering_2,
        type = "l",
        col = "red")
  lines(data$Time,
        data$Sub_metering_3,
        type = "l",
        col = "blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"),lty=1, lwd=2)  
}

##########################################################################################
## png3 function
## 
## Open the file and call the plot function
## 
png3 <- function(data) {
  png(paste0(wd, "/", "plot3.png"),
      width = 480,
      height = 480)

  plot3(data)
 
  
  dev.off()
}

#install libraries
install_load("dplyr", "tidyr")

#get data from remote repository
getData(zipfilename)

#load the data
power<-loadFileName(txtfilename)

# write the graph file
png3( power)