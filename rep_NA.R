rep_NA <- function() {
  i <- 0
  tempData <- actdata
  for (i in which(is.na(tempData$steps))) {
    tempData$steps[i] <- plotdata$meansteps[which(plotdata$interval==tempData$interval[i])]
  }
  tempData
}