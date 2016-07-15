Trep_NA <- function() {
  i <- 0
  tempData <- actdata
  for (i in which(is.na(tempData$steps))) {
    tempData$steps[i] <- Tplotdata$mediansteps[which(Tplotdata$interval==tempData$interval[i])]
  }
  tempData
}