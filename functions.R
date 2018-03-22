
library(jsonlite)
library(httr)

#-----------------------------------------------------------------------------------------------------------------------------------
# GETTING DATA FROM AQUARIUS
# 
# Contents: Connect to AQ - get a token
#           Define a multiPointCorrection class, keeping set, times, values and offsets
#           genericAQ - a function generically making a url and sending it to the API
#           getTimeSeriesIDs - get a list of published points time series with the given location and parameter
#           getRawData - get raw points from a time series with the given id, and between the start and end dates
#           getCorrections - get a list of multiPointCorrections for the given series and dates
#
#-----------------------------------------------------------------------------------------------------------------------------------

#Define a multiPointCorrection class
setClass(
  "multiPointCorrection",
  slots = c(
    startTime = "POSIXct",
    endTime = "POSIXct",
    set = "numeric",
    startValues = "numeric",
    startOffsets = "numeric",
    endValues = "numeric",
    endOffsets = "numeric"
  )
)

#Set limits on valid multiPointCorrection objects
setValidity("multiPointCorrection",
  function(object) {
      object@startTime <= object@endTime &&
      length(object@set) == 1 &&
      object@set %in% 1:3 &&
      length(object@startValues) <= 3 &&
      length(object@startValues) == length(object@startOffsets) &&
      length(object@endValues) <= 3 &&
      length(object@endValues) == length(object@endOffsets) 
  }
)


#Get generic data from the AQ API, giving the request name, the names of parameters, and their values
genericAQ <- function(serviceRequest, parameters, values, host="ts.nwis.usgs.gov") {
  url <- paste0("http://", host, "/AQUARIUS/Publish/V2/" , serviceRequest, "?")
  q <- paste(parameters, values, sep="=")
  q <- paste(q, collapse="&")
  url <- paste0(url, q)
  url <- gsub(" ", "+", url)
  resp <- GET(url)
  cont <- content(resp, "text")
  output <- fromJSON(cont)
  return(output)
}

#Get time series descriptions (including unqiue ID) by location and parameter
getTimeSeriesIDs <- function(location, parameter) {
  serviceRequest <- "GetTimeSeriesDescriptionList"
  parameters <- c("LocationIdentifier", "Parameter", "Publish", "ComputationIdentifier", "ComputationPeriodIdentifier")
  values <- c(location, parameter, "true", "Instantaneous", "Points")
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$TimeSeriesDescriptions
  return(out)
}

#Get the approval transaction list for given time series
getApprovalList <- function(timeSeriesID, start, end) {
  serviceRequest <- "GetApprovalsTransactionList"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo")
  values <- c(timeSeriesID, start, end)
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$ApprovalsTransactions
  out$DateAppliedUtc <- as.POSIXct(out$DateAppliedUtc, format="%Y-%m-%dT%H:%M")
  out$StartTime <- as.POSIXct(out$StartTime, format="%Y-%m-%dT%H:%M")
  out$EndTime <- as.POSIXct(out$EndTime, format="%Y-%m-%dT%H:%M")
  return(out)
}

#Determine whether two time periods overlap
isOverlap <- function(start1, end1, start2, end2) {
  if(end1 <= start2 | start1 >= end2) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#Find the overlap given two time periods
whatOverlap <- function(start1, end1, start2, end2) {
  #Find the start of the overlap
  if(start1 >= start2 & start1 < end2) {
    overlapStart <- start1
  } else {
    overlapStart <- start2
  }
  
  #Find the end of the overlap
  if(end1 > start2 & end1 <= end2) {
    overlapEnd <- end1 
  } else {
    overlapEnd <- end2
  }
  
  return(c(overlapStart, overlapEnd))
}

#Find unapproved periods
findDisapproval <- function(approvalList) {
  
  #Start times of unapproved periods
  unAppStart <- vector()
  #End times of unapproved periods
  unAppEnd <- vector()
  #When and who did the unapproving
  unAppDateApplied <- vector()
  unAppUser <- vector()
  
  #Loop through each approval transaction with a level less than 1200 see if it overlaps any periods that were previously 1200
  toCheck <- approvalList[approvalList$ApprovalLevel < 1200,]
  approved <- approvalList[approvalList$ApprovalLevel == 1200,]
  if(nrow(toCheck) > 0 & nrow(approved) > 0) {
    for(i in 1:nrow(toCheck)) {
      checkStart <- toCheck$StartTime[i]
      checkEnd <- toCheck$EndTime[i]
      #Check this start and end against every entry in the approved list
      for(j in 1:nrow(approved)) {
        appStart <- approved$StartTime[j]
        appEnd <- approved$EndTime[j]
        overlap <- isOverlap(checkStart, checkEnd, appStart, appEnd)
        #See if the approval was applied before the lower rating
        later <- toCheck$DateAppliedUtc[i] > approved$DateAppliedUtc[j]
        if(overlap & later) {
          period <- whatOverlap(checkStart, checkEnd, appStart, appEnd)
          unAppStart[length(unAppStart) + 1] <- as.character(period[1], format="%Y-%m-%d %H:%M")
          unAppEnd[length(unAppEnd) + 1] <- as.character(period[2], format="%Y-%m-%d %H:%M")
          unAppDateApplied[length(unAppDateApplied) + 1] <- as.character(toCheck$DateAppliedUtc[i], format="%Y-%m-%d %H:%M")
          unAppUser[length(unAppUser) + 1] <- toCheck$User[i]
        }
      }
    }
  }
  unApprovedPeriods <- data.frame(unAppStart, unAppEnd, unAppDateApplied, unAppUser)
  names(unApprovedPeriods) <- c("StartDate", "EndDate", "WhenUnapproved", "User")
  dup <- duplicated(unApprovedPeriods[,1:2])
  unApprovedPeriods <- unApprovedPeriods[!dup,]
  return(unApprovedPeriods)
  
}

#Get a token for using the API
getToken <- function(id, pw) {
  url <- paste0("http://ts.nwis.usgs.gov/AQUARIUS/Publish/V2/GetAuthToken?username=",
                id, "&encryptedPassword=", pw)
  resp <- GET(url)
  cookies <- resp$cookies[,"value"]
  names(cookies) <- resp$cookies[,"name"]
  return(cookies)
}

#Quick check if the toekn is still valid
testToken <- function() {
  
  test <- try({getTimeSeriesIDs("06892513", "Specific cond at 25C")}, silent=TRUE)
  if(class(test) != "try-error") {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#Get raw data for the time series between the start and end dates
getRawData <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesRawData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset")
  values <- c(tsID, start, end, "PointsOnly", "0")
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$Points
  out[,2] <- out[,2][,1]
  out$Timestamp <- as.POSIXct(out$Timestamp, format="%Y-%m-%dT%H:%M:%S")
  names(out) <- c("datetime", "raw")
  out <- na.omit(out)
  return(out)
}

#Get corrected data for a given time series
getCorrectedData <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesCorrectedData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset")
  values <- c(tsID, start, end, "PointsOnly", "0")
  corrected <- genericAQ(serviceRequest, parameters, values)
  out <- corrected$Points
  out[,2] <- out[,2][,1]
  out$Timestamp <- as.POSIXct(out$Timestamp, format="%Y-%m-%dT%H:%M:%S")
  names(out) <- c("datetime", "corrected")
  out <- na.omit(out)
  return(out)
}


#Get a list of correction objects from a time series ID and start and end times
getCorrections <- function(tsID, start, end) {
  corrections <- list()
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo")
  values <- c(tsID, start, end)
  temp <- genericAQ("GetCorrectionList", parameters, values)
  if(length(temp$Corrections) > 0) {
    temp <- temp$Corrections
    temp <- temp[temp$Type=="USGSMultiPoint",]
  }
  if(nrow(temp) > 0) {
    temp$StartTime <- gsub("T", " ", temp$StartTime)
    temp$EndTime <- gsub("T", " ", temp$EndTime)
    temp$StartTime <- substr(temp$StartTime, 1, regexpr("\\.", temp$StartTime)[1]-1)
    temp$EndTime <- substr(temp$EndTime, 1, regexpr("\\.", temp$EndTime)[1]-1)
    temp$StartTime <- as.POSIXct(temp$StartTime, format="%Y-%m-%d %H:%M:%S")
    temp$EndTime <- as.POSIXct(temp$EndTime, format="%Y-%m-%d %H:%M:%S")
    parameters <- temp$Parameters
    for(j in 1:nrow(temp)) {
      startTime <- temp[j, "StartTime"] 
      endTime <- temp[j, "EndTime"]
      set <- as.numeric(substr(parameters[j, "UsgsType"], 5, 5))
      endShiftPoints <- parameters[j, "EndShiftPoints"][[1]]
      startShiftPoints <- parameters[j, "StartShiftPoints"][[1]]
      startValues <- startShiftPoints$Value
      startOffsets <- startShiftPoints$Offset
      endValues <- endShiftPoints$Value
      endOffsets <- endShiftPoints$Offset
      try({corrections[[length(corrections) + 1]] <- new("multiPointCorrection", startTime = startTime, endTime = endTime, set = set, 
        startValues = startValues, startOffsets = startOffsets, endValues = endValues, endOffsets = endOffsets)}, silent=TRUE)
    }
  }
  return(corrections)
}

#Function to get the gap tolerance for a time series
getGapTolerance <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesCorrectedData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset", "IncludeGapMarkers")
  values <- c(tsID, start, end, "MetadataOnly", "0", "true")
  metadata <- genericAQ(serviceRequest, parameters, values)
  out <- metadata$GapTolerances
  out$StartTime <- as.POSIXct(out$StartTime, format="%Y-%m-%dT%H:%M")
  out$EndTime <- as.POSIXct(out$EndTime, format="%Y-%m-%dT%H:%M")
  return(out)
}

#-----------------------------------------------------------------------------------------------------------------------------------
# PROCESSING CORRECTIONS
#-----------------------------------------------------------------------------------------------------------------------------------

#Return the full effect of a correction given a raw value, starting points and ending points
fullCorrection <-function(raw, values, offsets) {
  corrPoints <- data.frame(values, offsets)
  #Order the fraom in increasing order of value
  corrPoints <- corrPoints[order(corrPoints$values),]
  check <- raw > corrPoints$values
  if(all(!check)) { #The raw value is less than the lowest value
    full <- corrPoints[corrPoints$value == min(corrPoints$value), "offsets"]
  } else if(all(check)) { #The raw value is greater than the highest value
    full <- corrPoints[corrPoints$value == max(corrPoints$value), "offsets"]
  } else { # The raw value is between two values
    #Find which two points bracket the raw value
    distance <- raw - corrPoints$values
    upperD <- max(distance[distance < 0])
    lowerD <- min(distance[distance > 0])
    upperV <- raw - upperD
    lowerV <- raw - lowerD
    upperO <- corrPoints[corrPoints$value == upperV, "offsets"]
    lowerO <- corrPoints[corrPoints$value == lowerV, "offsets"]
    full <- (((raw - lowerV) * (upperO - lowerO)) / (upperV - lowerV)) + lowerO
  }
  return(full)
}

#Return a function to calculate the time proration factor (between 0 and 1) for a starTime and endTime
tFactorFunction <- function(startTime, endTime) {
  tFactor <- function(time) {
    return(as.numeric(difftime(time, startTime, units='hours'))/as.numeric(difftime(endTime, startTime, units='hours')))
  }
  return(tFactor)
}

#Calculate a correction based on the raw values, time and correction object
calcCorrection <- function(raw, time, correction) {
  if(time <= correction@endTime & time >= correction@startTime) {
    tFactor <- tFactorFunction(correction@startTime, correction@endTime)
    startFull <- fullCorrection(raw, correction@startValues, correction@startOffsets)
    endFull <- fullCorrection(raw, correction@endValues, correction@endOffsets)
    correction <- startFull + (endFull - startFull) * tFactor(time)
  } else {
    correction <- 0
  }
  return(correction)
  
}

#Apply a correction (correction object) to a time series
applyCorrection <- function(timeSeries, correction, to="raw") {
  calc <- function(x, y) {
    calcCorrection(x, y, correction)
  }
  timeSeries$inCorr <- timeSeries$datetime > correction@startTime & timeSeries$datetime < correction@endTime
  timeSeries$corrValue <- 0
  timeSeries$corrValue[timeSeries$inCorr] <- 
    mapply(calc, timeSeries[timeSeries$inCorr,to], timeSeries[timeSeries$inCorr,"datetime"])
  return(timeSeries$corrValue)
}

#Cycle through the corrections and calculate the magnitude of the fouling corrections
applyFouling <- function(timeSeries, corrections, to="raw") {
  
  timeSeries$foulingCorrection <- 0
  for(i in corrections) {
    if((!all(c(i@startOffsets, i@endOffsets)==0)) & i@set == 1) {
      timeSeries$foulingCorrection <- timeSeries$foulingCorrection + unlist(applyCorrection(timeSeries, i, to))
    }
  }
  return(timeSeries$foulingCorrection)
}

#Cycle through the corrections and calculate the magnitude of the drift corrections
applyDrift <- function(timeSeries, corrections, to="raw") {
  
  timeSeries$driftCorrection <- 0
  for(i in corrections) {
    if((!all(c(i@startOffsets, i@endOffsets)==0)) & i@set == 2) {
      timeSeries$driftCorrection <- timeSeries$driftCorrection + unlist(applyCorrection(timeSeries, i, to))
    }
  }
  return(timeSeries$driftCorrection)
}

corrApply <- function(tsID, startDate, endDate) {
  
  #Get raw time series 
  timeSeries <- getRawData(tsID, startDate, endDate)
  #Download corrections
  corrections <- getCorrections(tsID, startDate, endDate)
  #Calculate the fouling corrections
  timeSeries$fouling <- applyFouling(timeSeries, corrections)
  timeSeries$foulingPercent <- (timeSeries$fouling/timeSeries$raw) * 100
  timeSeries$foulingCorrected <- timeSeries$raw + timeSeries$fouling
  #Calculate the drift corrections
  timeSeries$drift <- applyDrift(timeSeries, corrections, to="foulingCorrected")
  timeSeries$driftPercent <- (timeSeries$drift/timeSeries$foulingCorrected) * 100
  timeSeries$netCorrection <- timeSeries$fouling + timeSeries$drift
  timeSeries$netPercent <- timeSeries$netCorrection/timeSeries$raw * 100
  timeSeries$sumNumerical <- abs(timeSeries$drift) + abs(timeSeries$fouling)
  timeSeries$sumPercent <- abs(timeSeries$foulingPercent) + abs(timeSeries$driftPercent)
  timeSeries$Final <- timeSeries$raw + timeSeries$netCorrection
  #Round the columns
  timeSeries$fouling <- signif(timeSeries$fouling, 3)
  timeSeries$foulingPercent <- signif(timeSeries$foulingPercent, 3)
  timeSeries$foulingCorrected <- signif(timeSeries$foulingCorrected, 3)
  timeSeries$drift <- signif(timeSeries$drift, 3)
  timeSeries$driftPercent <- signif(timeSeries$driftPercent, 3)
  timeSeries$netCorrection <- signif(timeSeries$netCorrection, 3)
  timeSeries$netPercent <- signif(timeSeries$netPercent, 3)
  timeSeries$sumNumerical <- signif(timeSeries$sumNumerical, 3)
  timeSeries$sumPercent <- signif(timeSeries$sumPercent, 3)
  timeSeries$Final <- signif(timeSeries$Final, 3)
  return(timeSeries)
}

#Take a time series and grade it based on what parameter it is
wagnerGrade <- function(parameter, values, percent=NULL, numeric=NULL) {
  
  if(!(is.null(percent))) {
    percent <- abs(percent)
    percent <- percent/100
    percent <- round(percent, digits=4)
  }
  if(!(is.null(numeric))) {
    numeric <- abs(numeric)
    numeric <- round(numeric, 2)
  }
  if(parameter == "Specific cond at 25C") {
    pPoint <- 0
    pExcellent <- c(0, 0.03)
    pGood <- c(0.03, 0.10)
    pFair <- c(0.10, 0.15)
    pPoor <- c(0.15, 0.30)
    pDel <- c(0.30, Inf)
    nExcellent <- c(-1, -1)
    nGood <- c(-1, -1)
    nFair <- c(-1, -1)
    nPoor <- c(-1, -1)
    nDel <- c(-1, -1)
  } else if(parameter == "Turbidity, FNU") {
    pPoint <- 10
    pExcellent <- c(0, 0.05)
    pGood <- c(0.05, 0.10)
    pFair <- c(0.10, 0.15)
    pPoor <- c(0.15, 0.30)
    pDel <- c(0.30, Inf)
    nExcellent <- c(0, 0.5)
    nGood <- c(0.5, 1.0)
    nFair <- c(1.0, 1.5)
    nPoor <- c(1.5, 3)
    nDel <- c(3, Inf)
  } else if(parameter == "pH") {
    pPoint <- Inf
    pExcellent <- c(-1, -1)
    pGood <- c(-1, -1)
    pFair <- c(-1, -1)
    pPoor <- c(-1, -1)
    pDel <- c(-1, -1)
    nExcellent <- c(0, 0.2)
    nGood <- c(0.2, 0.5)
    nFair <- c(0.5, 0.8)
    nPoor <- c(0.8, 2)
    nDel <- c(2, Inf)
  } else if(parameter == "Dissolved oxygen") {
    pPoint <- 6
    pExcellent <- c(0, 0.05)
    pGood <- c(0.05, 0.10)
    pFair <- c(0.10, 0.15)
    pPoor <- c(0.15, 0.20)
    pDel <- c(0.20, Inf)
    nExcellent <- c(0, 0.3)
    nGood <- c(0.3, 0.5)
    nFair <- c(0.5, 0.8)
    nPoor <- c(0.8, 2)
    nDel <- c(2, Inf)
  } else if(parameter == "Temperature, water") {
    pPoint <- Inf
    pExcellent <- c(-1, -1)
    pGood <- c(-1, -1)
    pFair <- c(-1, -1)
    pPoor <- c(-1, -1)
    pDel <- c(-1, -1)
    nExcellent <- c(0, 0.2)
    nGood <- c(0.2, 0.5)
    nFair <- c(0.5, 0.8)
    nPoor <- c(0.8, 2)
    nDel <- c(2, Inf)
  }
  
  grade <- vector()
  
  for(i in 1:length(values)) {
   if(values[i] >= pPoint) {
     excellent <- (percent[i] <= pExcellent[2])
     good <- (percent[i] > pGood[1] & percent[i] <= pGood[2])
     fair <- (percent[i] > pFair[1] & percent[i] <= pFair[2])
     poor <- (percent[i] > pPoor[1] & percent[i] <= pPoor[2])
     considerDeletion <- (percent[i] > pDel[1])
   } else {
     excellent <- (numeric[i] <= nExcellent[2])
     good <- (numeric[i] > nGood[1] & numeric[i] <= nGood[2])
     fair <- (numeric[i] > nFair[1] & numeric[i] <= nFair[2])
     poor <- (numeric[i] > nPoor[1] & numeric[i] <= nPoor[2])
     considerDeletion <- (numeric[i] > nDel[1])
   }
    
    if(sum(c(excellent, good, fair, poor, considerDeletion)) != 1) {
      print("SUM != 1")
    }
    if(excellent) {
      grade[i] <- "Excellent"
    } else if(good) {
      grade[i] <- "Good"
    } else if(fair) {
      grade[i] <- "Fair" 
    } else if(poor) {
      grade[i] <- "Poor"
    } else if(considerDeletion) {
      grade[i] <- "Consider Deletion"
    } else {
      grade[i] <- "Ungraded"
    }
  }
  
  return(grade)
  
}

recordCopmleteness <- function(datetimes, start = "auto", end = "auto", freq = "auto") {
  #If the freq is "auto", use the most common value for the frequency, otherwise use the value supplied
  if(freq == "auto") {
    #Find the time differences between each point
    diff <- vector()
    diff[1] <- 0
    for(i in 2:length(datetimes)) {
      diff[i] <- difftime(datetimes[i], datetimes[i-1], units="mins")
    }
    freq_use <- unique(diff)[which.max(tabulate(match(diff, unique(diff))))]
  } else {
    freq_use <- freq
  }
  #Based on the frequency, find how many points should be present in the datetimes
  #If no start and end dates are specified, use the start and end times from the datetime vector
  if(start == "auto") {
    start_use <- min(datetimes)
  } else {
    start_use <- start
  }
  if(end=="auto") {
    end_use <- max(datetimes)
  } else {
    end_use <- end
  }
  
  time_span <- as.numeric(difftime(end_use, start_use, units="mins"))
  ifComplete <- floor(time_span/freq_use) + 1
  observed <- length(datetimes)
  completeness <- length(datetimes) / ifComplete
  comp_character <- paste(round(completeness * 100, 1), "%")
  
  output <- data.frame(
    start_date = as.character(start_use),
    end_date = as.character(end_use),
    completeness = comp_character,
    frequency_minutes = round(freq_use, 0),
    points_expected = round(ifComplete, 0),
    points_observed = round(observed, 0)
  )
  
  return(output)
}

#Find what percentage of the record falls into a gap based on a dataframe of gap toleranes OR 
#a manually specified gap tolerance in minutes
findGaps <- function(datetimes, gapTol = 120) {
  
  diff <- vector()
  diff[1] <- 0
  for(i in 2:length(datetimes)) {
    diff[i] <- difftime(datetimes[i], datetimes[i-1], units="mins")
  }
  gap <- rep(FALSE, length(datetimes))
  gapTimes <- data.frame(datetime = datetimes, diff = diff, gap = gap)
  if(class(gapTol) == "data.frame") {
    for(i in 1:nrow(gapTol)) {
      temp <- ifelse(gapTimes$datetime >= gapTol$StartTime[i] & 
                       gapTimes$datetime <= gapTol$EndTime[i] & 
                       gapTimes$diff > gapTol$ToleranceInMinutes[i], 
                     TRUE, FALSE)
      gapTimes$gap <- gapTimes$gap | temp
    }
  } else if(class(gapTol) == "numeric") {
    gapTimes$gap <- gapTimes$diff > gapTol
  }
  return(gapTimes)
}



