
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
genericAQ <- function(serviceRequest, parameters, values, host="ts-api.nwis.usgs.gov") {
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
  out$DateAppliedUtc <- as.POSIXct(out$DateAppliedUtc, format="%Y-%m-%dT%H:%M", tz = "GMT")
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
  url <- paste0("http://ts-api.nwis.usgs.gov/AQUARIUS/Publish/V2/GetAuthToken?username=",
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

retryToken <- function(id, pw) {
  
  tkn <- getToken(id, pw)
  i <- 0
  while(!(testToken()) & i < 10) {
    Sys.sleep(1)
    tkn <- getToken(id, pw)
    i <- i + 1
  }
  
  return(tkn)
  
}

#Get raw data for the time series between the start and end dates
getRawData <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesRawData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset")
  values <- c(tsID, start, end, "PointsOnly", "0")
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$Points
  out[,2] <- out[,2][,1]
  out$Timestamp <- as.POSIXct(out$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
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
  out$Timestamp <- as.POSIXct(out$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
  names(out) <- c("datetime", "corrected")
  out <- na.omit(out)
  return(out)
}


#Get a list of correction objects from a time series ID and start and end times
getCorrections <- function(tsID, start, end) {
  
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
      corrections[[length(corrections) + 1]] <- new("multiPointCorrection", 
                                                         startTime = startTime, 
                                                         endTime = endTime, 
                                                         set = set, 
                                                         startValues = startValues, 
                                                         startOffsets = startOffsets, 
                                                         endValues = endValues, 
                                                         endOffsets = endOffsets)
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
  out$StartTime <- as.POSIXct(out$StartTime, format="%Y-%m-%dT%H:%M", tz="GMT")
  out$EndTime <- as.POSIXct(out$EndTime, format="%Y-%m-%dT%H:%M", tz="GMT")
  return(out)
}

#-----------------------------------------------------------------------------------------------------------------------------------
# PROCESSING CORRECTIONS
#-----------------------------------------------------------------------------------------------------------------------------------

#Apply a correction (correction object) to a time series
applyCorrection <- function(datetime, raw, correction) {
  
  #Find the time factor for each raw value
  tf <- approx(x = c(correction@startTime, correction@endTime),
               y = c(0, 1),
               xout = datetime,
               yleft = NA,
               yright = NA)$y
  
  #Interpolate correction start points for each raw value
  if(length(correction@startOffsets) > 1) {
    sc <- approx(x = correction@startValues, 
                 y = correction@startOffsets, 
                 xout = raw,
                 yleft = correction@startOffsets[which.min(correction@startValues)],
                 yright = correction@startOffsets[which.max(correction@startValues)])$y
  } else if(length(correction@startOffsets) == 1){
    sc <- rep(correction@startOffsets[1], length(raw))
  } else if (length(correction@startOffsets) == 0) {
    sc <- rep(0, length(raw))
  }
  
  #Interpolate correction end points for each raw values
  if(length(correction@endOffsets) > 1) {
    ec <- approx(x = correction@endValues, 
                 y = correction@endOffsets, 
                 xout = raw,
                 yleft = correction@endOffsets[which.min(correction@endValues)],
                 yright = correction@endOffsets[which.max(correction@endValues)])$y
  } else if(length(correction@endOffsets == 1)) {
    ec <- rep(correction@endOffsets[1], length(raw))
  } else if(length(correction@endOffsets == 0)) {
    ec <- rep(0, length(raw))
  }
  
  #Calculate the corrected value for each raw value
  correction_value <- ((1 - tf) * sc) + (tf * ec)
  
  correction_value[is.na(correction_value)] <- 0
  
  return(correction_value)
  
}

#Cycle through the corrections and calculate the magnitude of the fouling corrections
applyFouling <- function(timeSeries, corrections) {
  
  timeSeries$foulingCorrection <- 0
  for(i in corrections) {
    if((!all(c(i@startOffsets, i@endOffsets)==0)) & i@set == 1) {
      timeSeries$foulingCorrection <- timeSeries$foulingCorrection + 
        applyCorrection(timeSeries$datetime, timeSeries$raw, i)
    }
  }
  return(timeSeries$foulingCorrection)
}

#Cycle through the corrections and calculate the magnitude of the drift corrections
applyDrift <- function(timeSeries, corrections) {
  
  timeSeries$driftCorrection <- 0
  for(i in corrections) {
    if((!all(c(i@startOffsets, i@endOffsets)==0)) & i@set == 2) {
      timeSeries$driftCorrection <- timeSeries$driftCorrection + 
        applyCorrection(timeSeries$datetime, timeSeries$foulingCorrected, i)
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
  timeSeries$drift <- applyDrift(timeSeries, corrections)
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
wagnerGrade <- function(parameter, raw, percent, numerical) {
  
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
  
  num_correction <- abs(numerical)
  per_correction <- abs(percent) / 100
  use_percent <- raw > pPoint
  use_numeric <- !use_percent
  grade <- rep("Unassigned", length(num_correction))
  
  #Apply percentage grades
  grade[use_percent & per_correction <= pExcellent[2]] <- "Excellent"
  grade[use_percent & per_correction > pGood[1] & per_correction <= pGood[2]] <- "Good"
  grade[use_percent & per_correction > pFair[1] & per_correction <= pFair[2]] <- "Fair"
  grade[use_percent & per_correction > pPoor[1] & per_correction <= pPoor[2]] <- "Poor"
  grade[use_percent & per_correction > pDel[1]] <- "Consider Deletion"
  
  #Apply numeric grades
  grade[use_numeric & num_correction <= nExcellent[2]] <- "Excellent"
  grade[use_numeric & num_correction > nGood[1] & num_correction <= nGood[2]] <- "Good"
  grade[use_numeric & num_correction > nFair[1] & num_correction <= nFair[2]] <- "Fair"
  grade[use_numeric & num_correction > nPoor[1] & num_correction <= nPoor[2]] <- "Poor"
  grade[use_numeric & num_correction > nDel[1]] <- "Consider Deletion"
  
  return(grade)
}

recordCompleteness <- function(datetimes, start = "auto", end = "auto", freq = "auto") {
  
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

#Function to make the data table in the app
makeTable <- function(tsID, start, end, parm) {
  
  #Get data and apply corrections
  output <- corrApply(tsID, start, end)
  #Only keep data that's in the final data
  correctedData <- getCorrectedData(tsID, start, end)
  output <- output[output$datetime %in% correctedData$datetime,]
  #Give the data a grade
  output <- na.omit(output)
  grade <- wagnerGrade(parm, output$raw, output$sumPercent, output$sumNumeric)
  output$Grade <- grade
  return(output)
  
}

#Make a grade summary table from the data table
summarizeGrades <- function(dataTable) {
  
  if(nrow(dataTable) != 0) {
    excellentPercent <- nrow(dataTable[dataTable$Grade == "Excellent",])/nrow(dataTable)
    goodPercent <- nrow(dataTable[dataTable$Grade == "Good",])/nrow(dataTable)
    fairPercent <- nrow(dataTable[dataTable$Grade == "Fair",])/nrow(dataTable)
    poorPercent <- nrow(dataTable[dataTable$Grade == "Poor",])/nrow(dataTable)
    delPercent <- nrow(dataTable[dataTable$Grade == "Consider Deletion",])/nrow(dataTable)
    
    grades <- c(excellentPercent, goodPercent, fairPercent, poorPercent, delPercent)
    grades <- grades * 100
    grades <- round(grades, 2)
    grades <- paste0(as.character(grades), "%")
    rows <- c("Excellent", "Good", "Fair", "Poor", "Consider Deletion")
    summary <- data.frame(rows, grades)
    names(summary) <- c("Grade", "Percent")
  } else {
    summary <- data.frame()
  }
  return(summary)

}

#Summarize gaps based on the results of the findGaps function
#e.g. findGaps(...) %>% summarizeGaps()
summarizeGaps <- function(gapTest, gapTol) {
  
  start <- min(gapTest$datetime)
  start_char <- as.character(start)
  end <- max(gapTest$datetime)
  end_char <- as.character(end)
  time_span <- as.numeric(difftime(end, start, units="mins"))
  gaps <- length(gapTest$gap[gapTest$gap==TRUE])
  gap_time <- sum(gapTest$diff[gapTest$gap==TRUE])
  gap_percent <- round((gap_time / time_span) * 100, 1)
  gap_percent <- paste(gap_percent, "%")
  if(class(gapTol) == "numeric") {
    tolerance <- gapTol
  } else {
    if(min(gapTol$ToleranceInMinutes) == max(gapTol$ToleranceInMinutes)) {
      tolerance <- gapTol$ToleranceInMinutes[1]
    } else {
      tolerance <- "multiple"
    }
  }
  
  out <- data.frame(start_date = start_char, 
                    end_date = end_char, 
                    gap_percent, 
                    gaps, 
                    gap_time,
                    time_span,
                    tolerance)
  return(out)
}

makeTableConnect <- function(tsID, start, end, parm, id, pw) {
  
  tkn <- retryToken(id, pw)
  out <- makeTable(tsID, start, end, parm)
  
}