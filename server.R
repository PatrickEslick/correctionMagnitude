
library(shiny)
library(httr)
source("functions.R")

shinyServer(function(input, output) {
    
   tsChoices <- reactive({
     
     if(testToken() == FALSE) {
       getToken(id = Sys.getenv("apiid"), pw = Sys.getenv("apipw"))
     }
     
     if(input$site != "") {
       ts <- getTimeSeriesIDs(input$site, input$parameter)
       if(nrow(ts) > 0) {
         ts <- ts[,c("Identifier", "UniqueId")]
       } else {
         ts <- data.frame(Identifier="None", UniqueId="None")
       }
     } else {
       ts <- data.frame(Identifier="None", UniqueId="None")
     }
     ts
   })

   output$tsUI <- renderUI({
     timeSeries <- tsChoices()
     ch <- split(timeSeries$UniqueId, timeSeries$Identifier)
     selectInput("tsID", "Time series", ch, width="100%")
   })
   
   output$startSelect <- renderUI({
     dateInput("start", "Start")
   })
   
   output$endSelect <- renderUI({
     start <- input$start
     maxDate <- start + as.difftime(72, units="weeks")
     dateInput("end", "End", max=maxDate)
   })
   
   output$text <- renderText({
     input$go
     isolate({
       loc <- input$site
       parm <- input$parameter
       start <- input$start
       end <- input$end
     })
     if(loc != "") {
       text <- paste0("Showing data for: ", loc, "; ", parm, " from ", start, " to ", end)
     } else {
       text <- ""
     }
     text
   })
   
   table <- reactive({
     
     input$go 
  
     if(testToken() == FALSE) {
       getToken()
     }
     
     isolate({
       location <- input$site
       start <- as.character(input$start)
       end <- as.character(input$end)
       tsID <- input$tsID
       parm <- input$parameter
     })
     
     if(location != "") {
       #Get data and apply corrections
      output <- corrApply(tsID, start, end)
      #Only keep data that's in the final data
      correctedData <- getCorrectedData(tsID, start, end)
      output <- output[output$datetime %in% correctedData$datetime,]
      #Give the data a grade
      output <- na.omit(output)
      grade <- wagnerGrade(parm, output$raw, output$sumPercent, output$sumNumeric)
      output$Grade <- grade
     } else {
       output <- data.frame()
     }
     output
   })
   
   output$summary <- renderTable({
     data <- table()
     if(nrow(data) != 0) {
       excellentPercent <- nrow(data[data$Grade == "Excellent",])/nrow(data)
       goodPercent <- nrow(data[data$Grade == "Good",])/nrow(data)
       fairPercent <- nrow(data[data$Grade == "Fair",])/nrow(data)
       poorPercent <- nrow(data[data$Grade == "Poor",])/nrow(data)
       delPercent <- nrow(data[data$Grade == "Consider Deletion",])/nrow(data)
       
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
     summary
   })
   
   output$table <- renderDataTable({
     table()
   })
   
   output$unApprovedTable <- renderTable({
     tsID <- input$tsID
     table <- findDisapproval(getApprovalList(tsID, "0002-01-01", "9998-02-01"))
     table
   })
   
   output$completeTable <- renderTable({
     
     freq <- input$complete_freq
     if(freq == 0) {
       freq <- "auto"
     }
     datetimes <- table()$datetime
     
     recordCopmleteness(datetimes, freq = freq)
     
   })
  
})
