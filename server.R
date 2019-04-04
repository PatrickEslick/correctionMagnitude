
library(shiny)
library(httr)
library(dplyr)
library(future)
library(promises)
plan(multiprocess)
source("functions.R")

shinyServer(function(input, output) {
    
   tsChoices <- reactive({
     
     if(testToken() == FALSE) {
       getToken(id = Sys.getenv("apiid"), pw = Sys.getenv("apipw"))
     }
     
     if(nchar(input$site) >= 8) {
       ts <- getTimeSeriesIDs(input$site, input$parameter)
       if(!is.null(ts)) {
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
   
   #CONVERT THIS TO A FUTURE
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
       output <- future({ makeTableConnect(tsID, start, end, parm,
                                           id = Sys.getenv("apiid"),
                                           pw = Sys.getenv("apipw")) })
     } else {
       output <- future ({ data.frame() })
     }
     
   })
   
   output$summary <- renderTable({
     table () %...>%
       summarizeGrades()
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
     
     freq <- as.numeric(input$complete_freq)
     if(freq == 0) {
       freq <- "auto"
     }
     
     table() %...>%
       pull(datetime) %...>%
       recordCompleteness(freq = freq)
     
   })
   
   output$gapTable <- renderTable({
     
     input$go 
     
     if(testToken() == FALSE) {
       getToken()
     }
     
     isolate({
       location <- input$site
       start <- input$start
       end <- input$end
       tsID <- input$tsID
       parm <- input$parameter
     })
     
     gapTol <- as.numeric(input$gapTolerance)
    
     if(gapTol == 0) {
       gapTol <- getGapTolerance(tsID, 
                                 as.character(start, format="%Y-%m-%d"),
                                 as.character(end, format="%Y-%m-%d"))
     }
     
     table() %...>%
       pull(datetime) %...>%
       findGaps(gapTol) %...>%
       summarizeGaps(gapTol)
     
   })
  
})
