#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
# Define UI 
shinyUI(fluidPage(
  # Application title
  titlePanel("Correction magnitude table"),
  
  mainPanel(
    fluidRow(
      column(3, 
        textInput("site", "Location", placeholder="XXXXXXXX"),
        selectInput("parameter", "Parameter", choices=c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", 
                                                        "pH", "Temperature, water"))
      ),
      # column(3, dateRangeInput("dateRange", "Date range", start="2017-01-01", end=as.Date(Sys.time()), format="yyyy-mm-dd")),
      column(2, 
        uiOutput("startSelect"),
        uiOutput("endSelect"),
        helpText("Maximum date range is 18 months")
      ),
      column(3, uiOutput("tsUI")),
      column(2, 
        helpText("Click to run time series (no need to click for approvals tab)"), 
        actionButton("go", "Go")
      )
    ),
    textOutput("text"),
    tabsetPanel(
      tabPanel("Information", includeMarkdown("help.Rmd")),
      tabPanel("Summary", tableOutput("summary")),
      tabPanel("Time Series", dataTableOutput("table")),
      tabPanel("Approval", 
        helpText("All unapproved periods:"),
        tableOutput("unApprovedTable")#,
        # helpText("Approval record for selected time period:"),
        # tableOutput("approvalTable")
      )
    )
  )

))
