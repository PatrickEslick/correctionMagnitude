library(shiny)
library(httr)

shinyUI(fluidPage(
  
  titlePanel("Correction magnitude table"),
  
  mainPanel(
    fluidRow(
      column(3, 
        textInput("site", "Location", placeholder="XXXXXXXX"),
        selectInput("parameter", "Parameter", choices=c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", 
                                                        "pH", "Temperature, water"))
      ),
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
      tabPanel("Record completeness", 
        selectInput("gapTolerance", label = "Gap Tolerance (in minutes)",
                    choices = c("Use AQUARIUS gap tolerance" = 0, 
                                30, 60, 120, 180, 240, 300, 360, 720),
                    selected = "Use AQUARIUS gap tolerance"),
        tableOutput("gapTable"),
        selectInput("complete_freq", label = "Observation frequency (in minutes)",
                    choices = c("Auto detect" = 0, 15, 30, 60, 120), selected = "Auto detect"),
        tableOutput("completeTable")
      ),
      tabPanel("Approval", 
        helpText("All unapproved periods:"),
        tableOutput("unApprovedTable")
      )
    )
  )

))
