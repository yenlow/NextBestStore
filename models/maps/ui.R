require(shiny)
require(shinythemes)
require(shinydashboard)
require(DT)
require(htmlwidgets)
require(sparkline)  #AWS has the latest github version
suppressPackageStartupMessages(require(googleVis))

source("sparkline.R")

shinyUI( dashboardPage(
  #fluidPage(theme = shinytheme("united"), 
  skin="yellow",
  dashboardHeader(title = "Next Best Store"),
  dashboardSidebar(disable=T),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .jqstooltip{
                      box-sizing: content-box;
                      }"))
    ),
    
    fluidRow(
      column(2,
             HTML('<a href="https://people.stanford.edu/yenlow/insight"><button type="button">Project website</button></a>'),
             br(),br(),
#             numericInput("num",label="Show (number of locations) with the",value=10),
             sliderInput("num",label="Number of locations",
                         min=0,max=1000,value=100),
             radioButtons("highOrFast", "",
                          c("Highest total revenue"="high",
                            "Fastest revenue growth"="fast"),
                          selected="high")
      ),
      column(6,
             tabBox(
               title = "Map",color="yellow",
               # The id lets us use input$mapTab on the server to find the current tab
               id = "mapTab", 
               height = "370px", width="100%",
               tabPanel("New stores", value="new", htmlOutput("newmap")),
               tabPanel("Our stores", value="old", htmlOutput("oldmap")),
               tabPanel("New vs our stores", value="compare", htmlOutput("comparemap"))
             )   
      ),
      column(4, align="left",
             #input.highOrFast instead of input$highOrFast
             #if not existing locations, show potential revenue gain
             conditionalPanel("input.mapTab != 'old'",
                              infoBoxOutput("gain",width=NULL),
                              align="center"
             ),
             
             #if existing stores, show drugs sold
             conditionalPanel("input.mapTab == 'old'",
                              textInput("zip",label = "Enter zip code to view the drugs sold"),
                              htmlOutput("zip")
            ),
            
            #if comparing predicted and existing stores, show legend
            conditionalPanel("input.mapTab == 'compare'",
#                             infoBoxOutput("gain",width=NULL),
                             imageOutput("legend",height=80),
				h4("Markers sized by annual revenue")
            )
      )
    ),
    
    fluidRow(
	br(),br(),
      h3("About the Location", align="center"),    
      sparklineOutput("spark"),   #even though this line points to an orphaned output$spark, it's required for rendering sparklines
      dataTableOutput("table")
    )
      ))
    )
