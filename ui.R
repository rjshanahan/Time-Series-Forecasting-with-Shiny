#Richard Shanahan  
#https://github.com/rjshanahan  
#rjshanahan@gmail.com
#20 January 2017

###### TIME SERIES FORECASTING WITH SHINY ###### 
# load required packages
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(dplyr)
library(devtools)
library(forecast)
library(plotly)
library(dygraphs)


#set theme for ggplot2 graphics
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top",
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust =1.25))


# Choices for drop-downs

myWeekends <- c("yes","no")

myDummy <- c("yes","no")

csvDummy <- 'https://raw.githubusercontent.com/rjshanahan/Time-Series-Forecasting-with-Shiny/master/demo_csv/TIME_SERIES_DUMMY.csv'

############################################################
## shiny user interface function
############################################################


shinyUI(
  

    
    navbarPage(
      
      
      title="Time Series Forecasting",
      theme="bootstrap.css",
      inverse=TRUE,


      ##### FORECASTING TAB  #####                
      tabPanel("Time Series Forecasting", icon = icon("line-chart"),
               
               fluidPage(
                 
                 titlePanel('Time Series Forecasting', windowTitle='Workload Forecast'),
                 
                 fluidRow(
                   sidebarPanel(
                     fileInput("i_file", "Upload your CSV file",
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                     ),
                     h5("Try it out - download a dummy ", a("CSV with three daily time series", href=csvDummy, target="_blank")),
                     h5('You can find many ', a("more time series here", href='https://datamarket.com/data/list/?q=interval:day%20provider:tsdl', target="_blank")),
                     tags$hr(),
                     selectInput(inputId="i_task_select", "Select Series",'',''),
                     sliderInput(inputId="i_forecast_n","Forecast periods",value=30,min=2,max=120,step=1),
                     radioButtons(inputId="i_weekends", "Include weekends", myWeekends, selected = myWeekends[2], inline = T),
                     radioButtons(inputId="i_dummy", "Adjust forecast for Events", myDummy, selected = myDummy[2], inline = T),
                     actionButton(inputId="goButton", "Start forecasting!"),
                     br(),
                     br(),
                     downloadButton('downloader', 'Download forecasts'),
                     width=3),
                     
                   
                   column(width=9,
                          
                          tabsetPanel(type="tabs",
                                      
                                      #build forecasting panels
                                      tabPanel("LOBF", icon = icon("bar-chart"), h4("Line of Best Fit (Linear Regression)"), br(), dygraphOutput("p_LOBF"), value=1),
                                      tabPanel("MF", icon = icon("bar-chart"), h4("Mean Forecast - Average of Entire Series"), br(), dygraphOutput("p_MF"), value=2),
                                      tabPanel("MA", icon = icon("line-chart"), h4("Moving Average Smoothing Model"), br(), dygraphOutput("p_MA"), value=3),
                                      tabPanel("SES", icon = icon("line-chart"), h4("Simple Exponential Smoothing Model"), br(), dygraphOutput("p_SES"), value=4),
                                      tabPanel("HW", icon = icon("line-chart"), h4("Holt-Winters Exponential Smoothing Model"), br(), dygraphOutput("p_HW"), value=5),
                                      tabPanel("FTARIMA", icon = icon("area-chart"), h4("Auto Regressive Integrated Moving Average (ARIMA) Model with Fourier Transform"), br(), dygraphOutput("p_FTARIMA"), value=6),
                                      tabPanel("TBATS", icon = icon("area-chart"), h4("TBATS Model: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend & Seasonal components"), br(), dygraphOutput("p_TBATS"), value=7),
                                      tabPanel("COMBINE", icon = icon("sliders"), h4("Hybrid Model with Weighted Forecasts based on Performance (ARIMA, Exponential, TBATS, Neural Network, STL)"), br(), dygraphOutput("p_COMBINE"), value=8),
                                      tabPanel("COMPARE", icon = icon("puzzle-piece"), h4("Compare Forecast Model Performance Metrics"), h6("Read more about ", a("forecasting accuracy here", href="https://en.wikipedia.org/wiki/Forecasting#Forecasting_accuracy", target="_blank")), br(), DT::dataTableOutput("t_COMPARE"), value=9, id="COMP_TAB"),
                                      
                                      id = "timeSeriesTabs"
                          )
                          )),

                 
                 fluidRow(
                   column(width=6,
                          h4("Forecast Values with Upper and Lower 95% Confidence Intervals"),
                          DT::dataTableOutput("time_series_table")),
                   column(width=6,
                          h4("Performance Metrics"),
                          DT::dataTableOutput("performance_metrics"))
                   ),
                 tags$div(id="cite",
                          'Want to know more?', 
                          a("visit my GitHub repository", href="https://github.com/rjshanahan/Time-Series-Forecasting-with-Shiny", target="_blank")
                          )
                 
               )
      )
    )
)


    

