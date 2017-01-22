#Richard Shanahan  
#https://github.com/rjshanahan  
#rjshanahan@gmail.com
#20 January 2017

###### TIME SERIES FORECASTING WITH SHINY ###### 


################# 1. LOAD PACKAGES & SETUP SESSION #################

## load packages
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(devtools)
library(forecastHybrid)
library(forecast)
library(data.table)
library(DT)
library(plotly)
library(dygraphs)
library(dplyr)

#set theme for ggplot2 graphics
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top",
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust =1.25))


#read in reference CSV for public holidays - dummy variables
pubhol <- read.csv('AuPublicHolidays.csv',
                   header=T,
                   sep=",",
                   quote='"',
                   strip.white=T,
                   stringsAsFactors=F,
                   fill=T)

pubhol$Date <- zoo::as.Date(pubhol$Date, format = "%d/%m/%Y")
pubhol$weekday <- weekdays(pubhol$Date)



################# 2. SHINY SERVER FUNCTION #################


server <- function(input,output,session) {
  
  
  ###################################################################
  #########              1 TIME SERIES FORECASTING           ########
  ###################################################################
  
  ####### READ IN CSV FILE BASED ON SELECTION ####### 
  mySeries_raw <- reactive({
    
    inFile <- input$i_file
    
    if (is.null(inFile))
      return(NULL)
    
    mySeries <- read.csv(inFile$datapath, 
             header = T,
             strip.white=T,
             stringsAsFactors=F,
             fill=T)
    
    
    ####### DYNAMIC DROP DOWN LIST FOR VARIABLES BASED ON INPUT FILE #######
    # updateSelectInput(session,
    #                   'i_task_select',
    #                   label = 'Select Task',
    #                   choices = names(mySeries[,-1]),
    #                   names(mySeries[,-1])[1])

    #convert date format
    mySeries$Date <- zoo::as.Date(mySeries$Date, format = "%d/%m/%Y")
    
    #remove NAs
    mySeries <- mySeries %>% filter(!is.na(Date))
    
    #add 'weekday' variable
    mySeries$weekday <- weekdays(mySeries$Date)
    
    #add public holiday variable for use as exogenous variable
    mySeries$pubhol <- ifelse(mySeries$Date %in% pubhol$Date,
                              1,
                              0)
    
    #assign id field for visualisations
    mySeries$id <- 1:nrow(mySeries)
    
    mySeries <- mySeries
    
  })
  
  
  ####### DYNAMIC DROP DOWN LIST FOR TASK BASED ON INPUT FILE ####### 
  observeEvent(mySeries_raw(), {
    
    mySeries <- mySeries_raw()
    
    updateSelectInput(session, 
                      'i_task_select', 
                      label = 'Select Series',
                      choices = names(select(mySeries, -Date, -pubhol, -id, -weekday)),
                      names(select(mySeries, -Date, -pubhol, -id, -weekday))[1])
    
  })
    


  ####### REACTIVE FILTERED DATAFRAME ####### 
  mySeries_filtered <- eventReactive(input$goButton, {
    
    #dependency on 'start forecasting' button being pressed
    #input$goButton
    
    if (nrow(mySeries_raw())==0) 
      return()
    
    #clear out performance comparison table
    performance$performance_compare <- data.frame(
      metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1'))
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    #BUILD DATAFRAME
    if (weekends == "no"){
      mySeries_filtered <- mySeries %>% 
        #select(Date, pubhol, id, weekday, task_type) %>%
        select_(.dots = list(quote(Date), quote(pubhol), quote(id), quote(weekday), task_type)) %>%
        filter_(paste0('!', quote(weekday),' %in% c("Saturday","Sunday")'))
    }
    else
    {
      mySeries_filtered <- mySeries %>% 
        select_(.dots = list(quote(Date), quote(pubhol), quote(id), quote(weekday), task_type))
    }
    
  })
  
  
  ###### OBJECT TO STORE PERFORMANCE METRICS ######
  performance <- reactiveValues(
    
    performance_compare = data.frame(
      metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1')
    )
    
  )
  
  
  #######  FOURIER TRANSFORM + AUTO ARIMA DYGRAPH  ####### 
  output$p_FTARIMA <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_FTARIMA_daily <- mySeries_filtered()
    
    if (nrow(mySeries_FTARIMA_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    

    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })

    #convert to TS object with weekly frequency
    myY <-  ts(select_(mySeries_FTARIMA_daily, task_type),
               freq=365.25/7)               #weekly frequency
    
    #fit model
    withProgress(message = 'Optimising forecast model... ', 
                 detail = 'this may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   
                   bestfit <- list(aicc=Inf)
                   for(i in 2:25)
                   {
                     #for progress bar
                     incProgress(0.1)
                     
                     
                     #create XREG object
                     if (dummy == "no"){
                       myXReg_fit <- fourier(myY, K=i)
                       myXReg_forecast <- fourier(myY, 
                                                  K=i-1, 
                                                  h=forecast_n)
                     }
                     else {
                       myXReg_fit <- cbind(fourier(myY, K=i),
                                           select_(mySeries_FTARIMA_daily, quote(pubhol)))
                       myXReg_forecast <- cbind(fourier(myY, K = i - 
                                                          1, h = forecast_n), 
                                                ifelse(seq.Date(as.Date(last(select_(mySeries_FTARIMA_daily, quote(Date)))[[1]]), 
                                                                as.Date(last(select_(mySeries_FTARIMA_daily, quote(Date)))[[1]]) + forecast_n, by = "days") %in% select_(pubhol, quote(Date)), 1, 0))
                       
                     }
                     
                     
                     fit <- auto.arima(myY,
                                       xreg = myXReg_fit,
                                       seasonal=FALSE)
                     
                     if(fit$aicc < bestfit$aicc)
                       bestfit <- fit
                     else break;
                   }
                   
                 })
    
    #forecast n periods using model
    TS_mySeries_FTARIMA_daily <- forecast(bestfit,
                                          xreg = myXReg_forecast)
    
    
    fit_FTARIMA_daily_df <- cbind(as.data.frame(TS_mySeries_FTARIMA_daily$fitted)[1:nrow(mySeries_FTARIMA_daily),],
                                  select_(mySeries_FTARIMA_daily, quote(Date)))
    
    colnames(fit_FTARIMA_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_FTARIMA_daily_df <- with(TS_mySeries_FTARIMA_daily,
                                      data.frame(mean=TS_mySeries_FTARIMA_daily$mean,
                                                 upper=TS_mySeries_FTARIMA_daily$upper[,2],
                                                 lower=TS_mySeries_FTARIMA_daily$lower[,2]))
    forecast_FTARIMA_daily_df$Date <- seq(max(mySeries_FTARIMA_daily$Date),
                                          max(mySeries_FTARIMA_daily$Date)+forecast_n-1,
                                          1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_FTARIMA_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_FTARIMA_daily_df$Date <- mySeq
    }
    
    
    
    #forecasted values table
    output$time_series_table <- renderDataTable({
      
      
      forecast_FTARIMA_daily_df <- forecast_FTARIMA_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
      
    })
    
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('FTARIMA','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_FTARIMA_daily_df, file)
    })
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_FTARIMA_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(FTARIMA = myAccuracy[, 2]))
      })
      
    })
    
    
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_FTARIMA_daily, series = task_type),
               select_(mySeries_FTARIMA_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_FTARIMA_daily$Date))
    
    myfitted <- xts(select_(fit_FTARIMA_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_FTARIMA_daily$Date))
    
    myPred <- xts(select_(forecast_FTARIMA_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_FTARIMA_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('ARIMA with FOURIER FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  #######  SIMPLE EXPONENTIAL SMOOTHING DYGRAPH  ####### 
  output$p_SES <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_SES_daily <- mySeries_filtered()
    
    if (nrow(mySeries_SES_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    #simple exponential smoothing
    myY <-  ts(select_(mySeries_SES_daily, task_type),
               freq=365.25/7)   
    
    TS_mySeries_SES_daily <- ses(myY, h=forecast_n, initial='optimal')
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_SES_daily_df <- cbind(as.data.frame(TS_mySeries_SES_daily$fitted)[1:nrow(mySeries_SES_daily),],
                              select_(mySeries_SES_daily, quote(Date)))
    
    colnames(fit_SES_daily_df) <- c('fitted','Date')
    
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_SES_daily_df <- with(TS_mySeries_SES_daily,
                                  data.frame(mean=TS_mySeries_SES_daily$mean,
                                             upper=TS_mySeries_SES_daily$upper[,2],
                                             lower=TS_mySeries_SES_daily$lower[,2]))
    forecast_SES_daily_df$Date <- seq(max(mySeries_SES_daily$Date),
                                      max(mySeries_SES_daily$Date)+forecast_n-1,
                                      1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_SES_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_SES_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_SES_daily_df <- forecast_SES_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('SES','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_SES_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_SES_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(SES = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_SES_daily, series = task_type),
               select_(mySeries_SES_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_SES_daily$Date))
    
    myfitted <- xts(select_(fit_SES_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_SES_daily$Date))
    
    myPred <- xts(select_(forecast_SES_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_SES_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('SIMPLE EXPONENTIAL SMOOTHING of ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  
  #######  HOLT-WINTERS EXPONENTIAL SMOOTHING DYGRAPH  ####### 
  output$p_HW <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_HW_daily <- mySeries_filtered()
    
    if (nrow(mySeries_HW_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    #HW exponential smoothing
    myY <-  ts(select_(mySeries_HW_daily, task_type),
               freq=7)   
    
    #HW smoothing
    TS_mySeries_HW_daily <- ets(myY,
                                model="ZAN",
                                damped=F,
                                additive.only=T
                                #gamma=myM
    )
    
    
    
    
    #create fit object
    fit_HW_daily_df <- cbind(as.data.frame(TS_mySeries_HW_daily$fitted)[1:nrow(mySeries_HW_daily),],
                             select_(mySeries_HW_daily, quote(Date)))
    
    colnames(fit_HW_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_HW_daily <- forecast(TS_mySeries_HW_daily, h=forecast_n)
    forecast_HW_daily_df <- with(forecast_HW_daily,
                                 data.frame(mean=forecast_HW_daily$mean,
                                            upper=forecast_HW_daily$upper[,2],
                                            lower=forecast_HW_daily$lower[,2]))
    forecast_HW_daily_df$Date <- seq(max(mySeries_HW_daily$Date),
                                     max(mySeries_HW_daily$Date)+forecast_n-1,
                                     1)
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_HW_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_HW_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_HW_daily_df <- forecast_HW_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('HW','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_HW_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_HW_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(HW = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_HW_daily, series = task_type),
               select_(mySeries_HW_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_HW_daily$Date))
    
    myfitted <- xts(select_(fit_HW_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_HW_daily$Date))
    
    myPred <- xts(select_(forecast_HW_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_HW_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('HOLT-WINTERS EXPONENTIAL SMOOTHING of ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  #######  MOVING AVERAGE DYGRAPH  ####### 
  output$p_MA <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_MA_daily <- mySeries_filtered()
    
    if (nrow(mySeries_MA_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    myMA = 7
    
    #convert to time series object
    myY <-  ts(select_(mySeries_MA_daily, series = task_type),
               freq=7)   
    
    withProgress(message = 'Optimising forecast model... ', 
                 detail = 'this may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   
                   #create moving average forecast object
                   TS_mySeries_MA_daily <- forecast(ma(myY, 
                                                       order = myMA),
                                                    h = forecast_n)
                 })
    
    
    #convert elements of time series MODEL to dataframe for plotting
    #fit_MA_daily_df <- select_(mySeries_MA_daily, task_type, quote(Date))
    
    fit_MA_daily_df <- cbind(as.data.frame(TS_mySeries_MA_daily$fitted)[1:nrow(mySeries_MA_daily),],
                             select_(mySeries_MA_daily, quote(Date)))
    
    colnames(fit_MA_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_MA_daily_df <- with(TS_mySeries_MA_daily,
                                 data.frame(mean=TS_mySeries_MA_daily$mean,
                                            upper=TS_mySeries_MA_daily$upper[,2],
                                            lower=TS_mySeries_MA_daily$lower[,2]))
    forecast_MA_daily_df$Date <- seq(max(mySeries_MA_daily$Date),
                                     max(mySeries_MA_daily$Date)+forecast_n-1,
                                     1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_MA_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_MA_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_MA_daily_df <- forecast_MA_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('MA','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_MA_daily_df, file)
    })
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_MA_daily, x=mySeries_MA_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(MA = myAccuracy[, 2]))
      })
      
    })
    
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_MA_daily, series = task_type),
               select_(mySeries_MA_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_MA_daily$Date))
    
    myfitted <- xts(select_(fit_MA_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_MA_daily$Date))
    
    myPred <- xts(select_(forecast_MA_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_MA_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0(myMA, '-ORDER MOVING AVERAGE of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      #dyRoller(rollPeriod = myMA) %>%     #lets users enter a 'smoothing' parameter interactively
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  
  #######  MEAN FORECAST SMOOTHING DYGRAPH  ####### 
  output$p_MF <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_MF_daily <- mySeries_filtered()
    
    if (nrow(mySeries_MF_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    #simple exponential smoothing
    myY <-  ts(select_(mySeries_MF_daily, task_type),
               freq=365.25/7)   
    
    TS_mySeries_MF_daily <- meanf(myY, 
                                  h=forecast_n)
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_MF_daily_df <- cbind(as.data.frame(TS_mySeries_MF_daily$fitted)[1:nrow(mySeries_MF_daily),],
                             select_(mySeries_MF_daily, quote(Date)))
    
    colnames(fit_MF_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_MF_daily_df <- with(TS_mySeries_MF_daily,
                                 data.frame(mean=TS_mySeries_MF_daily$mean,
                                            upper=TS_mySeries_MF_daily$upper[,2],
                                            lower=TS_mySeries_MF_daily$lower[,2]))
    forecast_MF_daily_df$Date <- seq(max(mySeries_MF_daily$Date),
                                     max(mySeries_MF_daily$Date)+forecast_n-1,
                                     1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_MF_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_MF_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_MF_daily_df <- forecast_MF_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('MF','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_MF_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_MF_daily, x=mySeries_MF_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(MF = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_MF_daily, series = task_type),
               select_(mySeries_MF_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_MF_daily$Date))
    
    myfitted <- xts(select_(fit_MF_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_MF_daily$Date))
    
    myPred <- xts(select_(forecast_MF_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_MF_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('MEAN FORECAST USING IID of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  #######  LINE OF BEST FIT LINEAR DYGRAPH  ####### 
  output$p_LOBF <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_LOBF_daily <- mySeries_filtered()
    
    if (nrow(mySeries_LOBF_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    
    #line of best fit
    TS_mySeries_LOBF_daily <- lm(paste0(task_type, ' ~ ', quote(Date)), 
                                 data = mySeries_LOBF_daily)
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_LOBF_daily_df <- cbind(as.vector(TS_mySeries_LOBF_daily$fitted.values),
                               select_(mySeries_LOBF_daily, quote(Date)))
    
    colnames(fit_LOBF_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    myTS <-  ts(fit_LOBF_daily_df$fitted,
                freq=365.25/7)  
    
    TS_mySeries_LOBF_daily <- forecast(myTS, 
                                       h=forecast_n)
    
    forecast_LOBF_daily_df <- with(TS_mySeries_LOBF_daily,
                                   data.frame(mean=TS_mySeries_LOBF_daily$mean,
                                              upper=TS_mySeries_LOBF_daily$upper[,2],
                                              lower=TS_mySeries_LOBF_daily$lower[,2]))
    forecast_LOBF_daily_df$Date <- seq(max(mySeries_LOBF_daily$Date),
                                       max(mySeries_LOBF_daily$Date)+forecast_n-1,
                                       1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_LOBF_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_LOBF_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_LOBF_daily_df <- forecast_LOBF_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('LOBF','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_LOBF_daily_df, file)
    })
    
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_LOBF_daily, x=mySeries_LOBF_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(LOBF = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_LOBF_daily, series = task_type),
               select_(mySeries_LOBF_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_LOBF_daily$Date))
    
    myfitted <- xts(select_(fit_LOBF_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_LOBF_daily$Date))
    
    myPred <- xts(select_(forecast_LOBF_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_LOBF_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('LINE OF BEST FIT of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  #######  TBATS STATE SPACE DYGRAPH  ####### 
  output$p_TBATS <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_TBATS_daily <- mySeries_filtered()
    
    if (nrow(mySeries_TBATS_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    #simple exponential smoothing
    myY <-  ts(select_(mySeries_TBATS_daily, task_type),
               freq=365.25/7)   
    
    withProgress(message = 'Optimising forecast model... ', 
                 detail = 'this may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   setProgress()
                   
                   #TBATS smoothing            
                   TS_mySeries_TBATS_daily <- tbats(myY)
                 })
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_TBATS_daily_df <- cbind(as.data.frame(TS_mySeries_TBATS_daily$fitted)[1:nrow(mySeries_TBATS_daily),],
                                select_(mySeries_TBATS_daily, quote(Date)))
    
    colnames(fit_TBATS_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_TBATS_daily <- forecast(TS_mySeries_TBATS_daily, h=forecast_n)
    forecast_TBATS_daily_df <- with(forecast_TBATS_daily,
                                    data.frame(mean=forecast_TBATS_daily$mean,
                                               upper=forecast_TBATS_daily$upper[,2],
                                               lower=forecast_TBATS_daily$lower[,2]))
    forecast_TBATS_daily_df$Date <- seq(max(mySeries_TBATS_daily$Date),
                                        max(mySeries_TBATS_daily$Date)+forecast_n-1,
                                        1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_TBATS_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_TBATS_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_TBATS_daily_df <- forecast_TBATS_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('TBATS','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_TBATS_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_TBATS_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(TBATS = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_TBATS_daily, series = task_type),
               select_(mySeries_TBATS_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_TBATS_daily$Date))
    
    myfitted <- xts(select_(fit_TBATS_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_TBATS_daily$Date))
    
    myPred <- xts(select_(forecast_TBATS_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_TBATS_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('TBATS STATE SPACE MODEL of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      # dySeries("series") %>%
      # dySeries("fitted", fillGraph = TRUE) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  
  #######  COMBINED FORECAST DYGRAPH  ####### 
  output$p_COMBINE <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_COMB_daily <- mySeries_filtered()
    
    if (nrow(mySeries_COMB_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    isolate({
      weekends <- input$i_weekends
      if(input$i_task_select ==""){
        task_type = select_(mySeries, .dots = list(quote(-Date), quote(-pubhol), quote(-id), quote(-weekday)))
        task_type = names(task_type[1])
      } else
      {
        task_type = input$i_task_select
      }
      forecast_n <- input$i_forecast_n
      dummy <- input$i_dummy
    })
    
    #create ts object
    myY <-  ts(select_(mySeries_COMB_daily, task_type),
               freq=7)   
    
    
    withProgress(message = 'Optimising forecast model... ', 
                 detail = 'this may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   
                   
                   #create XREG object
                   if (dummy == "no"){
                     myXReg_fit <- fourier(myY, K=2)
                     myXReg_forecast <- fourier(myY, 
                                                K=2, 
                                                h=forecast_n)
                   }
                   else {
                     myXReg_fit <- cbind(fourier(myY, K = 2),
                                         select_(mySeries_COMB_daily, quote(pubhol)))
                     myXReg_forecast <- cbind(fourier(myY, K = 2, h = forecast_n), 
                                              ifelse(seq.Date(as.Date(last(select_(mySeries_COMB_daily, quote(Date)))[[1]]), 
                                                              as.Date(last(select_(mySeries_COMB_daily, quote(Date)))[[1]]) + forecast_n, by = "days") %in% select_(pubhol, quote(Date)), 1, 0))
                     
                   }
                   
                   #cast myY as numeric for 'hybridModel' function
                   myY <- as.numeric(myY)
                   
                   #COMBINED Forecast using forecastHybrid package
                   TS_mySeries_COMB_daily <- hybridModel(myY, 
                                                         models = "aens",
                                                         errorMethod = "RMSE",
                                                         weights = "insample.errors",
                                                         a.args = list(xreg=myXReg_fit),
                                                         n.args = list(xreg=myXReg_fit))
                   
                 })
    
    
    #create fit object
    fit_COMB_daily_df <- cbind(as.data.frame(TS_mySeries_COMB_daily$fitted)[1:nrow(mySeries_COMB_daily),],
                               select_(mySeries_COMB_daily, quote(Date)))
    
    colnames(fit_COMB_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_COMB_daily <- forecast(TS_mySeries_COMB_daily, 
                                    h=forecast_n,
                                    xreg=myXReg_forecast)
    forecast_COMB_daily_df <- with(forecast_COMB_daily,
                                   data.frame(mean=forecast_COMB_daily$mean,
                                              upper=forecast_COMB_daily$upper[,2],
                                              lower=forecast_COMB_daily$lower[,2]))
    forecast_COMB_daily_df$Date <- seq(max(mySeries_COMB_daily$Date),
                                       max(mySeries_COMB_daily$Date)+forecast_n-1,
                                       1)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    if (weekends == "no") {
      
      mySeq <- seq.Date(as.Date(first(select_(forecast_COMB_daily_df, quote(Date)))[[1]]), length.out = forecast_n*2, by = "days")
      
      mySeq <- mySeq[!weekdays(mySeq) %in% c('Saturday', 'Sunday')][0:forecast_n]
      
      forecast_COMB_daily_df$Date <- mySeq
    }
    
    
    output$time_series_table <- renderDataTable({
      
      forecast_COMB_daily_df <- forecast_COMB_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('COMBINED','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_COMB_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_COMB_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(COMB = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_COMB_daily, series = task_type),
               select_(mySeries_COMB_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_COMB_daily$Date))
    
    myfitted <- xts(select_(fit_COMB_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_COMB_daily$Date))
    
    myPred <- xts(select_(forecast_COMB_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_COMB_daily_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred)
    
    
    d <- dygraph(myDy[,1:6][,-3], main=paste0('COMBINED FORECAST MODEL of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2016-3-27", "Easter 2016", labelLoc = "bottom") %>%
      dyEvent("2015-4-5", "Easter 2015", labelLoc = "bottom") %>%
      dyEvent("2014-4-20", "Easter 2014", labelLoc = "bottom") %>%
      dyEvent("2013-3-31", "Easter 2013", labelLoc = "bottom") %>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  

  
  
  ###### PERFORMANCE METRIC COMPARISON TABLE ######
  output$t_COMPARE <- renderDataTable({
    
    performance$performance_compare
    
  })
  
  
  
}


