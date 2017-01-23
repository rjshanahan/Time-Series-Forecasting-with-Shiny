# Time Series Forecasting with Shiny

## <a href="https://rjshanahan.shinyapps.io/Time-Series-Forecasting-with-Shiny/" target="_blank">Launch the Shiny app!</a>

This <a href="http://shiny.rstudio.com/" target="_blank">Shiny</a> app provides an interactive user interface to visualise and forecast time series. 
  
Users can upload their own CSV with single or multiple ```daily``` time series. The user interace allows users to compare fitted time series models and forecasts with several algorithms including:
- line of best fit (regression)
- moving average
- exponential smoothing (simple & Holt-Winters)
- ARIMA with Fourier Transform
- TBATS
- hybrid forecast ensemble model

## Packages Utilised
- Many of the models draw on the excellent <a href="http://robjhyndman.com/software/forecast/" target="_blank">```forecast```</a> package from Rob J Hyndman
- Visualisations are interactive using <a href="https://rstudio.github.io/dygraphs/" target="_blank">Dygraphs for R</a>
- and of course <a href="http://shiny.rstudio.com/" target="_blank">Shiny</a>. Excellent.
  
## Features
- interactive time series visualisations showing series, fitted & forecast
- forecast between 1 and 120 periods with 95% confidence intervals
- collect & compare forecasts using various models with the same parameters - see the ```compare``` tab. Note: changing parameters refreshes the ```compare``` view
- view and download forecasts and 95% confidence interval predictions
- include/exclude weekends
- adjust relevant models for shocks using exogenous dummy variables. Note: the current file used is a CSV of Australian public holidays for demo purposes only.
  
## File Format
Note: there are currently no file format validations built in. Files should have format as per the table below. An example is provided <a href="https://raw.githubusercontent.com/rjshanahan/Time-Series-Forecasting-with-Shiny/master/TIME_SERIES_DUMMY.csv" target="_blank">here</a>. 
  
|```Date```								| ```Series_1```| ```Series_n``` (optional) |
|:---------------------------------------------------|:-------|:---------------------|
|```dd/mm/yyyy```| ```series_1_value_1```	|	```series_n_value_1```|
|```dd/mm/yyyy```| ```series_1_value_2```	|	```series_n_value_2```|
|```dd/mm/yyyy```| ```series_1_value_n```	|	```series_n_value_n```|


## Known Issues
This is very much ```alpha``` with a number of issues to fix and features to add including:
- file format validations
- extend model to other series eg. minute, hourly, weekly etc
- handling for date formats other than ```dd/mm/yyyy```
- add conditional modelling parameters eg. moving average ```alpha``` and ```periods```
- ability to upload own exogenous variables
- ability to define series frequency - currently hardcoded to 365.25/7 (weekly)