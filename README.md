# Time Series Forecasting with Shiny

## Launch the Shiny app here

This <a href="http://shiny.rstudio.com/" target="_blank">Shiny</a> app provides an interactive user interface to forecast time series. 
  
Users can upload their own time series CSV with a single or multiple series. The user interace allows users to compare fitted time series models and forecasts with several algorithms including:
- line of best fit (regression)
- moving average
- exponential smoothing
- ARIMA with Fourier Transform
- TBATS
- hybrid forecast ensemble model

Many of the models draw on the excellent <a href="http://robjhyndman.com/software/forecast/" target="_blank">```forecast```</a> package from Rob J Hyndman.
  
Visualisations are interactive using <a href="https://rstudio.github.io/dygraphs/" target="_blank">Dygraphs for R</a>.

Note: there are currently no file format validations built in. Files should have format as per the table below. An example is provided <a href="https://raw.githubusercontent.com/rjshanahan/Time-Series-Forecasting-with-Shiny/master/TIME_SERIES_DUMMY.csv" target="_blank">here</a>. 
  
|```Date```								| ```Series_1```| ```Series_n``` (optional) |
|:---------------------------------------------------|:-------|:---------------------|
|```dd/mm/yyyy```| ```series_1_value_1```	|	```series_n_value_1```|
|```dd/mm/yyyy```| ```series_1_value_2```	|	```series_n_value_2```|
|```dd/mm/yyyy```| ```series_1_value_n```	|	```series_n_value_n```|

