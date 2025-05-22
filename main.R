library(tidyverse)
library(lubridate)
library(plotly)
library(astsa)
library(tseries)

sp500 <- read_csv("./data/GSPC.csv")

filtered <- sp500 %>% filter(Date >= as.Date("2019-01-01"))

plot_ly(data = filtered, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',
        line = list(color = 'darkblue')) %>%
  layout(title = "S&P 500 záróárfolyam (2019-től)",
         xaxis = list(title = "Dátum"),
         yaxis = list(title = "Záróár (USD)"),
         hovermode = "x unified")


filtered["log_close"]=log(filtered$Close + 1)

plot_ly(data = filtered, x = ~Date, y = ~log_close, type = 'scatter', mode = 'lines',
        line = list(color = 'darkblue')) %>%
  layout(title = "S&P 500 záróárfolyam (2019-től)",
         xaxis = list(title = "Dátum"),
         yaxis = list(title = "Záróár (USD)"),
         hovermode = "x unified")

filtered_ts <- ts(filtered$log_close)

# TODO komment, magyarázat, értelmezése
acf1(filtered_ts)
Box.test(filtered_ts, lag = 20, type = "Ljung-Box")

acf(filtered_ts)
pacf(filtered_ts)

#Dickey-Fuller statisztika: minél negatívabb, annál inkább stacionárius.
adf_test_result <- adf.test(filtered_ts)
adf_test_result
