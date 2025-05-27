library(tidyverse)
library(lubridate)
library(plotly)
library(astsa)
library(tseries)
library(forecast)

sp500 <- read_csv("./data/GSPC.csv")

filtered_sp500 <- sp500 %>% filter(Date >= as.Date("2019-01-01"))

plot_ly(data = filtered_sp500, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',
        line = list(color = 'darkblue')) %>%
  layout(title = "S&P 500 záróárfolyam (2019-től)",
         xaxis = list(title = "Dátum"),
         yaxis = list(title = "Záróár (USD)"),
         hovermode = "x unified")

summary(filtered_sp500$Close)
sd(filtered_sp500$Close)


filtered_sp500_ts <- ts(filtered_sp500$Close, frequency = 252)

# ACF (Autokorrelációs függvény) ábra
acf(filtered_sp500_ts, main = "ACF - S&P 500 záróárak")

# PACF (Parciális autokorrelációs függvény) ábra
pacf(filtered_sp500_ts, main = "PACF - S&P 500 záróárak")

diff_filtered_sp500_ts <- diff(filtered_sp500_ts)
adf.test(diff_filtered_sp500_ts)

diff_dates <- seq.Date(
  from = filtered_sp500$Date[2],    # Az első differenciált érték a második naphoz tartozik
  by = "day",
  length.out = length(diff_filtered_sp500_ts)
)

diff_df <- data.frame(
  Date = diff_dates,
  Change = as.numeric(diff_filtered_sp500_ts)
)

plot_ly(data = diff_df, 
        x = ~Date, 
        y = ~Change, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'tomato', width = 2),
        hoverinfo = 'text',
        text = ~paste('Dátum:', Date, '<br>Árváltozás:', round(Change,2), 'USD')) %>%
  layout(title = list(text = "Differenciált S&P 500 idősor (záróárkülönbségek)", x = 0.5),
         xaxis = list(title = "Dátum"),
         yaxis = list(title = "Árváltozás (USD)"),
         hovermode = "x unified")

acf(diff_filtered_sp500_ts, main = "ACF - Differenciált S&P 500 idősor")

# PACF a differenciált idősorra
pacf(diff_filtered_sp500_ts, main = "PACF - Differenciált S&P 500 idősor")


arima_model <- auto.arima(filtered_sp500_ts, seasonal = FALSE)
summary(arima_model)

fit <- arima(filtered_sp500_ts, order = c(2, 1, 1))
summary(fit)
sp500_forecast <- forecast(fit, h = 30)


observed_data <- data.frame(
  Date = seq.Date(from = min(filtered_sp500$Date),
                  by = "day",
                  length.out = length(filtered_sp500_ts)),
  Close = as.numeric(filtered_sp500_ts)
)

# 1.2. Előrejelzett adatok (forecast objektumból)
forecast_dates <- seq.Date(
  from = max(observed_data$Date) + 1, 
  by = "day", 
  length.out = length(sp500_forecast$mean)
)

forecast_data <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(sp500_forecast$mean),
  Lower80 = sp500_forecast$lower[,1],
  Upper80 = sp500_forecast$upper[,1],
  Lower95 = sp500_forecast$lower[,2],
  Upper95 = sp500_forecast$upper[,2]
)

plot_ly() %>%
  
  # Megfigyelt adatok vonal
  add_lines(data = observed_data, x = ~Date, y = ~Close, 
            name = "Megfigyelt", 
            line = list(color = 'darkblue')) %>%
  
  # Előrejelzés vonal
  add_lines(data = forecast_data, x = ~Date, y = ~Forecast, 
            name = "Előrejelzés", 
            line = list(color = 'red', dash = 'dash')) %>%
  
  # 80%-os előrejelzési intervallum (szalag)
  add_ribbons(data = forecast_data,
              x = ~Date,
              ymin = ~Lower80,
              ymax = ~Upper80,
              name = "80%-os intervallum",
              fillcolor = 'rgba(255, 0, 0, 0.2)',
              line = list(color = 'transparent')) %>%
  
  # 95%-os előrejelzési intervallum (szalag)
  add_ribbons(data = forecast_data,
              x = ~Date,
              ymin = ~Lower95,
              ymax = ~Upper95,
              name = "95%-os intervallum",
              fillcolor = 'rgba(255, 0, 0, 0.1)',
              line = list(color = 'transparent')) %>%
  
  # Layout beállítás
  layout(title = list(text = "S&P 500 záróárak előrejelzése (30 nap)", x = 0.5),
         xaxis = list(title = "Dátum"),
         yaxis = list(title = "Záróár (USD)"),
         hovermode = "x unified")


