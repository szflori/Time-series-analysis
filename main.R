library(tidyverse)
library(lubridate)
library(plotly)
library(astsa)
library(tseries)
library(forecast)
library(rugarch)

# 1. Adatbetöltés és előkészítés
# Adat betöltése
sp500 <- read_csv("./data/GSPC.csv")

# Szűrés 2019-től
log_500 <- sp500 %>%
  filter(Date >= as.Date("2019-01-01")) %>% 
  mutate(log_return = log(Close + 1)) # Logtranszformáció, ha nincs 0 érték, elég a log(Close)

# Eredeti és log-árfolyam plot
plot_ly(data = log_500, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines') %>%
  layout(title = "S&P 500 záróárfolyam (2019-től)")

plot_ly(data = log_500, x = ~Date, y = ~log_return, type = 'scatter', mode = 'lines') %>%
  layout(title = "Log-záróárfolyam (2019-től)")


# 2. ACF és PACF vizsgálat, stacionaritás teszt
# Idősor objektummá alakítás
log_500_ts <- ts(log_500$log_return, frequency = 252)  # 252 trading days/year

# ACF, PACF vizsgálat
acf(log_500_ts, main = "ACF - log záróár")
pacf(log_500_ts, main = "PACF - log záróár")

# Ljung-Box teszt: autokorreláció jelenlétére
Box.test(log_500_ts, lag = 20, type = "Ljung-Box")

# Augmented Dickey-Fuller teszt a stacionaritásra
adf_test_result <- adf.test(log_500_ts)
print(adf_test_result)
cat("A nullhipotézist (nem stacionárius) nem tudjuk elutasítani → az idősor nem stacionárius.")

# 3. Stacionarizálás
# 1. differenciálás
digg_log_500 <- log_500 %>% mutate(log_return=diff(c(NA, log(Close)))) %>% drop_na()

plot_ly(digg_log_500, x = ~Date, y = ~log_return, type = "scatter", mode = "lines") %>%
  layout(title = "Loghozam - diff(log(záróár))")

diff_ts <- diff(log_500_ts)

acf(diff_ts)
pacf(diff_ts)
adf.test(diff_ts)

#GARCH
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = log_500_ts)
show(fit)
plot(fit)  # 12 alapgrafikon: reziduum, volatilitás, qq-plot, stb.


# 4. Modell illesztése
fit <- Arima(log_500_ts, order = c(2, 1, 1), include.drift = TRUE)
summary(fit)
checkresiduals(fit)

arima_model <- auto.arima(log_500_ts, seasonal = FALSE)
summary(arima_model)

forecast_fit <- forecast(fit, h = 30)


observed_data <- data.frame(
  Date = seq.Date(from = min(log_500$Date),
                  by = "day",
                  length.out = length(log_500_ts)),
  Close = as.numeric(log_500_ts)
)

# 1.2. Előrejelzett adatok (forecast objektumból)
forecast_dates <- seq.Date(
  from = max(observed_data$Date) + 1, 
  by = "day", 
  length.out = length(forecast_fit$mean)
)

forecast_data <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(forecast_fit$mean),
  Lower80 = forecast_fit$lower[,1],
  Upper80 = forecast_fit$upper[,1],
  Lower95 = forecast_fit$lower[,2],
  Upper95 = forecast_fit$upper[,2]
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

