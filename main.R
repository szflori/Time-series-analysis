library(tidyverse)
library(lubridate)
library(plotly)
library(astsa)
library(tseries)
library(forecast)

# 1. Adatbetöltés és előkészítés
# Adat betöltése
sp500 <- read_csv("./data/GSPC.csv")

# Szűrés 2019-től
log_500 <- sp500 %>%
  filter(Date >= as.Date("2019-01-01")) %>% 
  mutate(log_close = log(Close + 1)) # Logtranszformáció, ha nincs 0 érték, elég a log(Close)

# Eredeti és log-árfolyam plot
plot_ly(data = log_500, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines') %>%
  layout(title = "S&P 500 záróárfolyam (2019-től)")

plot_ly(data = log_500, x = ~Date, y = ~log_close, type = 'scatter', mode = 'lines') %>%
  layout(title = "Log-záróárfolyam (2019-től)")


# 2. ACF és PACF vizsgálat, stacionaritás teszt
# Idősor objektummá alakítás
log_500_ts <- ts(log_500$log_close, frequency = 252)  # 252 trading days/year

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

# 4. Modell illesztése
fit <- arima(diff_ts, order = c(1, 0, 1))
summary(fit)
checkresiduals(fit)


