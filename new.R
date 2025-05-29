library(rugarch)
library(forecast)
library(quantmod)
library(xts)
library(timeDate)
library(plotly)

data <- get_clean_financial_data("^GSPC", from = "2019-01-01")

basic_stats <- get_basic_stats(data)

check_stationarity(data$log_return)
visual_stationarity_diagnostics(data$log_return, window = 1000, name = "S&P 500 loghozam")

diff_log_returns <- make_diff_series(data$log_return)

check_stationarity(diff_log_returns)
visual_stationarity_diagnostics(diff_log_returns, window = 1000, name = "S&P 500 loghozam")

# ARIMA modell három komponensből áll:
# p = autoregresszív (AR) rend (PACF segítségével azonosítható),
# d = differenciálás foka (ha stacionáriussá tetted: d = 0 vagy 1),
# q = mozgóátlag (MA) rend (ACF alapján dönthető el).

# ACF
# Csak 1. lag szignifikáns → gyors levágás	MA(1) → ARIMA(0,d,1)
# ACF fokozatosan csökken → lassú lecsengés	AR komponens → ARIMA(p,d,0)
# PACF is levág az 1. lag után	kombinált → ARIMA(1,d,1)

# PACF
# Ha a PACF is lassan csökken (nem vág le) → nem lehet sima AR modell (AR(1) vagy AR(2)), mert ott a PACF levág.
# Ha több lag átlépi a konfidencia sávot, és nincs egyértelmű levágás → szükség lehet több AR komponensre (pl. AR(2), AR(3)).
# Mivel az ACF gyorsan levág (csak az 1. lag szignifikáns) → az MA komponens valószínűleg alacsony rendű (pl. MA(1)).

fit1 <- Arima(diff_log_returns, order = c(2, 0, 1))
fit2 <- Arima(diff_log_returns, order = c(3, 0, 1))

AIC(fit1); AIC(fit2);
# AIC alacsonyabb → jobb illeszkedés a bonyolultsággal együtt.

checkresiduals(fit2)
logLik(fit2)
BIC(fit2)

forecast_arima <- forecast(fit2, h = 30)
summary(forecast_arima)
plot(forecast_arima)


autoplot(forecast_arima)

last_price <- as.numeric(last(Cl(data$data)))

cum_return <- cumsum(forecast_arima$mean)

price_forecast <- last_price * exp(cum_return)

plot(price_forecast, type = "l", col = "darkgreen", lwd = 2,
     main = "Előrejelzett árfolyam (ARIMA alapján)",
     xlab = "Nap", ylab = "Ár")

future_dates <- timeSequence(from = index(Cl(data$data))[nrow(Cl(data$data))] + 1,
                             length.out = length(price_forecast),
                             by = "day")
future_dates <- as.Date(future_dates[isBizday(future_dates)])

price_future <- xts(price_forecast[1:length(future_dates)], order.by = future_dates)

price_hist <- Cl(data$data)
price_combined <- rbind(price_hist, price_future)

# Kezdő dátum az idősorhoz
start_date <- as.Date("2019-01-01")

# Átalakítás data.frame-é (feltételezve napi adatok)
observed_data <- data.frame(
  Date = seq.Date(from = start_date, by = "day", length.out = length(data$log_return)),
  Close = as.numeric(data$log_return)
)

# Alapár (utolsó megfigyelt záróár)
last_price <- as.numeric(tail(observed_data$Close, 1))

# Várható kumulált loghozam előrejelzés
cum_mean <- cumsum(forecast_arima$mean)
cum_lower80 <- cumsum(forecast_arima$lower[,1])
cum_upper80 <- cumsum(forecast_arima$upper[,1])
cum_lower95 <- cumsum(forecast_arima$lower[,2])
cum_upper95 <- cumsum(forecast_arima$upper[,2])

# Árfolyam előrejelzés visszaskálázása
forecast_price <- last_price * exp(cum_mean)
lower80_price <- last_price * exp(cum_lower80)
upper80_price <- last_price * exp(cum_upper80)
lower95_price <- last_price * exp(cum_lower95)
upper95_price <- last_price * exp(cum_upper95)

forecast_data <- data.frame(
  Date = forecast_dates,
  Forecast = forecast_price,
  Lower80 = lower80_price,
  Upper80 = upper80_price,
  Lower95 = lower95_price,
  Upper95 = upper95_price
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

# ------------------------------------------------


cat("AIC	-23945.77	✅ alacsony → jó illeszkedés \n")
cat("Log-likelihood	11978.88	magasabb LL = jobb illesztés\n")
cat("BIC	-23908.20	kissé kevésbé kedvező mint AIC\n")
cat("Ljung-Box Q	69.436 (df = 6)	❌ p < 0.05 → rossz reziduum\n")
cat("p-value	5.338e-13	❌ szignifikáns → reziduum nem fehér zaj\n")

fit3 <- Arima(diff_log_returns, order = c(4, 0, 2))
AIC(fit3)
checkresiduals(fit3)
# ------------------------------------------------



cat("A reziduum továbbra sem fehér zaj, sőt: még rosszabb lett a p-érték (előző modellnél p ≈ 5e-13 helyett most 1.8e-5, de még mindig szignifikáns).")
cat("az ARIMA modell struktúrája jól követi az átlagos mozgást (trend, impulzus),\n
de nem képes modellezni a maradékban rejlő dinamikát → különösen a szórás időbeli változását.")

squared_return <- diff_log_returns^2

acf(squared_return, main = "ACF - loghozam^2")
# Nagyon magas érték az első 1–3 lagre (0.6–0.4 körül),
# Lassú lecsengés (exponenciális vagy geometriai),
# Ez nem levágás, hanem fokozatos csökkenés.
# GARCH struktúra (GARCH komponens, azaz p ≥ 1).

pacf(squared_return, main = "PACF - loghozam^2")
# Az első 2–3 lag szignifikánsan pozitív (0.4, 0.3, 0.15 körül),
# Az első 2–3 lag szignifikánsan pozitív (0.4, 0.3, 0.15 körül),
# Ez ARCH komponens jelenlétére utal (ARCH q ≥ 1...2).

# GARCH(1,1)	Alapmodell, ha gyorsan csökken az ACF
# GARCH(1,2)	Ha PACF-ben több lag is szignifikáns
# GARCH(2,1)	Ha ACF-ben 2–3 lag is jelentős marad
# GARCH(2,2)	Ha mindkét sorozatban több szignifikáns érték látható

# GARCH(1,1) specifikáció
spec11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"  # t-eloszlás jobban kezeli a "fat tails"-t
)

fit11  <- ugarchfit(spec = spec11, data = diff_log_returns)

# Próbálj GARCH(1,2)
spec12 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"  # t-eloszlás jobban kezeli a "fat tails"-t
)
fit12 <- ugarchfit(spec12, data = diff_log_returns)

# GARCH(2,1)
spec21 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"  # t-eloszlás jobban kezeli a "fat tails"-t
)
fit21 <- ugarchfit(spec21, data = diff_log_returns)

# GARCH(2,2)
spec22 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"  # t-eloszlás jobban kezeli a "fat tails"-t
)
fit22 <- ugarchfit(spec22, data = diff_log_returns)

# Hasonlítsd össze:
infocriteria(fit11)
infocriteria(fit12)
infocriteria(fit21)
infocriteria(fit22)

forecast_garch <- ugarchforecast(fit21, n.ahead = 10)
sigma_forecast <- forecast_garch@forecast$sigmaFor
print(sigma_forecast)

plot(sigma_forecast, type = "l", main = "GARCH(2,1) – 10 napos volatilitás előrejelzés", ylab = "σ(t)", xlab = "Nap")


