library(rugarch)

data <- get_clean_financial_data("^GSPC", from = "2010-01-01")

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

forecast_arima <- forecast(fit2, h = 10)
autoplot(forecast_arima)

cat("AIC	-23945.77	✅ alacsony → jó illeszkedés \n")
cat("Log-likelihood	11978.88	magasabb LL = jobb illesztés\n")
cat("BIC	-23908.20	kissé kevésbé kedvező mint AIC\n")
cat("Ljung-Box Q	69.436 (df = 6)	❌ p < 0.05 → rossz reziduum\n")
cat("p-value	5.338e-13	❌ szignifikáns → reziduum nem fehér zaj\n")

fit3 <- Arima(diff_log_returns, order = c(4, 0, 2))
AIC(fit3)
checkresiduals(fit3)



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


