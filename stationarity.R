library(tseries)
library(urca)
library(zoo)
library(xts)

check_stationarity <- function(series) {
  if (!("xts" %in% class(series))) {
    stop("A bemenetnek xts objektumnak kell lennie.")
  }
  
  cat("Stacionaritás vizsgálat loghozamra:\n")
  
  adf_result <- adf.test(series)
  cat("\n--- Augmented Dickey-Fuller teszt ---\n")
  print(adf_result)
  if (adf_result$p.value < 0.05) {
    cat("✅ ADF szerint a sor stacionárius.\n")
  } else {
    cat("❌ ADF szerint a sor nem stacionárius.\n")
  }
  
  kpss_result <- ur.kpss(series)
  cat("\n--- KPSS teszt ---\n")
  print(summary(kpss_result))
  if (kpss_result@teststat > kpss_result@cval[1]) {
    cat("❌ KPSS szerint a sor nem stacionárius.\n")
  } else {
    cat("✅ KPSS szerint a sor stacionárius.\n")
  }
  
  cat("\nℹ️ Javaslat: Ha az egyik teszt szerint nem stacionárius, differenciálni érdemes.\n")
}

visual_stationarity_diagnostics <- function(series, window = 100, name = "Idősor") {
  if (!("xts" %in% class(series))) {
    stop("A bemenetnek xts objektumnak kell lennie.")
  }
  
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  # 1. ACF
  acf(series, main = paste("ACF -", name), lag.max = 30)
  
  # 2 PACF
  pacf(series, main = paste("PACF -", name), lag.max = 30)
}
