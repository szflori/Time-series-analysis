library(quantmod)
library(dplyr)

get_clean_financial_data <- function(symbol = "^GSPC",
                                     from = "2010-01-01",
                                     to = Sys.Date(),
                                     periodicity = "daily") {
  
  # Adatok lekérése Yahoo Finance-ből
  getSymbols(symbol, src = "yahoo", from = from, to = to,
             periodicity = periodicity, auto.assign = FALSE) -> raw_data
  
  # NA értékek eltávolítása
  raw_data <- na.omit(raw_data)
  
  # Záróár kinyerése (Close oszlop)
  close_prices <- Cl(raw_data)
  
  # Loghozam számítása
  log_returns <- log(close_prices) %>% na.omit()
  
  # Visszatérés listaként
  return(list(
    data= raw_data,
    price = close_prices,
    log_return = log_returns
  ))
}
