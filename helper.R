
make_diff_series <- function(series){
  diff_series <- diff(series) %>% na.omit()
  return(diff_series)
}