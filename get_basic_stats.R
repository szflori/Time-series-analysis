library(moments)
library(xts)

get_basic_stats <- function(data){
  mean_ret <- mean(data$log_return)
  sd_ret <- sd(data$log_return)
  skew_ret <- skewness(data$log_return)
  kurt_ret <- kurtosis(data$log_return)
  
  cat("Átlagos napi loghozam: ", round(mean_ret, 6), "\n")
  cat("Szórás: ", round(sd_ret, 6), "\n")
  cat("Ferdeség (skewness): ", round(skew_ret, 3), "\n")
  cat("Csúcsosság (kurtosis): ", round(kurt_ret, 3), "\n")
  
  return(list(mean_ret, sd_ret, skew_ret, kurt_ret))
}