#' Total Delay Rate(TDR, %)
#'
#' This function calculates total delay rate(TDR, %). It follows <Formula 7-1> or <Formula 7-2>
#' @param method *Categorical* Observation method. Choose one from : \code{'actual'}, \code{'theory'}
#' @param travel_time_actual *Numeric* Values of Actual travel time(s). Input data : \code{c(num1, num2, ...)}. It's length must be same as travel_time_desired's.It would be used if \code{method = 'actual'}
#' @param travle_time_desired *Numeric* Values Desired Travel time(s). Input data : \code{c(num1, num2, ...)}. It's length must be same as travel_time_actual's. It would be used if \code{method = 'actual'}
#' @param V_d *Numeric* Traffic Volume in direction(pc/lane). It would be used if \code{method = 'theory'}
#' @keywords TDR Total Delay Rate
#' @export TDR Total Delay Rate(%)
#' @examples
#' TDR(method = 'actual', travel_time_actual = c(10, 20, 10, 14, 12), travel_time_desired = c(8, 15, 8, 7, 6))
#' TDR(method = 'theory', V_d = 300)
TDR <- function(method = NULL, travel_time_actual = NULL, travel_time_desired = NULL, V_d = NULL){
  if (method == 'actual'){
    if (length(travel_time_actual) == length(travel_time_desired)){
      ts <- 0
      for (i in 1:length(travel_time_actual)){
        s <-  (travel_time_actual[i] - travel_time_desired[i])/travel_time_actual[i]
        ts <- ts + s
      }
      tdr <- ts * 100 / length(travel_time_actual)
    }
  }
  if (method == 'theory'){
    if (V_d > 0 & V_d <= 200){tdr <- (1 - exp(-0.0008*(V_d**0.8650))) * 100}
    if (V_d > 200 & V_d <= 400){tdr <- (1 - exp(-0.0010*(V_d**0.8397))) * 100}
    if (V_d > 400 & V_d <= 600){tdr <- (1 - exp(-0.0016*(V_d**0.7934))) * 100}
    if (V_d > 600 & V_d <= 1000){tdr <- (1 - exp(-0.0020*(V_d**0.7754))) * 100}
    if (V_d > 1000){tdr <- (1 - exp(-0.0040*(V_d**0.6856))) * 100}
  }
  tdr
}
