#' Utilization efficiency coefficient according to the stopping surface(N)
#'
#' It follows <Table 13-9> KHCM(2013), p.599
#' @param l length of the bus stop(m)
#' @export N_bus_using_efficiency
#' @examples
#' N_bus_using_efficiency(43.22)
N_bus_using_efficiency <- function(l = NULL){
  if (l > 0 & l < 24){f <- 1.00}
  if (l >= 24 & l < 36){f <- 1.75}
  if (l >= 36 & l < 48){f <- 2.25}
  if (l >= 48 & l < 60){f <- 2.55}
  if (l >= 60){f <- 2.65}
  f
}
