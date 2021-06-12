#' Average cycling speed in bicycle road(km/h)
#'
#' It follows <Formula 15-17> in KHCM(2013), p.648
#' @param L_T Total section length(km)
#' @param L_i Subsection length (km)
#' @param S_i Propulsion speed of section i (kph)
#' @param d_j Average stopping delay for bicycles at j intersections (seconds)
#' @keywords
#' @export avg_speed_bike
#' @examples
#' avg_speed_bike(L_T = 10, L_i = c(3, 3.2, 3.8), S_i = c(15, 13.2, 10.5), d_j = c(300, 200, 400))
avg_speed_bike <- function(L_T = NULL, L_i = NULL, S_i = NULL, d_j = NULL){
  lssum <- 0
  dsum <- 0
  for (i in 1:length(L_i)){
    ls <- L_i[i]/S_i[i]
    d <- d_j[i]/3600
    lssum <- lssum + ls
    dsum <- dsum + d
  }
  spd <- L_T / (lssum + dsum)
  spd
}
