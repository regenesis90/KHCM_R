#' Bendiness in Multi-lane Road(B, degree/km)
#'
#' This function calculates horizontal curve curvature in multi-lane road. It follows <Formula 6-1> in KHCM(2013).
#' @param angles *Series* Series of each direct angle. \code{c(theta1, theta2, theta3, ...)} (degree)
#' @param L *Numeric* Length of the route(km). Generally, \code{L = 3}
#' @keywords bendiness Multilane road
#' @export Bendiness The sum of angles divided by L
#' @examples
#' B_multilane_road(angles = c(30, 20, 30, 40, 50), L = 3)
#' B_multilane_road(c(10, 8, 12, 14), 5)
B_multilane_road <- function(angles = NULL, L = NULL){
  if (L > 0 & is.null(angles) == FALSE){
    theta_sum <- 0
    for (i in 1:length(angles)){
      theta <- angles[i]
      theta_sum <- theta_sum + theta
    }
    B <- theta_sum / L
    B
  }
}
