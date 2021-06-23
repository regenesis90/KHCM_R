#' Bendiness of Multi-lane Road
#'
#' This function calculates horizontal curve curvature in multi-lane road(degree/km).
#'     A scale expressing the degree of bending of the plane curve as the sum of the piers of the plane curved part per unit length.
#'     It is defined as the degree of change of the pier (theta) with respect to the unit section length (L).
#'     It follows <Formula 6-1> in KHCM(2013), p.140.
#' @param angles Series of each direct angle. \code{c(theta1, theta2, theta3, ...)} (degree)
#' @param L Length of the route(km). Generally, \code{L = 3}
#' @keywords bendiness Multilane road
#' @export Bendiness The sum of angles divided by L
#' @examples
#' B_ml(angles = c(30, 20, 30, 40, 50), L = 3)
#' B_ml(c(10, 8, 12, 14), 5)
B_ml <- function(angles = NULL, L = NULL){
  if (L > 0 & is.null(angles) == FALSE){
    theta_sum <- 0
    for (i in 1:length(angles)){
      theta <- angles[i]
      theta_sum <- theta_sum + theta
    }
    B <- theta_sum / L
  }
  else {B <- 'Error : [L], [angles] must be positive. Please check that.'}
  B
}
