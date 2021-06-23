#' Hilliness of Multi-lane Road
#'
#' This function calculates difference in height in multi-lane road.
#'     The degree to which the vertical curve is inclined.
#'     The degree of curvature of the vertical alignment is an index indicating the degree of ups and downs of the road along the profile.
#'     It is a scale expressed as the sum of the height difference of the upward slope section per unit length.
#'     It is defined as the elevation difference (h) with respect to the section length (L) of the route.
#'     It follows <Formula 6-1> in KHCM(2013).
#' @param dif_h Series of each difference in height on direction. \code{c(height1, height2, height3, ...)} (m)
#' @param L Length of the route(km). Generally, \code{L = 3}
#' @keywords Hillness Multilane road
#' @export Hillness The sum of difference in height divided by L
#' @examples
#' H_ml(dif_h = c(3, 2, 8, 3.2, 5), L = 3)
#' H_ml(c(2.1, 5.3, 4.48, 3.32, 2.1), 3.4)
H_ml <- function(dif_h = NULL, L = NULL){
  if (L > 0 & is.null(dif_h) == FALSE){
    dff_sum <- 0
    for (i in 1:length(dif_h)){
      dff <- dif_h[i]
      dff_sum <- dff_sum + dff
    }
    H <- dff_sum / L
  }
  else {H <- 'Error : [L], [dif_h] must be positive values. Please check that.'}
  H
}
