#' Hillness in Multi-lane Road(H, m/km)
#'
#' This function calculates difference in height in multi-lane road. It follows <Formula 6-1> in KHCM(2013).
#' @param diff_in_height *Series* Series of each difference in height on direction. \code{c(height1, height2, height3, ...)} (m)
#' @param L *Numeric* Length of the route(km). Generally, \code{L = 3}
#' @keywords Hillness Multilane road
#' @export Hillness The sum of difference in height divided by L
#' @examples
#' H_multilane_road(diff_in_height = c(3, 2, 8, 3.2, 5), L = 3)
#' H_multilane_road(c(2.1, 5.3, 4.48, 3.32, 2.1), 3.4)
H_multilane_road <- function(diff_in_height = NULL, L = NULL){
  if (L > 0 & is.null(diff_in_height) == FALSE){
    dff_sum <- 0
    for (i in 1:length(diff_in_height)){
      dff <- diff_in_height[i]
      dff_sum <- dff_sum + dff
    }
    H <- dff_sum / L
    H
  }
}
