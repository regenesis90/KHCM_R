#' Saturated traffic flow rate correction factor according to the width of the bicycle road
#'
#' It follows <Table 15-1> in KHCM(2013), p.648
#' @param width bicycle road width(m)
#' @keywords
#' @export f_w_bike
#' @examples
#' f_w_bike(width = 1.83)
#' f_w_bike(2)
f_w_bike <- function(width = NULL){
  if (width == 1.0){f <- 0.80}
  if (width > 1.0 & width < 1.5){f <- 0.80 + ((0.87 - 0.80)/(1.5 - 1.0)) * (width - 1.0)}
  if (width == 1.5){f <- 0.87}
  if (width > 1.5 & width < 2.0){f <- 0.87 + ((0.92 - 0.87)/(2.0 - 1.5)) * (width - 1.5)}
  if (width == 2.0){f <- 0.92}
  if (width > 2.0 & width < 2.5){f <- 0.92 + ((1.0 - 0.92)/(2.5 - 2.0)) * (width - 2.0)}
  if (width >= 2.5){f <- 1.0}
  f
}
