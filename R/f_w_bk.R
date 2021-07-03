#' Saturated Traffic Flow Rate Correction Factor According to the Width of the Bicycle Road
#'
#' It follows <Table 15-1> in KHCM(2013), p.648.
#' @param width Bicycle road width(m)
#' @keywords saturated traffic flow rate correction factor bicycle road
#' @export f_w_bk
#' @examples
#' f_w_bk(width = 1.83)
#' f_w_bk(2)
f_w_bk <- function(width = NULL){
  if (width == 1.0){f <- 0.80}
  else if (width > 1.0 & width < 1.5){f <- 0.80 + ((0.87 - 0.80)/(1.5 - 1.0)) * (width - 1.0)}
  else if (width == 1.5){f <- 0.87}
  else if (width > 1.5 & width < 2.0){f <- 0.87 + ((0.92 - 0.87)/(2.0 - 1.5)) * (width - 1.5)}
  else if (width == 2.0){f <- 0.92}
  else if (width > 2.0 & width < 2.5){f <- 0.92 + ((1.0 - 0.92)/(2.5 - 2.0)) * (width - 2.0)}
  else if (width >= 2.5){f <- 1.0}
  else {f <- 'Error : [width] must be >= 1(m). Please check that.'}
  f
}
