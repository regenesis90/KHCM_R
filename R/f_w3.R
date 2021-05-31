#' Lane width correction factor(f_w3)
#'
#' It follows <Table 8-15> in KHCM(2013)
#' @param lane_width When the lane widths within a lane group are different, the average value of these is used.
#' @export f_w3
#' @examples
#' f_w3(2.89)
#' f_w3(lane_width = 3.23)
f_w3 <- function(lane_width = NULL){
  if (lane_width > 0 & lane_width <= 2.6){f <- 0.88}
  if (lane_width > 2.6 & lane_width <= 2.9){f <- 0.94}
  if (lane_width >= 3.0){f <- 1.00}
  f
}
