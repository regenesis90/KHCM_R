#'  The One Side Lane Width and side Clearance Factor in 2-lane Road(in ATS)
#'
#' This function calculates the one side lane width and side clearance factor in 2-lane road(ATS, kph) in given conditions.
#'     This function maximum reduced speed. It follows <Table 7-11> in KHCM(2013).
#' @param side_clearance The side clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left side clearance and right side clearance.
#' @param lane_width The width of each lane(m). It must be more than 2.75
#' @export f_w_ATS_2l The One Side Lane Width and side Clearance Factor(kph).
#' @examples
#' f_w_ATS_2l(side_clearance = 1.0, lane_width = 3.134)
#' f_w_ATS_2l(0.5, 3.6478)
f_w_ATS_2l <- function(side_clearance = NULL, lane_width = NULL){
  if (side_clearance == 1.5){
    if (lane_width > 3.50){f <- 0}
    else if (lane_width == 3.50){f <- 0}
    else if (lane_width > 3.25 & lane_width < 3.50){f <- 0 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.25){f <- 1}
    else if (lane_width > 3.0 & lane_width < 3.25){f <- 1 - 8 * (lane_width - 3.25)}
    else if (lane_width <= 3.0){f <- 3}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  #if (side_clearance < 1.5 & side_clearance > 1.0){}
  else if (side_clearance == 1.0){
    if (lane_width >= 3.75){f <- 0}
    else if (lane_width > 3.50 & lane_width < 3.75){f <- 1 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.50){f <- 1}
    else if (lane_width > 3.25 & lane_width < 3.50){f <- 1 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.25){f <- 2}
    else if (lane_width > 3.0 & lane_width < 3.25){f <- 2 - 8 * (lane_width - 3.25)}
    else if (lane_width <= 3.0){f <- 4}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  #if (side_clearance < 1.0 & side_clearance > 0.5){}
  else if (side_clearance == 0.5){
    if (lane_width >= 4.00){f <- 0}
    else if (lane_width > 3.50 & lane_width < 4.00){f <- 2 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.50){f <- 2}
    else if (lane_width > 3.25 & lane_width < 3.50){f <- 2 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.25){f <- 3}
    else if (lane_width > 3.0 & lane_width < 3.25){f <- 3 - 8 * (lane_width - 3.25)}
    else if (lane_width <= 3.0){f <- 5}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  #if (side_clearance < 0.5 & side_clearance > 0.0){}
  else if (side_clearance == 0.0){
    if (lane_width >= 4.25){f <- 0}
    else if (lane_width > 3.50 & lane_width < 4.25){f <- 3 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.50){f <- 3}
    else if (lane_width > 3.25 & lane_width < 3.50){f <- 3 - 4 * (lane_width - 3.50)}
    else if (lane_width == 3.25){f <- 4}
    else if (lane_width > 3.0 & lane_width < 3.25){f <- 4 - 8 * (lane_width - 3.25)}
    else if (lane_width <= 3.0){f <- 6}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  else {f <- 'Error : [side_clearance] must be one of 1.5, 1.0, 0.5, 0.0(m). Please check that.'}
}
