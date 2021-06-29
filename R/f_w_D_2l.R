#'  The One Side Lane Width and Lateral Clearance Factor in 2-lane Road(in TDR)
#'
#' This function calculates the one side lane width and lateral clearance factor in 2-lane road(TDR, %) in given conditions.
#'     It follows <Table 7-10> in KHCM(2013), p.181.
#' @param side_clearance The side clearance of lane(m). From the center line of the center line to the pavement edge of the lane. The Average of left side clearance and right lateral clearance.
#' @param lane_width The width of each lane(m). It must be more than 2.75
#' @export f_w_D_2l The One Side Lane Width and Lateral Clearance Factor(TDR, %)
#' @examples
#' f_w_D_2l(side_clearance = 2.3, lane_width = 3.19)
#' f_w_D_2l(2.99, 2.83)
f_w_D_2l <- function(side_clearance = NULL, lane_width = NULL){
  if (side_clearance >= 1.5){
    if (lane_width >= 3.50){f <- 0}
    else if (lane_width >= 3.25 & lane_width < 3.50){f <- 3}
    else if (lane_width >= 3.00 & lane_width < 3.25){f <- 6}
    else if (lane_width >= 2.75 & lane_width < 3.00){f <- 9}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  else if (side_clearance >= 1.0 & side_clearance < 1.5){
    if (lane_width >= 3.50){f <- 3}
    else if (lane_width >= 3.25 & lane_width < 3.50){f <- 6}
    else if (lane_width >= 3.00 & lane_width < 3.25){f <- 9}
    else if (lane_width >= 2.75 & lane_width < 3.00){f <- 12}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  else if (side_clearance >= 0.5 & side_clearance < 1.0){
    if (lane_width >= 3.50){f <- 6}
    else if (lane_width >= 3.25 & lane_width < 3.50){f <- 9}
    else if (lane_width >= 3.00 & lane_width < 3.25){f <- 12}
    else if (lane_width >= 2.75 & lane_width < 3.00){f <- 15}
    else {f <- 'Error : [lane_width] must be >= 2.75(m). Please check that.'}
  }
  else {f <- 'Error : [side_clearance] must be >= 0.5(m). Please check that.'}
  f
}
