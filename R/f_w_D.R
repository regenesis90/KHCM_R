#'  The One Side Lane Width and Lateral Clearance Factor in 2-lane Road(f_w_D, %)
#'
#' This function calculates the one side lane width and lateral clearance factor in 2-lane road(f_w_D) in given conditions.
#' @param lateral_clearance *Numeric* The lateral clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left lateral clearance and right lateral clearance.
#' @param lane_width *Numeric* The width of each lane(m). It must be more than 2.75
#' @export f_w_D The One Side Lane Width and Lateral Clearance Factor(f_w_D)
#' @examples
#' f_w_D(lateral_clearance = 2.3, lane_width = 3.19)
#' f_w_D(2.99, 2.83)
f_w_D <- function(lateral_clearance = NULL, lane_width = NULL){
  if (lateral_clearance >= 0.5 & lane_width >= 2.75){
    if (lateral_clearance >= 1.5){
      if (lane_width >= 3.50){f <- 0}
      if (lane_width >= 3.25 & lane_width < 3.50){f <- 3}
      if (lane_width >= 3.00 & lane_width < 3.25){f <- 6}
      if (lane_width >= 2.75 & lane_width < 3.00){f <- 9}
    }
    if (lateral_clearance >= 1.0 & lateral_clearance < 1.5){
      if (lane_width >= 3.50){f <- 3}
      if (lane_width >= 3.25 & lane_width < 3.50){f <- 6}
      if (lane_width >= 3.00 & lane_width < 3.25){f <- 9}
      if (lane_width >= 2.75 & lane_width < 3.00){f <- 12}
    }
    if (lateral_clearance >= 0.5 & lateral_clearance < 1.0){
      if (lane_width >= 3.50){f <- 6}
      if (lane_width >= 3.25 & lane_width < 3.50){f <- 9}
      if (lane_width >= 3.00 & lane_width < 3.25){f <- 12}
      if (lane_width >= 2.75 & lane_width < 3.00){f <- 15}
    }
    f
  }
}
