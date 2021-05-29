#'  The One Side Lane Width and Lateral Clearance Factor in 2-lane Road(f_w_ATS, kph)
#'
#' This function maximum reduced speed. It follows <Table 7-11> in KHCM(2013).
#' @param lateral_clearance *Numeric* The lateral clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left lateral clearance and right lateral clearance.
#' @param lane_width *Numeric* The width of each lane(m). It must be more than 2.75
#' @export f_w_ATS The One Side Lane Width and Lateral Clearance Factor(f_w_D)
#' @examples
#' f_w_ATS(lateral_clearance = 1.0, lane_width = 3.134)
#' f_w_ATS(0.5, 3.6478)
f_w_ATS <- function(lateral_clearance = NULL, lane_width = NULL){
  if (lateral_clearance >= 0.0 & lane_width >= 2.75){
    if (lateral_clearance == 1.5){
      if (lane_width > 3.50){f <- 0}
      if (lane_width == 3.50){f <- 0}
      if (lane_width > 3.25 & lane_width < 3.50){f <- 0 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.25){f <- 1}
      if (lane_width > 3.0 & lane_width < 3.25){f <- 1 - 8 * (lane_width - 3.25)}
      if (lane_width <= 3.0){f <- 3}
    }
    #if (lateral_clearance < 1.5 & lateral_clearance > 1.0){}
    if (lateral_clearance == 1.0){
      if (lane_width >= 3.75){f <- 0}
      if (lane_width > 3.50 & lane_width < 3.75){f <- 1 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.50){f <- 1}
      if (lane_width > 3.25 & lane_width < 3.50){f <- 1 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.25){f <- 2}
      if (lane_width > 3.0 & lane_width < 3.25){f <- 2 - 8 * (lane_width - 3.25)}
      if (lane_width <= 3.0){f <- 4}
    }
    #if (lateral_clearance < 1.0 & lateral_clearance > 0.5){}
    if (lateral_clearance == 0.5){
      if (lane_width >= 4.00){f <- 0}
      if (lane_width > 3.50 & lane_width < 4.00){f <- 2 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.50){f <- 2}
      if (lane_width > 3.25 & lane_width < 3.50){f <- 2 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.25){f <- 3}
      if (lane_width > 3.0 & lane_width < 3.25){f <- 3 - 8 * (lane_width - 3.25)}
      if (lane_width <= 3.0){f <- 5}
    }
    #if (lateral_clearance < 0.5 & lateral_clearance > 0.0){}
    if (lateral_clearance == 0.0){
      if (lane_width >= 4.25){f <- 0}
      if (lane_width > 3.50 & lane_width < 4.25){f <- 3 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.50){f <- 3}
      if (lane_width > 3.25 & lane_width < 3.50){f <- 3 - 4 * (lane_width - 3.50)}
      if (lane_width == 3.25){f <- 4}
      if (lane_width > 3.0 & lane_width < 3.25){f <- 4 - 8 * (lane_width - 3.25)}
      if (lane_width <= 3.0){f <- 6}
    }
  }
  f
}
