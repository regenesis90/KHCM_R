#'  The One Side Lane Width and Lateral Clearance Factor(f_w)
#'
#' This function calculates the one side lane width and lateral clearance factor(f_w) in given conditions.
#' @param obstacle *Categorical* Choose one from :\code{'one_side'}, \code{'both_sides'}
#' @param lateral_clearance *Numeric* The lateral clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left lateral clearance and right lateral clearance.
#' @param lane *Numeric* The number of freeway lane(Round-trip lane). It must be 4 or more.
#' @param lane_width *Numeric* The width of each lane(m). It must be more than 2.75
#' @export f_w The One Side Lane Width and Lateral Clearance Factor(f_w)
#' @examples
#' f_w(obstacle = 'one_side', lateral_clearance = 1.4, lane = 8, lane_width = 3.1)
#' f_w(obstacle = 'both_sides', lateral_clearance = 1, lane = 8, lane_width = 3)
f_w <- function(obstacle = NULL, lateral_clearance = NULL, lane = NULL, lane_width = NULL){
  if (lateral_clearance >=0 & lane >= 4 & lane_width >=2.75){
    if (obstacle == "one_side"){
      if (lateral_clearance >= 0 & lateral_clearance < 0.5){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.90}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.87}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.82}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.73}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.94}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.91}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.85}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.74}
        }
      }
      if (lateral_clearance >= 0.5 & lateral_clearance < 1.0){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.97}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.94}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.88}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.79}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.97}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.93}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.87}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        }
      }
      if (lateral_clearance >= 1.0 & lateral_clearance < 1.5){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.98}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.95}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.89}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.79}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.98}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.94}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.87}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        }
      }
      if (lateral_clearance >= 1.5){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 1.00}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.96}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.90}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.80}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 1.00}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.95}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.88}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.77}
        }
      }
    }
    if (obstacle == "both_sides"){
      if (lateral_clearance >= 0 & lateral_clearance < 0.5){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.81}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.79}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.74}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.66}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.91}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.87}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.81}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.70}
        }
      }
      if (lateral_clearance >= 0.5 & lateral_clearance < 1.0){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.94}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.91}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.86}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.96}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.92}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.85}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.75}
        }
      }
      if (lateral_clearance >= 1.0 & lateral_clearance < 1.5){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.96}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.93}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.87}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.77}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.97}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.93}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.86}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        }
      }
      if (lateral_clearance >= 1.5){
        if (lane == 4){
          if (lane_width >= 3.5){factor <- 0.99}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.96}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.90}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.80}
        }
        if (lane >= 6){
          if (lane_width >= 3.5){factor <- 0.99}
          if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.95}
          if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.88}
          if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.77}
        }
      }
    }
    factor
  }
}
