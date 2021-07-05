#' Correction factor for lane width and side clearance width
#'
#' It is a correction coefficient that reflects the effect of lane width and side clearance on traffic flow.
#'     It follows <Table 2-2> in KHCM(2013), p.24-25.
#' @param obs Obstacle. Choose one from :\code{'one_side'}, \code{'both_sides'}
#' @param side_clearance The lateral clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left lateral clearance and right lateral clearance.
#' @param lane The number of expressway lane(Round-trip). It must be 4 or more.
#' @param lane_width The width of each lane(m). It must be equal to or more than 2.75
#' @keywords factor lane width side clearance
#' @details
#'     - **side_clearance** : It refers the distance from the pavement edge of the shoulder lane and median lane to the obstacle recognized by the driver.
#'     - Protective fences and median barriers that do not significantly affect the driver's traffic are not usually considered obstacles.
#'     - It is calculated based on the basic conditions of a road pole of 3.5m or more and a side clearance width of 1.5m or more.
#'     - **obs** :
#'         - \code{'one_side'} : In general, most (expressway) roads with a median in the form of a concrete barrier are applicable.
#'         - \code{'both_sides'} : When there are obstacles on both sides: Applies when a median other than a concrete barrier is installed, or when a median is not installed (center line marked). If the center line is double-tracked or spaced apart, the counter-traffic flow can be considered as an obstacle.
#' @export f_w_expwy_basic
#' @seealso
#' @examples
#' f_w_expwy_basic(obs = 'one_side', side_clearance = 1.4, lane = 8, lane_width = 3.1)
#' f_w_expwy_basic('both_sides', 1, 8, 3)
f_w_expwy_basic <- function(obs = NULL, side_clearance = NULL, lane = NULL, lane_width = NULL){
  if (obs == "one_side"){
    if (side_clearance >= 0 & side_clearance < 0.5){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.90}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.87}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.82}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.73}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.94}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.91}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.85}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.74}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else if (side_clearance >= 0.5 & side_clearance < 1.0){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.97}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.94}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.88}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.79}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.97}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.93}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.87}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else if (side_clearance >= 1.0 & side_clearance < 1.5){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.98}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.95}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.89}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.79}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.98}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.94}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.87}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else if (side_clearance >= 1.5){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 1.00}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.96}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.90}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.80}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 1.00}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.95}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.88}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.77}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else {factor <- 'Error : [side_clearance] must be >= 0. Please check that.'}
  }
  else if (obs == "both_sides"){
    if (side_clearance >= 0 & side_clearance < 0.5){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.81}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.79}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.74}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.66}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.91}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.87}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.81}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.70}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else if (side_clearance >= 0.5 & side_clearance < 1.0){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.94}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.91}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.86}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.96}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.92}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.85}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.75}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else if (side_clearance >= 1.0 & side_clearance < 1.5){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.96}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.93}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.87}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.77}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.97}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.93}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.86}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.76}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else if (side_clearance >= 1.5){
      if (lane == 4){
        if (lane_width >= 3.5){factor <- 0.99}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.96}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.90}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.80}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else if (lane >= 6){
        if (lane_width >= 3.5){factor <- 0.99}
        else if (lane_width >= 3.25 & lane_width < 3.5){factor <- 0.95}
        else if (lane_width >= 3.00 & lane_width < 3.25){factor <- 0.88}
        else if (lane_width >= 2.75 & lane_width < 3.00){factor <- 0.77}
        else {factor <- 'Error : [lane_width] must be eqal to or more than 2.75. Please check that'}
      }
      else {factor <- 'Error : [lane] must be 4 or 6 or more. Please check that.'}
    }
    else {factor <- 'Error : [side_clearance] must be >= 0. Please check that.'}
  }
  else{factor <- 'Error : [obs] must be one of [one_side] and [both_sides]. Please check that.'}
  factor
}
