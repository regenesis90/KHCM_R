#'  The One Side Lane Width and Side Clearance Speed Correction Factor : Reduced Maximum Travel Speed in Multi-lane Road
#'
#' This function calculates the one side lane width and side clearance speed correction factor in given conditions(kph).
#'     It follows <Table 6-2> in KHCM(2013), p.139.
#' @param side_clearance The side clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left side clearance and right side clearance.
#' @param lane_width The width of each lane(m). Choose one from: \code{3.5}, \code{3.25}, \code{3}
#' @export F_wc_ml Reduced Maximum Travel Speed(kph)
#' @examples
#' F_wc_ml(side_clearance = 1.2, lane_width = 3.0)
#' F_wc_ml(1.5, 3.5)
F_wc_ml <- function(side_clearance = NULL, lane_width = NULL){
  if (side_clearance >= 0 & lane_width >= 3.0){
    if (lane_width == 3.5){result <- (side_clearance - 1.5)*(-2)}
    else if (lane_width == 3.25){result <- (side_clearance - 1)*(-2) + 1}
    else if (lane_width == 3.0){result <- (side_clearance - 0.5)*(-2) + 3}
    else {'Error : [lane_width] must be one of 3.5, 3.25, 3.0. Please check that. '}
  }
  else {'Error : [side_clearance] must be >= 0, [lane_width] must be one of 3.5, 3.25, 3.0. Please check that.'}
  result
}
