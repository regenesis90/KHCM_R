#'  The One Side Lane Width and Lateral Clearance Speed Correction Factor : Reduced Maximum Travel Speed in Multi-lane Road(F_wc, kph)
#'
#' This function calculates the one side lane width and lateral clearance speed correction factor(F_wc) in given conditions. It follows <Table 6-2> in KHCM(2013).
#' @param lateral_clearance *Numeric* The lateral clearance of lane. From the center line of the center line to the pavement edge of the lane. The Average of left lateral clearance and right lateral clearance.
#' @param lane_width *Categorical* The width of each lane(m). Choose one from: \code{3.5}, \code{3.25}, \code{3}
#' @export F_wc Reduced Maximum Travel Speed(kph)
#' @examples
#' F_wc(lateral_clearance = 2.2, lane_width = 3.5)
#' F_wc(1.9, 3.25)
F_wc <- function(lateral_clearance = NULL, lane_width = NULL){
  if (lateral_clearance >= 0 & lane_width > 0){
    if (lane_width == 3.5){result <- (lateral_clearance - 1.5)*(-2)}
    if (lane_width == 3.25){result <- (lateral_clearance - 1)*(-2) + 1}
    if (lane_width == 3.0){result <- (lateral_clearance - 0.5)*(-2) + 3}
  }
  result
}
