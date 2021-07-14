#' Straight-through Conversion Factor of Left Turn by U-turn Ratio at Signalized Intersection
#'
#' The straight-through conversion factor for left turns according to U-turn % at signalized intersections.
#'     Indicates the effect of a U-turn.
#'     This function follows <Table 8-10>, <Table 8-11> in KHCM(2013), p.229.
#' @param N_left Number of left-turn lane. \code{1}, \code{2}
#' @param u_ratio U-tern ratio(%)
#' @keywords straight conversion factor left turn u tern ratio signalized intersection
#' @seealso \code{\link{E_L_si}}
#' @details
#'     * The ratio of U-turns in the left-turning lane affects the left-turning saturation flow rate.
#'     * The traffic volume of the U-turn itself is excluded from the analysis because it proceeds at a different signal than other moving flows and it does not affect the signal time.
#'     * Use the ratio of U-turn traffic to total uncorrected left turn and U-turn traffic.
#' @export E_u_si Forward conversion factor of left turn by U-turn %
#' @examples
#' E_u_si(N_left = 1, u_ratio = 32)
#' E_u_si(N_left = 2, u_ratio = 11.4)
E_u_si <- function(N_left = NULL, u_ratio = NULL){
  if (N_left == 1){
    if (u_ratio == 0){e <- 1.00}
    else if (u_ratio > 0 & u_ratio < 10){e <- 1.00 + (0.21/10) * (u_ratio - 0)}
    else if (u_ratio == 10){e <- 1.21}
    else if (u_ratio > 10 & u_ratio < 20){e <- 1.21 + (0.18/10) * (u_ratio - 10)}
    else if (u_ratio == 20){e <- 1.39}
    else if (u_ratio > 20 & u_ratio < 30){e <- 1.39 + (0.25/10) * (u_ratio - 20)}
    else if (u_ratio == 30){e <- 1.64}
    else if (u_ratio > 30 & u_ratio < 40){e <- 1.64 + (0.33/10) * (u_ratio - 30)}
    else if (u_ratio == 40){e <- 1.97}
    else if (u_ratio > 40 & u_ratio < 50){e <- 1.97 + (0.58/10) * (u_ratio - 40)}
    else if (u_ratio == 50){e <- 2.55}
    else if (u_ratio > 50 & u_ratio < 60){e <- 2.55 + (0.70/10) * (u_ratio - 50)}
    else if (u_ratio == 60){e <- 3.25}
    else if (u_ratio > 60 & u_ratio <= 100){e <- 2.55 + (0.70/10) * (u_ratio - 50)}
    else {e <- 'Error : [u_ratio] must be >= 0 and <= 100(%). Please check that.'}
  }
  else if (N_left == 2){
    if (u_ratio == 0){e <- 1.00}
    else if (u_ratio > 0 & u_ratio < 10){e <- 1.00 + (0.17/10) * (u_ratio - 0)}
    else if (u_ratio == 10){e <- 1.17}
    else if (u_ratio > 10 & u_ratio < 20){e <- 1.17 + (0.13/10) * (u_ratio - 10)}
    else if (u_ratio == 20){e <- 1.30}
    else if (u_ratio > 20 & u_ratio < 30){e <- 1.30 + (0.18/10) * (u_ratio - 20)}
    else if (u_ratio == 30){e <- 1.48}
    else if (u_ratio > 30 & u_ratio <= 100){e <- 1.30 + (0.18/10) * (u_ratio - 20)}
    else {e <- 'Error : [u_ratio] must be >= 0 and <= 100(%). Please check that.'}
  }
  else {e <- 'Error : [N_left] must be one of 1, 2. Please check that.'}
  e
}
