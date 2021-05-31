#' Forward conversion factor of left turn by U-turn %(E_u)
#'
#' This function follows <Table 8-10>, <Table 8-11> in KHCM(2013)
#' @param N_left *Categorical* Number of left-turn lane. \code{1}, \code{2}
#' @param u_tern_ratio *Numeric* U-tern ratio(%)
#' @keywords
#' @export E_u Forward conversion factor of left turn by U-turn %
#' @examples
#' E_u(1, 32)
#' E_u(2, 42.5)
E_u <- function(N_left = NULL, u_tern_ratio = NULL){
  if (N_left == 1){
    if (u_tern_ratio == 0){e <- 1.00}
    if (u_tern_ratio > 0 & u_tern_ratio < 10){e <- 1.00 + (0.21/10) * (u_tern_ratio - 0)}
    if (u_tern_ratio == 10){e <- 1.21}
    if (u_tern_ratio > 10 & u_tern_ratio < 20){e <- 1.21 + (0.18/10) * (u_tern_ratio - 10)}
    if (u_tern_ratio == 20){e <- 1.39}
    if (u_tern_ratio > 20 & u_tern_ratio < 30){e <- 1.39 + (0.25/10) * (u_tern_ratio - 20)}
    if (u_tern_ratio == 30){e <- 1.64}
    if (u_tern_ratio > 30 & u_tern_ratio < 40){e <- 1.64 + (0.33/10) * (u_tern_ratio - 30)}
    if (u_tern_ratio == 40){e <- 1.97}
    if (u_tern_ratio > 40 & u_tern_ratio < 50){e <- 1.97 + (0.58/10) * (u_tern_ratio - 40)}
    if (u_tern_ratio == 50){e <- 2.55}
    if (u_tern_ratio > 50 & u_tern_ratio < 60){e <- 2.55 + (0.70/10) * (u_tern_ratio - 50)}
    if (u_tern_ratio == 60){e <- 3.25}
    if (u_tern_ratio > 60 & u_tern_ratio <= 100){e <- 2.55 + (0.70/10) * (u_tern_ratio - 50)}
  }
  if (N_left == 2){
    if (u_tern_ratio == 0){e <- 1.00}
    if (u_tern_ratio > 0 & u_tern_ratio < 10){e <- 1.00 + (0.17/10) * (u_tern_ratio - 0)}
    if (u_tern_ratio == 10){e <- 1.17}
    if (u_tern_ratio > 10 & u_tern_ratio < 20){e <- 1.17 + (0.13/10) * (u_tern_ratio - 10)}
    if (u_tern_ratio == 20){e <- 1.30}
    if (u_tern_ratio > 20 & u_tern_ratio < 30){e <- 1.30 + (0.18/10) * (u_tern_ratio - 20)}
    if (u_tern_ratio == 30){e <- 1.48}
    if (u_tern_ratio > 30 & u_tern_ratio <= 100){e <- 1.30 + (0.18/10) * (u_tern_ratio - 20)}
  }
  e
}
