#' Right turn correction factor(f_RT)
#'
#' This function follows <Formula 8-29>, <Formula 8-31> in KHCM(2013)
#' @param lane_type *Categorical* Choose from one : \code{'practical_only'}, \code{'public'}
#' @param E_R
#' @param P_R
#' @param P_RT
#' @keywords
#' @export f_RT Right turn correction factor
#' @examples
f_RT <- function(lane_type = NULL, E_R = NULL, P_R = NULL, P_RT = NULL){
  if (lane_type == 'practical_only'){
    f <- 1/(1 + P_R * (E_R - 1))
  }
  if (lane_type == 'public'){
    f <- 1/(1 + P_T * (E_R - 1))
  }
  f
}
