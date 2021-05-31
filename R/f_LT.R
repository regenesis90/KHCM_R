#' Left turn correction factor(f_LT)
#'
#' This function follows <Formula 8-27>, <Formula 8-28>, <Formula 8-30> in KHCM(2013)
#' @param lane_type *Categorical* Choose from one : \code{'dedicated'}, \code{'practical_only'}, \code{'public'}
#' @param E_L
#' @param P_L
#' @param P_LT
#' @keywords
#' @export f_LT Left turn correction factor
#' @examples
f_LT <- function(lane_type = NULL, E_L = NULL, P_L = NULL, P_LT = NULL){
  if (lane_type == 'dedicated'){
    f <- 1/E_L
    }
  if (lane_type == 'practical_only'){
    f <- 1/(1 + P_L * (E_L - 1))
    }
  if (lane_type == 'public'){
    f <- 1/(1 + P_LT * (E_L - 1))
    }
  f
}
