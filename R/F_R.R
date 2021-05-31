#' Right Turn Traffic Volume Correction Factor(F_R)
#'
#' This function follows <Table 8-6> in KHCM(2013)
#' @param intersection_prongs *Categorical* Choose one from : \code{3}, \code{4}
#' @param dowry *Categorical* \code{'yes'}, \code{'no'}
#' @keywords Right Turn Traffic Volume Correction Factor Coefficient
#' @export F_R Right turn traffic volume correction factor(V+R/V_R0)
#' @examples
#' F_R(3)
#' F_R(4, 'no')
F_R <- function(intersection_prongs = NULL, dowry = NULL){
  if (intersection_prongs == 3){f <- 0.5}
  if (intersection_prongs == 4){
    if (dowry == 'yes'){f <- 0.4}
    if (dowry == 'no'){f <- 0.5}
  }
  f
}
