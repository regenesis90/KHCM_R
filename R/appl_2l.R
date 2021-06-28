#' Determination of whether to analyze LOS of the 2-lane Road
#'
#' This function Determine whether to proceed with the service level analysis procedure
#'     for 2-lane road based on the capacity.
#'     It follows some definitions in KHCM(2013), p.174.
#' @param v_bi Bidirectional traffic volume of 2-lane road(pcph).
#' @param v_one One-way traffic volume of 2-lane road(pcphpl).
#' @export appl_2l \code{'Possible'}, \code{'Impossible'}
#' @keywords 2-lane road capacity analyze determination
#' @seealso \code{\link{capa_2l}}
#' @examples
#' appl_2l(v_bi = 3283, v_one = 1653)
#' appl_2l(v_bi = 2938, v_one = 1531)
appl_2l <- function(v_bi = NULL, v_one = NULL){
  capa_bi <- capa_2l(direction = 'bidirectional')
  capa_one <- capa_2l(direction = 'one_way')
  if (v_bi >= 0 & v_one >= 0){
    if (v_bi <= capa_bi & v_one <= capa_one){res <- 'Possible'}
    else {res <- 'Impossible'}
  }
  else {res <- 'Error : [v_bi], [v_one] must be positive. Please check that.'}
}
