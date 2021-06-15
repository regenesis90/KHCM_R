#' Peak Directional Design Hourly Volume(PDDHV)
#'
#' This function calculates Peak Directional Design Hourly Volume(PDDHV, vph) for a given road and traffic condition.
#'     It follows <Formula 2-9> in KHCM(2013), p.31
#' @param DDHV *Numeric* Directional Design Hourly Volume(DDHV, vph). See \code{\link{DDHV}}
#' @param PHF *Numeric* Peak Hour Factor(PHF). See \code{\link{PHF}}
#' @keywords PDDHF DDHV DHV AADT Directional Design Hourly Factor PHF
#' @export PDDHV \code{DDHV * PHF}
#' @examples
#' PDDHV(DDHV = 2132, PHF = 0.8)
#' PDDHV(1829, 0.74)
PDDHV <- function(DDHV = NULL, PHF = NULL){
  if (DDHV >= 0 & PHF > 0){
    if (PHF <= 1){DDHV / PHF}
    else{'Error : PHF must be same or less than 1. Please check PHF value.'}
  }
  else{'Error : DDHV and PHF must be positive. Please check their values.'}
}
