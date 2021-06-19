#' Level of Service(LOS) in Expressway Weaving Section, Ramp Weave.
#'
#' This function decides Level of Service(LOS) in the expressway eaving section, ramp weave.
#'     It follows <Table 3-1> in KHCM(2013), p.63.
#' @param type Choose one from : \code{'ramp'}, \code{'side_road'}
#' @param density The density of the road(pcpkmpl)
#' @keywords level of service expressway weaving section ramp weave
#' @seealso \code{\link{D_expwy_wv}}, \code{\link{LOS_expwy_wv_fr}}
#' @export LOS_expwy_wv_ramp \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_expwy_wv_ramp(type = 'ramp', density = 29)
#' LOS_expwy_wv_ramp('side_road', 28)
LOS_expwy_wv_ramp <- function(type = NULL, density = NULL){
  if (type == "ramp"){
    if (density >= 0 & density <= 6){LOS <-  'A'}
    else if (density > 6 & density <= 12){LOS <- 'B'}
    else if (density > 12 & density <= 17){LOS <- 'C'}
    else if (density > 17 & density <= 22){LOS <- 'D'}
    else if (density > 22 & density <= 27){LOS <- 'E'}
    else if (density > 27){LOS <- 'F'}
    else {LOS <- 'Error : [density] must be >= 0(pcpkmpl). Please check that.'}
  }
  else if (type == "side_road"){
    if (density >= 0 & density <= 8){LOS <-  'A'}
    else if (density > 8 & density <= 13){LOS <- 'B'}
    else if (density > 13 & density <= 18){LOS <- 'C'}
    else if (density > 18 & density <= 25){LOS <- 'D'}
    else if (density > 25 & density <= 38){LOS <- 'E'}
    else if (density > 38){LOS <- 'F'}
    else {LOS <- 'Error : [density] must be >= 0(pcpkmpl). Please check that.'}
  }
  else {LOS <- 'Error : [type] must be one of [ramp] or [side_road]. Please check that.'}
  LOS
}
