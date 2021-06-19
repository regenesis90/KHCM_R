#' Level of Service(LOS) in Expressway-Ramp Junction
#'
#' This function decides Level of Service(LOS) in the expressway-ramp junction.
#'     It follows <Table 4-3> in KHCM(2013), p.86.
#' @param V The traffic volume(pcph)
#' @param capacity Capacity(pcph)
#' @param density The density of the road(pcpkmpl)
#' @export LOS_expwy_rpjt
#' @examples
#' LOS_expwy_rpjt(density = 24.32, V = 3921, capacity = 4400)
#' LOS_expwy_rpjt(density = 32.32, V = 4821, capacity = 2000)
LOS_expwy_rpjt <- function(density = NULL, V = NULL, capacity = NULL){
  if (V > capacity){LOS <- 'F'}
  else if (V <= capacity){
    if (density >= 0 & density <= 6){LOS <-  'A'}
    else if (density > 6 & density <= 12){LOS <- 'B'}
    else if (density > 12 & density <= 17){LOS <- 'C'}
    else if (density > 17 & density <= 22){LOS <- 'D'}
    else if (density > 22){LOS <- 'E'}
    else {LOS <- 'Error : [density] must be positive(pcpkmpl). Please check that.'}
  }
  else {LOS <- 'Error : [V], [capacity] must be positive(pcph). Please check that.'}
  LOS
}
