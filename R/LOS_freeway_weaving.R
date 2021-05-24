#' Level of Service(LOS) in Freeway Weaving Section
#'
#' This function decides Level of Service(LOS) in the freeway weaving section.
#' @param type *Categorical* Choose one from : \code{'link_link'}, \code{'main_link'}
#' @param density *Numeric* The density of the road(pcpkmpl)
#' @export LOS_freeway_weaving Average speed in freeway weaving section.(pcpkmpl)
#' @examples
#' LOS_freeway_weaving(type = 'link_link', density = 29)
#' LOS_freeway_weaving('main_link', density = 28)
LOS_freeway_weaving <- function(type = NULL, density = NULL){
  if (type == "link_link"){
    if (density > 0 & density <= 6){LOS <-  'A'}
    if (density > 6 & density <= 12){LOS <- 'B'}
    if (density > 12 & density <= 17){LOS <- 'C'}
    if (density > 17 & density <= 22){LOS <- 'D'}
    if (density > 22 & density <= 27){LOS <- 'E'}
    if (density > 27){LOS <- 'F'}
  }
  if (type == "main_link"){
    if (density > 0 & density <= 8){LOS <-  'A'}
    if (density > 8 & density <= 13){LOS <- 'B'}
    if (density > 13 & density <= 18){LOS <- 'C'}
    if (density > 18 & density <= 25){LOS <- 'D'}
    if (density > 25 & density <= 38){LOS <- 'E'}
    if (density > 38){LOS <- 'F'}
  }
  LOS
}
