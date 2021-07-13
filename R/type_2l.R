#' 2-lane road type determination
#'
#' This function identifies the type of 2-lane road.
#' @param flow Characteristic of traffic flow. Choose one from: \code{'interrupted'}, \code{'uninterrupted'}
#' @param si_density Signal Intersection Density(ea/km)
#' @keywords 2-lane road
#' @seealso \code{\link{capa_2l}}
#' @export type_2l \code{'type1'}, \code{'type2'}, \code{'type3'}
#' @details - type1 : High standard roads such as highways. It is a two-lane road with continuous flow characteristics.
#'          - Type2: A general road with more than 0.5 signal intersections/km and an interval of 2 km or more between signal intersections. Roadside development is insufficient, and continuous and intermittent flow characteristics are mixed.
#'          - Type3: General roads with less than 0.5 signal intersections/km and less than 2 km between signal intersections. The area around the road has been developed and has intermittent flow characteristics. (* Analysis by the method of urban and suburban arterial roads)
#' @examples
#' type_2l(flow = 'uninterrupted')
#' type_2l(flow = 'interrupted', si_density = 0.3)
type_2l <- function(flow = NULL, si_density = NULL){
  if (flow == 'uninterrupted'){res <- 'type1'}
  else if (flow == 'interrupted'){
    if (si_density <= 0.5 & si_density >= 0){res <- 'type2'}
    else if (si_density > 0.5){res <- 'type3'}
    else {res <- 'Error : [si_density] must be positive(ea/km). Please check that.'}
  }
  else {res <- 'Error : [flow] must be one of [uninterrupted], [interrupted]. Please check that.'}
  res
}
