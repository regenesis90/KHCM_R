#' Design Hour Volume(DHV)
#'
#' This function calculate Design Hour Volume(DHV).
#' @param AADT *Numeric* Average Annual Daily Traffic(pc/day, vph)
#' @param region *Categorical* Choose one from : \code{'city'}, \code{'rural'}, \code{'tourist_area'}
#' @param road *Categorical* Choose one from : \code{'general'}, \code{'highway'}
#' @param lane *Numeric* The number of lane(round-trip). It should \code{2} or \code{4} or more.
#' @keywords DHV AADT Design Hour Factor
#' @export DHV \code{AADT * K}. It means Design Hour Volume(DHV, pc/h/bidirectional)
#' @examples
#' DHV(AADT = 2000, region = 'tourist_area', road = 'highway', lane = 8)
#' DHV(AADT = 1500, region = 'city', road = 'general', lane = 2)
#' DHV(3000, 'city', 'general', 8)
DHV <- function(AADT = NULL, region = NULL, road = NULL, lane = NULL){
  if (AADT >= 0 & lane >=2){
    if(region =='city'){
      if(road == 'general'){
        if(lane == 2){K <- 0.12}
        if(lane >= 4){K <- 0.1}
      }
      if(road == 'highway'){K <- 0.1}
    }
    if(region == 'rural'){
      if(road == 'general'){
        if(lane == 2){K <- 0.16}
        if(lane >= 4){K <- 0.12}
      }
      if(road == 'highway'){K <- 0.14}
    }
    if(region == 'tourist_area'){
      if(road == 'general'){
        if(lane == 2){K <- 0.23}
        if(lane >= 4){K <- 0.14}
      }
      if(road == 'highway'){K <- 0.14}
    }
    AADT*K
  }
}
