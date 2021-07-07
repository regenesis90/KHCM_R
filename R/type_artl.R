#' Arterial Road Type Determination
#'
#' Types of arterial roads according to road classification and road conditions.
#'     The function and design level of arterial roads are classified into 'high standard', 'medium standard',
#'     and 'low standard', and road conditions are divided into 'good' and 'normal'.
#' @param standard Standard of arterial road. Choose one from : \code{'high'}, \code{'medium'}, \code{'low'}
#' @param condition Condition of arterial road. Choose one from : \code{'good'}, \code{'normal'}
#' @keywords arterial road type
#' @seealso \code{\link{LOS_artl}}
#' @export type_artl \code{'type1'}, \code{'type2'}, \code{'type3'}
#' @details
#'     - \code{standard == 'high'} : An arterial road that connects highways or national roads that connect to urban areas.
#'     Mobility is more important than accessibility, and it mainly handles long-distance transit.
#'     It occurs mainly in suburban areas rather than urban areas,
#'     and the form of land use around roads is mainly low-density residential areas, green areas or farmland.
#'     In general, there are no more than 2 signal intersections per km, and the number of bus stops is no more than 2 per km.
#'     The free speed also shows a high value of 75 kph or more.
#'     - \code{standard == 'low'} : Arterial roads mainly connected to collection roads.
#'     It mainly handles traffic in urban areas.
#'     In general, land use around roads has the characteristics of typical high-density business and commercial areas with high-rise buildings and large-scale shopping malls.
#'     In terms of design, the free speed is less than 65 kph,
#'     the number of signal intersections per km is 2 or more, and the number of bus stops per km is 2 or more.
#'     - \code{standard == 'medium'} : It is an intermediate type between high standard and low standard arterial roads.
#'     In terms of land use around the road, there is a mixture of residential areas,
#'     including business buildings, shopping malls, schools, hospitals, gas stations, parks, factories, and large-scale apartment complexes, which are smaller than the city center.
#'     The number of signal intersections and bus stops per km is 1-3.
#'     - \code{condition == 'good'} : Low standard and medium standard are the condition that the link has at least 4 lanes one way and there are at least 2 lanes going straight through the intersection approach.
#'     High standard is a good condition when the link lane is three or more lanes one way.
#'     - \code{condition == 'normal} : Low standard and medium standard are roads with two or three lanes one way.
#'     The high standard corresponds to the case where the link lane is two lanes one way.
#' @examples
#' type_artl(standard = 'high', condition = 'good')
type_artl <- function(standard = NULL, condition = NULL){
  if (standard == 'high'){
    if (condition == 'good'){t <- 'type1'}
    else if (condition == 'normal'){t <- 'type1'}
    else {t <- 'Error : [condition] must be one of [good], [normal]. Please check that.'}
  }
  else if (standard == 'medium'){
    if (condition == 'good'){t <- 'type1'}
    else if (condition == 'normal'){t <- 'type2'}
    else {t <- 'Error : [condition] must be one of [good], [normal]. Please check that.'}
  }
  else if (standard == 'low'){
    if (condition == 'good'){t <- 'type2'}
    else if (condition == 'normal'){t <- 'type3'}
    else {t <- 'Error : [condition] must be one of [good], [normal]. Please check that.'}
  }
  else {t <- 'Error : [standard] must be one of [high], [medium], [low]. Please check that.'}
  t
}
