#' Multi-lane road type determination
#'
#' Since multi-lane roads have both continuous and intermittent traffic flow characteristics,
#'     a wide range of fluctuations in road traffic characteristics is observed.
#'     In order to analyze the service level of multi-lane roads reasonably and consistently in consideration of this wide range of fluctuations,
#'     facilities are divided into two types.
#'     A multi-lane road with a facility condition with a traffic light density exceeding 0.5 lights/km often has intermittent flow characteristics, so it falls under the type of urban and bridge arterial roads.
#'     The maximum travel speed under the basic conditions refers to the average travel speed observed when the average traffic volume of a section is 500 vphpl or less under the basic conditions of the relevant road type.
#' @param design_speed Design speed(kph). Choose one from : \code{100}, \code{90}, \code{80}, \code{80}, \code{70}
#' @param tl_density Traffic light density(ea/km).
#' @keywords multilane road type multi-lane
#' @seealso \code{\link{B_SP_ml}}
#' @export type_ml \code{'type1'}, \code{'type2'}
#' @details - type 1 : Roads with the strongest continuous flow characteristics.
#'              The design speed is 90~100kph, and the maximum travel speed under the basic conditions is 87kph and 97kph.
#'              This applies to roads that do not have signal intersections, are multi-level intersections, and have access roads and sidewalks.
#'          - type 2 : A road in which continuous flow characteristics are somewhat predominant. The design speed is 70~80kph, and the maximum travel speed under basic conditions is 87kph and 70kph.
#'              In terms of ancillary facilities, the density of traffic lights is less than 0.5/km, and it refers to a road in a partially three-dimensional state.
#' @examples
#' type_ml(design_speed = 100)
#' type_ml(design_speed = 80, tl_density = 0.2)
#' type_ml(tl_density = 0.45)
type_ml <- function(design_speed = NULL, tl_density = NULL){
  if (is.null(design_speed) == FALSE){
    if (design_speed == 100){'type1'}
    else if (design_speed == 90){'type1'}
    else if (design_speed == 80){
      if (tl_density == 0){'type1'}
      else if (tl_density > 0 & tl_density <= 0.5){'type2'}
      else if (tl_density > 0.5){'It is Arterial Road. Not multi-lane.'}
      else {'Error : [tl_density] must be positive(ea/km). Please check that.'}
    }
    else if (design_speed == 70){'type2'}
    else {'Error : [design_speed] must be one of 100, 90, 80, 70. Please check that.'}
  }
  else {
    if (is.null(tl_density) == FALSE){
      if (tl_density == 0){'type1'}
      else if (tl_density > 0 & tl_density <= 0.5){'type2'}
      else if (tl_density > 0.5){'It is Arterial Road. Not multi-lane.'}
      else {'Error : [tl_density] must be positive(ea/km). Please check that.'}
    }
    else {'Error : [design_speed], [tl_density], are empty. Please check that.'
    }
  }
}
