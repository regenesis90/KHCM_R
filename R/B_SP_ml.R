#' Maximum Travel Speed Under Basic Conditions in Multi-lane Road
#'
#' Average maximum travel speed that a passenger car can achieve when the traffic volume is less than 500 vphpl on a multi-lane road in a straight section that is not affected by traffic lights.
#'    Depending on the type of multi-lane road.
#'    - type 1 : 97kph, 87kph
#'    - type 2 : 87kph, 70kph
#' @param design_speed Design speed(kph). Choose one from : \code{100}, \code{90}, \code{80}, \code{80}, \code{70}
#' @keywords multilane road type multi-lane average maximum speed
#' @seealso \code{\link{type_ml}}
#' @export type_ml \code{'type1'}, \code{'type2'}
#' @details - type 1 : Roads with the strongest continuous flow characteristics.
#'              The design speed is 90~100kph, and the maximum travel speed under the basic conditions is 87kph and 97kph.
#'              This applies to roads that do not have signal intersections, are multi-level intersections, and have access roads and sidewalks.
#'          - type 2 : A road in which continuous flow characteristics are somewhat predominant. The design speed is 70~80kph, and the maximum travel speed under basic conditions is 87kph and 70kph.
#'              In terms of ancillary facilities, the density of traffic lights is less than 0.5/km, and it refers to a road in a partially three-dimensional state.
#' @examples
#' B_SP_ml(design_speed = 100)
#' B_SP_ml(80)
B_SP_ml <- function(design_speed = NULL){
  if (design_speed == 100){97}
  else if (design_speed == 90){87}
  else if (design_speed == 80){87}
  else if (design_speed == 70){70}
  else {'Error : [design_speed] must be one of 100, 90, 80, 70. Please check that.'}
}
