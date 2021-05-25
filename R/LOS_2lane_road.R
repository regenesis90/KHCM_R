#' Level of Service(LOS) in 2-lane Road
#'
#' This function decides Level of Service(LOS). It follows <Table 7-2>
#' @param type *Categorical* Type of 2-lane road. Choose one from : \code{'type_1'}, \code{'type_2'}
#' @param max_speed *Categorical* It means maximum speed(kph).
#' @param speed *Numeric* It means speed(kph)
#' @param V *Numeric* Traffic Volume(pcph)
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS_2lane_road Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
LOS_2lane_road <- function(type = NULL, max_speed = NULL){
}
