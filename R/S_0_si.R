#' Saturated Traffic Flow Rate Under Basic Conditions in Two-point Crossover Diamond-shaped Interchange
#'
#' Saturated traffic flow rate (pcphgpl, passenger car per hour of green per lane) under basic conditions in signalized intersection. 2200pcphgpl.
#'     The initial number of waiting vehicles for each display of the internal link is measured and used as an input value to calculate the additional loss time for green time.
#'     It follows a definition in KHCM(2013), p.427.
#' @keywords saturated traffic flow rate two-point diamond interchange
#' @details
#'     The basic conditions for a signalized intersection are as follows.
#'     - More than 3m lane width
#'     - no-slope access
#'     - The traffic flow is straight, and all vehicles are
#'     - There is no bus stop or on-street parking facility within 75m upstream of the approach stop line
#'     - No vehicles entering or leaving within 60m upstream of the approach stop line
#' @export S_0_si 2200
#' @example
#' S_0_si()
S_0_si <- function(){
  2200
}
