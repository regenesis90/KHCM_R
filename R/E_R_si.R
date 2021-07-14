#' Forward Conversion Coefficient for Public Right Turns in a Right Turn Lane at Signalized Intersection
#'
#' Forward conversion coefficient for public right turns in a right turn lane at signalized intersection.
#'     This function follows <Formula 8-11>, <Formula 8-12> in KHCM(2013), p.233-234.
#' @param case Case of signalized intersection. Choose one from: \code{'case1'}, \code{'case2'}, \code{'case3'}, \code{'case4'}, \code{'case5'}, \code{'case6'}. See \code{\link{case_si}}
#' @param dowry Choose one from : \code{'yes'}, \code{'no'}
#' @param V_R Corrected Right Turn Traffic (vph). See \code{\link{V_R_si}}
#' @param V_TH straight-through traffic (vph)
#' @param N_ped Number of pedestrians crossing (in both directions, person/hour)
#' @param G_p Pedestrian crossing signal(sec)
#' @param C Signal Cycle(sec)
#' @param L_H Roadside friction (sec) due to entry and exit of the back road, bus stop, and on-street parking, See \code{\link{L_H_si}}
#' @param N Number of lanes that can go straight
#' @keywords conversion coefficient public right turn lane signalized intersection
#' @seealso \code{\link{f_c_si}}, \code{\link{V_R_si}}, \code{\link{L_H_si}}, \code{\link{case_si}}
#' @details
#'     - In case of \code{dowry == 'no'} : Use the stop line for straight and right turns as in a public right-turn lane that has not been bridged, and after turning right, the pedestrian crossing signal at the crosswalk at the intersection temporarily blocks the right turn. The straight-through conversion factor when the straight-line arriving in front of the first right-turning vehicle is released
#'     - In case of \code{dowry == 'yes'} : This is a case in which there is no crosswalk on the crossroads in the public lane that is bridged, or even if there is a crosswalk, it is connected to a traffic island, so that the right-turning vehicle is hardly obstructed by pedestrians after turning right.
#'     - Although it is a rare case, if a crosswalk is encountered after a right turn, even if it is bridged, it is considered not to have been bridged. The straight-line conversion factor is 1.0.
#' @export E_R_si
#' @examples
#' E_R_si(case = 'case1', dowry = 'yes', V_R = 101, V_TH = 300, N_ped = 232, G_p = 80, C = 240, L_H = 34, N = 4)
E_R_si <- function(case = NULL, dowry = NULL, V_R = NULL, V_TH = NULL, N_ped = NULL, G_p = NULL, C = NULL, L_H = NULL, N = NULL){
  if (dowry == 'yes'){e <- 1.16 + L_H /(1.63 * V_R)}
  else if (dowry == 'no'){
    if (case == 'case1' | case == 'case2' | case == 'case3' | case == 'case4' | case == 'case6'){
      N_T <- N
      e <- 1.16 + (2200/V_R) * ((f_c * G_p / C) + (L_H / 3600) - ((1.63 * V_TH)/(C * N_T * V_R)))}
    else if (case == 'case5'){
      N_T <- N - 1
      e <- 1.16 + (2200/V_R) * ((f_c * G_p / C) + (L_H / 3600) - ((1.63 * V_TH)/(C * N_T * V_R)))}
    else {e <- 'Error : [case] must be one of [case1], [case2], [case3], [case4], [case5], [case6]. Please check that.'}
  }
  else {e <- 'Error : [dowry] must be one of [yes], [no]. Please check that.'}
  e
}
