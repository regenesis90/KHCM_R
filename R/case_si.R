#' Classification by Signal Operation and Left Turn Lane at Signalized Intersection
#'
#' Classification by signal operation and left turn lane at signalized intersections.
#'    At signal intersections, the capacity analysis method differs depending on the signal operation method
#'    and the presence or absence of an exclusive left-turn lane. Therefore, the results divided by case are as follows.
#'    Even if the left lane is an exclusive left-turn lane, if the right lane is shared, both lanes are considered to be shared.
#'    It follows <Table 8-4>, <Figure 8-2> in KHCM(2013), p.222-223.
#' @param type_left Type of left lane. Choose one from: \code{'dedicated'}, \code{'public'}
#' @param N_left Number of left lane. Choose one from: \code{1}, \code{2}
#' @param si_left Method of left signal operation. Choose one from: \code{'two-way-protection'}, \code{'direct-left'}, \code{'unprotected'}
#' @keywords case classification sigmalized intersection
#' @export case_si \code{'case1'}, \code{'case2'}, \code{'case3'}, \code{'case4'}, \code{'case5'}, \code{'case6'}
#' @details
#'     - case1 : There is one dedicated left turn lane, and both protected left turn signal or simultaneous direct and left signal
#'     - case2: There are two dedicated left turn lanes, and the signal operation is the same as in CASE 1
#'     - case3 : The exclusive left turn lane is 1 and unprotected left turn signal
#'     - case4 : There is one common straight and left turn lane, simultaneous direct and left signal
#'     - case5 : The leftmost lane is a dedicated left turn, and the next lane is a common lane for straight and left turns, and the signal is Same as CASE 4⑥
#'     - case6 : There is one common lane for straight and left turns, and there is an unprotected left turn signal.
#' @example
#' case_si(type_left = 'public', N_left = 1, si_left = 'unprotected')
#' case_si(type_left = 'dedicated', N_left = 2, si_left = 'direct-left')
case_si <- function(type_left = NULL, N_left = NULL, si_left = NULL){
  if (type_left == 'dedicated'){
    if (N_left == 1){
      if (si_left == 'two-way-protection'){case <- 'case1'}
      else if (si_left == 'direct-left'){case <- 'case1'}
      else if (si_left == 'unprotected'){case <- 'case3'}
      else {case <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
    }
    else if (N_left == 2){
      if (si_left == 'two-way-protection'){case <- 'case2'}
      else if (si_left == 'direct-left'){case <- 'case2'}
      else {case <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
    }
    else {case <- 'Error : [N_left] must be one of 1, 2. Please check that.'}
  }
  else if (type_left == 'public'){
    if (N_left == 1){
      if (si_left == 'direct-left'){case <- 'case4'}
      else if (si_left == 'unprotected'){case <- 'case6'}
      else {case <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
    }
    else if (N_left == 2){
      if (si_left == 'direct-left'){case <- 'case5'}
      else {case <- 'Error : [si_left] must be one of [two-way-protection], [direct-left], [unprotected]. Please check that.'}
    }
    else {case <- 'Error : [N_left] must be one of 1, 2. Please check that.'}
  }
  else {case <- 'Error : [type_left] must be one of [dedicated], [public]. Please check that.'}
  case
}
