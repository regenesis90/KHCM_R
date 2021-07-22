#' Classification of Signalized Intersection with Exclusive Central Bus Lanes according to Left Turn Signal Operation
#'
#' Classification of signal intersections with exclusive central bus lanes according to left turn signal operation
#'    It follows <Table 8-18>, <Figure 8-23> in KHCM(2013), p.297-298.
#' @param dedicated_left_cbl Is there a dedicated left turn lane on the exclusive central bus lane? Choose one from: \code{'yes'}, \code{'no'}
#' @param dedicated_left_gn Is there a dedicated left turn lane on general lane? Choose one from: \code{'yes'}, \code{'no'}
#' @param si_left Method of left signal operation. Choose one from: \code{'two-way-protection'}, \code{'direct-left'}, \code{'no-left-turn'}
#' @keywords case classification signalized intersection
#' @export case_si_cbl \code{'case1'}, \code{'case2'}, \code{'case3'}, \code{'case4'}
#' @details
#'     - \code{'case1'} : Dedicated left turn lane is one each for exclusive use of median bus and one for general lane, and there is a left turn signal for both sides.
#'     - \code{'case2'} : There is a dedicated left-turn lane only in the general lane, and the signal is a left-turn signal that protects both sides.
#'     - \code{'case3'} : There is only one dedicated left turn lane in the exclusive median bus lane, and the signal is a two-way protection left turn signal or a direct-left simultaneous signal.
#'     - \code{'case4'} : There is no left turn lane, and only when going straight
#' @example
#' case_si_cbl(dedicated_left_cbl = 'yes', dedicated_left_gn = 'yes', si_left = 'two-way-protection')
#' case_si_cbl(dedicated_left_cbl = 'yes', dedicated_left_gn = 'no', si_left = 'two-way-protection')
#' case_si_cbl(dedicated_left_cbl = 'no', dedicated_left_gn = 'yes', si_left = 'two-way-protection')
#' case_si_cbl(dedicated_left_cbl = 'no', dedicated_left_gn = 'no', si_left = 'no-left-turn')
case_si_cbl <- function(dedicated_left_cbl = NULL, dedicated_left_gn = NULL, si_left = NULL){
  if (dedicated_left_cbl == 'yes'){
    if (dedicated_left_gn == 'yes'){
      if (si_left == 'two-way-protection'){case <- 'case1'}
      else {case <- 'Error : When [dedicated_left_cbl] == [yes] and [dedicated_left_gn] == [yes], [si_left] == [two-way-protection] is only available.'}
    }
    else if (dedicated_left_gn == 'no'){
      if (si_left == 'direct-left' | si_left == 'two-way-protection'){case <- 'case3'}
      else {case <- 'Error : When [dedicated_left_cbl] == [yes] and [dedicated_left_gn] == [no], [si_left] == [two-way-protection] or [si_left] == [direct-left] are just available.'}
    }
    else {case <- 'Error : [dedicated_left_gn] must be one of [yes], [no]. Please check that.'}
  }
  else if (dedicated_left_cbl == 'no'){
    if (dedicated_left_gn == 'yes'){
      if (si_left == 'two-way-protection'){case <- 'case2'}
      else {case <- 'Error : When [dedicated_left_cbl] == [no] and [dedicated_left_gn] == [yes], [si_left] == [two-way-protection] is just available.'}
    }
    else if (dedicated_left_gn == 'no'){
      if (si_left == 'direct-left' | si_left == 'no-left-turn'){case <- 'case4'}
      else {case <- 'Error : When [dedicated_left_cbl] == [no] and [dedicated_left_gn] == [no], [si_left] == [no-left-turn] are just available.'}
    }
    else {case <- 'Error : [dedicated_left_gn] must be one of [yes], [no]. Please check that.'}
  }
  else {case <- 'Error : [dedicated_left_cbl] must be one of [yes], [no]. Please check that.'}
  case
}
