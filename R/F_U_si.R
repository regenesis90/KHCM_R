#' Lane Use Coefficient at Signalized Intersection
#'
#' Lane use coefficient at signal intersections.\
#'     It is a coefficient calculated for correcting the lane utilization ratio of traffic volume.
#'     It follows <Table 8-5> in KHCM(2013), p.225.
#' @param v_avg Average traffic volume by lane.(vphpl)
#' @param design_level Road design level. Choose one from : \code{C}, \code{D}, \code{E}
#' @param N Number of exclusive lanes going straight.
#' @keywords Lane Use Coefficient Signalized Intersection
#' @seealso \code{\link{V_si}}
#' @details
#'     This coefficient is also applied when there are two or more dedicated left (or right) turn lanes,
#'     as is common at approach roads with two or more dedicated left-turn lanes or T-shaped intersections.
#' @export F_U_si
#' @examples
#' F_U_si(v_avg = 1200, N = 3)
#' F_U_si(design_level = 'C', N = 3)
F_U_si <- function(v_avg = NULL, design_level = NULL, N = NULL){
  if (is.null(v_avg) == FALSE){
    if (v_avg <= 800){
      if (N == 1){f <- 1.00}
      else if (N == 2){f <- 1.02}
      else if (N == 3){f <- 1.10}
      else if (N >= 4){f <- 1.15}
      else {f <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else if (v_avg > 800){
      if (N == 1){f <- 1.00}
      else if (N == 2){f <- 1.00}
      else if (N == 3){f <- 1.05}
      else if (N >= 4){f <- 1.08}
      else {f <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else {f <- 'Error : [v_avg] must be positive(vphpl). Please check that.'}
  }
  else if (is.null(design_level) == FALSE) {
    if (design_level == 'C' | design_level == 'D'){
      if (N == 1){f <- 1.00}
      else if (N == 2){f <- 1.02}
      else if (N == 3){f <- 1.10}
      else if (N >= 4){f <- 1.15}
      else {f <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else if (design_level == 'E'){
      if (N == 1){f <- 1.00}
      else if (N == 2){f <- 1.00}
      else if (N == 3){f <- 1.05}
      else if (N >= 4){f <- 1.08}
      else {f <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else {f <- 'Error : [design_level] must be one of [C], [D], [E]. Please check that.'}
  }
  else {f <- 'Error : [v_avg] or [design_level] must be positive. Please check that.'}
  f
}
