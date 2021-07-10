#' Critical Interval of Unsignalized Two-way Stop Intersection
#'
#' Critical interval (seconds) for the flow x of the two-way stop intersection(type1)
#'     among unsignalized intersections
#'     It follows <Table 10-1> in KHCM(2013), p.466.
#' @param form Intersection form. Choose one from: \code{'1x1'}, \code{'2x1'}
#' @param dir1 Flow direction. Choose one from : \code{'main'}, \code{'sub'}
#' @param dir2 Flow direction. Choose one from: \code{'left'}, \code{'straight'}, \code{'right'}
#' @keywords Critical Interval two-way stop non-signalized Unsignalized intersection
#' @seealso \code{\link{t_f_x_nsi}}, \code{\link{c_p_x_nsi}}
#' @details
#'     * \code{form == '1x1'} : Intersection of one-lane one-way roads on the main road and one-lane lanes on the sub-road (1×1)
#'     * \code{form == '2x1'} : Intersection of two lanes one-way on the main road and one-lane one-way on the sub-road (2×1)
#' @export t_c_x_nsi
#' @examples
#' t_c_x_nsi(form = '1x1', dir1 = 'main', dir2 = 'left')
#' t_c_x_nsi(form = '2x1', dir1 = 'sub', dir2 = 'right')
t_c_x_nsi <- function(form = NULL, dir1 = NULL, dir2 = NULL){
  if (form == '1x1'){
    if (dir1 == 'main'){
      if (dir2 == 'left'){tcx <- 4.2}
      else {tcx <- 'Error : When dir1 == main, [dir2] must be [left]. Please check that.'}
    }
    else if (dir1 == 'sub'){
      if (dir2 == 'left'){tcx <- 4.6}
      if (dir2 == 'straight'){tcx <- 4.5}
      if (dir2 == 'right'){tcx <- 3.7}
    }
    else {tcx <- 'Error : [dir1] must be one of [main], [sub]. Please check that.'}
  }
  else if (form == '2x1'){
    if (dir1 == 'main'){
      if (dir2 == 'left'){tcx <- 4.9}
      else {tcx <- 'Error : When dir1 == main, [dir2] must be [left]. Please check that.'}
    }
    else if (dir1 == 'sub'){
      if (dir2 == 'left'){tcx <- 5.2}
      else if (dir2 == 'straight'){tcx <- 5.4}
      else if (dir2 == 'right'){tcx <- 4.4}
    }
    else {tcx <- 'Error : [dir1] must be one of [main], [sub]. Please check that.'}
  }
  else {tcx <- 'Error : [form] must be one of [1x1], [2x1]. Please check that.'}
  tcx
}
