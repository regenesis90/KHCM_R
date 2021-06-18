#' Service Flow Rate
#'
#' This function calculates Service Flow Rate(vph) under basic conditions.
#'     It follows <Formula 2-2>, Formula <2-3> in KHCM(2013), p.22.
#' @param MSF_i Maximum Service Flow Rate(pcphpl). See \code{\link{MSF_i}}.
#' @param N The number of lane(one-way).
#' @param f_w The lane width and side clearance factor. See \code{\link{f_w_expwy_basic}}.
#' @param f_hv Heavy Vehicle Factors. See \code{\link{f_hv_expwy_basic}}.
#' @keywords serfice flow rate sfi
#' @export SF_i Service Flow Rate under LOS i, given road and traffic conditions(vph)
#' @seealso \code{\link{MSF_i}}, \code{\link{f_w_expwy_basic}}, \code{\link{f_hv_expwy_basic}}.
#' @examples
#' SF_i(MSF_i = 1570, N = 3, f_w = 0.74, f_hv = 0.601)
SF_i <- function(MSF_i = NULL, N = NULL, f_w = NULL, f_hv = NULL){
  if (MSF_i >= 0){
    if (N >= 2){
      if (f_w > 0){
        if (f_hv > 0){res <- MSF_i * N * f_w * f_hv}
        else {res <- 'Error : [f_hv] must be positive. Please check that. See [f_hv_expw_basic()].'}
      }
      else {res <- 'Error : [f_w] must be positive. Please check that. See [f_w_expwy_basic()].'}
    }
    else {res <- 'Error : [N] must be >= 2 and integer. Please check that.'}
  }
  else {res <- 'Error : [MSF_i] must be positive(pcphpl). Please check that. See [MSF_i()].'}
  res
}
