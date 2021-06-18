#' Maximum Service Flow Rate
#'
#' Maximum service traffic per lane in basic conditions, service level i(pcphpl).
#'    It follows <Formula 2-1> in KHCM(2013), p.23.
#' @param c_j Basic capacity of basic section of expressway at design speed j(pcphpl). See \code{\link{capa_expwy_basic_j}}
#' @param v_c_ratio_i V/c ratio under LOS i
#' @export MSF_i \code{c_j * v_c_ratio_i} Maximum service flow rate in LOS i(pcphpl)
#' @keywords
#' @seealso \code{\link{capa_expwy_basic_j}}, \code{\link{SF_i}}
#' @examples
#' MSF_i(c_j = 1700, v_c_ratio_i = 0.9)
#' MSF_i(2000, 0.76)
MSF_i <- function(c_j = NULL, v_c_ratio_i = NULL){
  if (c_j > 0){
    if (v_c_ratio_i >= 0){res <- c_j * v_c_ratio_i}
    else {res <- '[v_c_ratio_i] must be positive. Please check that.'}
  }
  else {res <- '[c_j] must be positive(pcphpl). Please check that. See [capa_expwy_basic_j()].'}
  res
}
