#' Average Travel Speed in type_1 2-lane road, i Section(ATS_1_i, kph)
#'
#' This function follows <Formula 6-2>
#' @param FFS Free speed(kph)
#' @param V_d *Numeric* Traffic in the direction of travel(pcph?)
#' @param V_o *Numeric* Counter traffic(pcph?)
#' @param f_np_ATS *Numeric* See <Table 7-12>
#' @param f_w_ATS *Numeric* See <Table 7-11>
#' @keywords
#' @export ATS_1_i #' Average Travel Speed in type_1 2-lane road, i Section(kph)
#' @examples
ATS_1_i <- function(FFS = NULL, V_d = NULL, V_o = NULL, f_np_ATS = NULL, f_w_ATS = NULL){
  if (FFS >=0 & V_d >= 0 & V_o >= 0 & f_np_ATS > 0 & f_w_ATS > 0){
    ATS <- FFS - 0.0132 * V_d - 0.0037 * V_o - f_np_ATS - f_w_ATS
    ATS
  }
}
ATS_1_i(FFS = 100, V_d = 1000, V_o = 700, f_np_ATS = ) #미완성입니다.
