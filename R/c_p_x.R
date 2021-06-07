#' Potential capacity of moving flow x (c_p_x, pcph)
#'
#' It follows <Formula 10-1> in KHCM(2013), p.466
#' @param V_c_x Conflict flow for flow x. see V_ci()
#' @param t_c_x Critical spacing for moving flow x. See t_c_x()
#' @param t_f_x Follow-up time for flow x. See t_f_x()
#' @keywords
#' @export c_p_x
#' @examples
#' c_p_x(V_c_x = 1200, t_c_x = 4.6, t_f_x = 3.0)
#' c_p_x(832, 5.4, 2.7)
c_p_x <- function(V_c_x = NULL, t_c_x = NULL, t_f_x = NULL){
  V_c_x * (exp(-1 * V_c_x * t_c_x / 3600))/(1 - exp(-1 * V_c_x * t_f_x / 3600))
}
