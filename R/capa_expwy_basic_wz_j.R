#' Basic Capacity of Expressway Work Zone at Design Speed j(pcphpl)
#'
#' It follows <Table 2-6> in KHCM(2013), p.37.
#' @param design_speed Design speed(kph). Choose one from : \code{80}, \code{100}, \code{120}.
#' @export capa_expwy_basic_wz_j Basic capacity in Expressway Work Zone(pcphpl).
#' @examples
#' capa_expwy_basic_wz_j(design_speed = 100)
#' capa_expwy_basic_wz_j(120)
capa_expwy_basic_wz_j <- function(design_speed = NULL){
  if (design_speed == 80){result <- 1650}
  else if (design_speed == 100){result <- 1700}
  else if (design_speed == 120){result <- 1750}
  else {result <- 'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
  result
}
