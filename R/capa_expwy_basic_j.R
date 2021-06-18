#' Basic Capacity of Basic Section of Expressway at Design Speed j(pcphpl)
#'
#' It follows <Table 2-1> in KHCM(2013), p.20.
#' @param design_speed Design speed(kph). Choose one from : \code{80}, \code{100}, \code{120}.
#' @export capa_expwy_basic_j Basic capacity in basic section of expressway(pcphpl).
#' @examples
#' capa_expwy_basic_j(design_speed = 100)
#' capa_expwy_basic_j(120)
capa_expwy_basic_j <- function(design_speed = NULL){
  if (design_speed == 80){result <- 2000}
  else if (design_speed == 100){result <- 2200}
  else if (design_speed == 120){result <- 2300}
  else {result <- 'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
  result
}
