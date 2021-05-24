#' Basic capacity of construction section by design speed(pcphpl)
#'
#' This function decides Basic capacity of construction section by design speed(pcphpl). The contents of <Table 2-7> of KHCM(2013) were reflected.
#' @param design_speed *Categorical* Choose one from : \code{120}, \code{100}, \code{80} (kph)
#' @export basic_capacity_in_construction Basic capacity of construction section by design speed(pcphpl)
#' @examples
#' basic_capacity_in_construction(80)
#' basic_capacity_in_construction(design_speed = 120)
basic_capacity_in_construction <- function(design_speed = NULL){
  if (design_speed == 80){basic_capacity <- 1650}
  if (design_speed == 100){basic_capacity <- 1700}
  if (design_speed == 120){basic_capacity <- 1750}
  basic_capacity
}
