#' Free speed on arterial roads(free_speed_arterial)
#'
#' It follows <Table 12-2> in KHCM(2013) p.533
#' @param road_standard \code{'high'}, \code{'intermediate'}, \code{'low'}
#' @param road_condition \code{'good'}, \code{'normal'}
#' @keywords
#' @export free_speed_arterial
#' @examples
#' free_speed_arterial('high', 'good')
free_speed_arterial <- function(road_standard = NULL, road_condition = NULL){
  if (road_standard == 'high'){
    if (road_condition == 'good'){fs <- 80}
    if (road_condition == 'normal'){fs <- 80}
  }
  if (road_standard == 'intermediate'){
    if (road_condition == 'good'){fs <- 80}
    if (road_condition == 'normal'){fs <- 70}
  }
  if (road_standard == 'low'){
    if (road_condition == 'good'){fs <- 70}
    if (road_condition == 'normal'){fs <- 60}
  }
  fs
}
