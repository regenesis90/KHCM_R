#' Degree of Roadside Friction on Arterial Roads
#'
#' Criteria for setting the degree of roadside friction on arterial roads.
#'     It follows <Table 12-6> in KHCM(2013) p.536.
#' @param type Arterial road type. Choose one from : \code{'type1'}, \code{'type2'}, \code{'type3'}. See \code{\link{type_artl}}
#' @param bus_stop Number of bus stop(ea/km)
#' @param entr_exit Number of entrance and exit(ea/km)
#' @keywords Roadside Friction Arterial Road
#' @seealso \code{\link{type_artl}}, \code{\link{t_trv_km_artl}}
#' @export friction_artl
#' @examples
#' friction_artl(type = 'type1', bus_stop = 3, entr_exit = 3)
friction_artl <- function(type = NULL, bus_stop = NULL, entr_exit = NULL){
  if (type == 'type1'){
    if (bus_stop > 2 & entr_exit > 2){fr <- 'big'}
    else if (bus_stop <= 2 & entr_exit <= 2){fr <- 'small'}
    else {fr <- 'Please check [bus_stop] and [entr_exit].'}
  }
  else if (type == 'type2'){
    if (bus_stop > 2 & entr_exit > 3){fr <- 'big'}
    else if (bus_stop <= 2 & entr_exit <= 3){fr <- 'small'}
    else {fr <- 'Please check [bus_stop] and [entr_exit].'}
  }
  else if (type == 'type3'){
    if (bus_stop > 2 & entr_exit > 4){fr <- 'big'}
    else if (bus_stop <= 2 & entr_exit <= 4){fr <- 'small'}
    else {fr <- 'Please check [bus_stop] and [entr_exit].'}
  }
  else {fr <- 'Error : [type] must be one of [type1], [type2], [type3]. Please check that.'}
  fr
}
