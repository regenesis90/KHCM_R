#' Number of lanes required(N)
#'
#' This function calculates the numver of lanes required(N) from PDDHV and SFi. It returns an integer.
#' @param PDDHV *Numeric* Peak Directional Design Hourly Volume(PDDHV, vph). It must be positive.
#' @param SFi *Numeric* #' Service Flow Rate(SFi) It must be positive.
#' @export number_of_lanes_required Number of lanes required
#' @examples
#' number_of_lanes_required(PDDHV = 14000, SFi = 3000)
#' number_of_lanes_required(3000, 500)
number_of_lanes_required <- function(PDDHV = NULL, SFi = NULL){
  if (PDDHV >= 0 & SFi >= 0){
    PDDHV / SFi
  }
}
