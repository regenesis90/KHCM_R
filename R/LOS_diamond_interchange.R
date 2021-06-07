#' Level of Service(LOS) in Diamond-Interchange
#'
#' This function decides Level of Service(LOS). It follows <Table 6-8>
#' @param control_delay Control delay per Car(second)
#' @keywords LOS Level of Service
#' @export LOS_diamond_interchange Level of Service. \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}, \code{'FF'}, \code{'FFF'}
#' @examples
#' LOS_diamond_interchange(33.4427)
LOS_diamond_interchange <- function(control_delay = NULL){
  if (control_delay >= 0 & control_delay <= 22){LOS <- 'A'}
  if (control_delay >= 22 & control_delay <= 45){LOS <- 'B'}
  if (control_delay >= 45 & control_delay <= 75){LOS <- 'C'}
  if (control_delay >= 75 & control_delay <= 105){LOS <- 'D'}
  if (control_delay >= 105 & control_delay <= 150){LOS <- 'E'}
  if (control_delay >= 150 & control_delay <= 330){LOS <- 'F'}
  if (control_delay >= 330 & control_delay <= 510){LOS <- 'FF'}
  if (control_delay > 510){LOS <- 'FFF'}
  LOS
}
