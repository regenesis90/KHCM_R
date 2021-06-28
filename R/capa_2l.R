#' Capacity in 2-lane Road
#'
#' Capacity of 2-lane Road on basic condition.
#'    Basically, It is 'Bi-directional' value(3200 pcph).
#'    One-direction capacity is 1700pcphpl.
#'    It follows some definitions KHCM(2013), p.170.
#' @param direction Choose one from : \code{'bidirectional'}, \code{'one-way'}
#' @export capa_2l
#' @keywords capacity 2-lane road
#' @seealso \code{\link{appl_2l}}
#' @examples
#' capa_2l('bidirectional')
#' capa_2l('one_way')
capa_2l <- function(direction= NULL){
  if (direction == 'bidirectional'){capa <- 3200}
  else if (direction == 'one_way'){capa <- 1700}
  else {capa <- 'Error : [direction] must be one of [bidirectional], [one_way]. Please check that.'}
  capa
}
