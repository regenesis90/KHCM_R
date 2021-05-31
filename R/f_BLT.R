#' Turn correction factor for dedicated left-turn lane group(f_BLT)
#'
#' It follows <Formula 8-57> in KHCM(2013)
#' @param E_L
#' @export f_BLT Turn correction factor for dedicated left-turn lane group
#' @examples
f_BLT <- function(E_L = NULL){
  if (E_L != 0){
    f <- 1/E_L
    f
  }
}
