#' Capacity reduction rate index by construction type(CRF)
#'
#' It follows <Table 8-20> in KHCM(2013)
#' @param construction_type Choose one from: \code{'lane_reduce'}, \code{'width_reduce'}, \code{'lane_moved'}, \code{'court_board'}
#' @param lane_reduce Choose one from : \code{'2_to_1'}, \code{'3_to_2'}, \code{'4_to_3'}
#' @param lane_width
#' @export CRF Capacity reduction rate index
#' @examples
#' CRF('court_board')
#' CRF('lane_reduce', '3_to_2')
CRF <- function(construction_type = NULL, lane_reduce = NULL, width_reduce = NULL){
  if (construction_type == 'lane_reduce'){
    if (lane_reduce == '2_to_1'){crf <- 0.83}
    if (lane_reduce == '3_to_2'){crf <- 0.85}
    if (lane_reduce == '4_to_3'){crf <- 0.87}
  }
  if (construction_type == 'width_reduce'){
    if (lane_width < 2.6){crf <- 0.88}
    if (lane_width >= 2.6 & lane_width < 2.9){crf <- 0.94}
    if (lane_width >= 3.0){crf <- 1.00}
  }
  if (construction_type == 'lane_moved'){crf <- 0.90}
  if (construction_type == 'court_board'){crf <- 0.90}
  crf
}
