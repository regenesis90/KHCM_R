#' The width of the sidewalk obstructed by the facility
#'
#' It follows <Table 14-7> in KHCM(2013), p.621
#' @param f *Categorical* factors that impede walking
#' @param valtype *Categorical* Choose maximum, minimum, mean value
#' @export W_O (m)
#' @examples
#' W_O(f = 'street_tree', valtype = 'mean')
#' W_O('subway_stairs', 'max')
W_O <- function(f = NULL, valtype = NULL){
  if (f == 'lamppost'){
    if (valtype == 'min'){w <- 0.8}
    if (valtype == 'max'){w <- 1.1}
    if (valtype == 'mean'){w <- 0.95}
  }
  if (f == 'signal_controller_and_pillar'){
    if (valtype == 'min'){w <- 0.9}
    if (valtype == 'max'){w <- 1.2}
    if (valtype == 'mean'){w <- 1.05}
  }
  if (f == 'fireplug'){
    if (valtype == 'min'){w <- 0.8}
    if (valtype == 'max'){w <- 0.9}
    if (valtype == 'mean'){w <- 0.85}
  }
  if (f == 'road_sign'){w <- 0.6}
  if (f == 'mailbox'){
    if (valtype == 'min'){w <- 1.0}
    if (valtype == 'max'){w <- 1.1}
    if (valtype == 'mean'){w <- 1.05}
  }
  if (f == 'telephone_booth'){w <- 1.2}
  if (f == 'trash_can'){w <- 0.9}
  if (f == 'curb'){w <- 0.5}
  if (f == 'subway_stairs'){
    if (valtype == 'min'){w <- 1.7}
    if (valtype == 'max'){w <- 2.1}
    if (valtype == 'mean'){w <- 1.9}
  }
  if (f == 'street_tree'){
    if (valtype == 'min'){w <- 0.6}
    if (valtype == 'max'){w <- 1.2}
    if (valtype == 'mean'){w <- 0.9}
  }
  if (f == 'street_tree_guard'){w <- 1.5}
  if (f == 'pillar'){
    if (valtype == 'min'){w <- 0.8}
    if (valtype == 'max'){w <- 0.9}
    if (valtype == 'mean'){w <- 0.85}
  }
  if (f == 'revolving_door'){
    if (valtype == 'min'){w <- 1.5}
    if (valtype == 'max'){w <- 2.1}
    if (valtype == 'mean'){w <- 1.8}
  }
  if (f == 'pipe_connection'){w <- 0.3}
  if (f == 'awning_pillar'){w <- 0.8}
  w
}
