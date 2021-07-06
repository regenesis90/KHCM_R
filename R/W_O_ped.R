#' The width of the sidewalk obstructed by the facility
#'
#' The width of the sidewalk obstructed by the facility on a pedestrian road.
#'     It is an obstruction width caused by factors that impede walking.
#'     It follows <Table 14-7> in KHCM(2013), p.621.
#' @param lamppost Number of lamppost
#' @param signal_controller Number of signal controllar
#' @param fireplug Number of fireplug
#' @param sign Number of road sign
#' @param mailbox Number of mailbox
#' @param p_booth Number of phone booth
#' @param trash_can Number of trash can
#' @param curb Number of curb
#' @param subway_stair Number of subway stairs
#' @param tree Number of tree
#' @param tree_guard Number of tree guard
#' @param pillar Number of pillar
#' @param main_door Number of main door
#' @param revolving_door Number of revolving door
#' @param pipe Number of pipe connection
#' @param awn_pillar Number of awning pillar
#' @param valtype Choose one from : \code{'max'}, \code{'min'}, \code{'mean'}
#' @export W_O_ped(m)
#' @seealso
#' @examples
#' W_O_ped(lamppost = 3, signal_controller = 1, fireplug = 1, trash_can = 3, tree = 2, valtype = 'max')
W_O_ped <- function(lamppost = 0, signal_controller = 0, fireplug = 0,
                sign = 0, mailbox = 0, p_booth = 0, trash_can = 0,
                curb = 0, subway_stair = 0, tree = 0, tree_guard = 0,
                pillar = 0, entrance_stair = 0, revolving_door = 0, pipe = 0, awn_pillar = 0,
                valtype = 0){
  if (valtype == 'max'){
    w <- lamppost * 1.1 + signal_controller * 1.2 + fireplug * 0.9 +
      sign * 0.6 + mailbox * 1.1 + p_booth * 1.2 + trash_can * 0.9 +
      curb * 0.5 + subway_stair * 2.1 + tree * 1.2 + tree_guard * 1.5 +
      pillar * 0.9 + entrance_stair * 1.8 + revolving_door * 2.1 + pipe * 0.3 + awn_pillar * 0.8
  }
  else if (valtype == 'min'){
    w <- lamppost * 0.8 + signal_controller * 0.9 + fireplug * 0.8 +
      sign * 0.6 + mailbox * 1.0 + p_booth * 1.2 + trash_can * 0.9 +
      curb * 0.5 + subway_stair * 1.7 + tree * 0.6 + tree_guard * 1.5 +
      pillar * 0.8 + entrance_stair * 0.6 + revolving_door * 1.5 + pipe * 0.3 + awn_pillar * 0.8
  }
  else if (valtype == 'mean'){
    w <- lamppost *0.95 + signal_controller * 1.05 + fireplug * 0.85 +
      sign * 0.6 + mailbox * 1.05 + p_booth * 1.2 + trash_can * 0.9 +
      curb * 0.5 + subway_stair * 1.9 + tree * 0.9 + tree_guard * 1.5 +
      pillar * 0.85 + entrance_stair * 1.2 + revolving_door * 1.8 + pipe * 0.3 + awn_pillar * 0.8
  }
  else {w <- 'Error : [valtype] must be one of [max], [min], [mean]. Please check that.'}
  w
}


