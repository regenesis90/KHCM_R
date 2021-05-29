#' Lane Use Coefficient(F_U)
#'
#' This function decides Level of Service(LOS). It follows <Table 8-5>
#' @param V_avg *Numeric* Average traffic volume by lane.(vphpl)
#' @param design_level *Categorical* Choose one from : \code{C}, \code{D}, \code{E}
#' @param N *Numeric* Number of exclusive lanes going straight
#' @keywords Lane Use Coefficient
#' @export F_U
#' @examples
#' F_U(V_avg = 1200, N = 3)
#' F_U(790, 2)
F_U <- function(V_avg = NULL, design_level = NULL, N = NULL){
  if (is.null(V_avg) == FALSE){
    if (V_avg <= 800){
      if (N == 1){f <- 1.00}
      if (N == 2){f <- 1.02}
      if (N == 3){f <- 1.10}
      if (N >= 4){f <- 1.15}
    }
    if (V_avg > 800){
      if (N == 1){f <- 1.00}
      if (N == 2){f <- 1.00}
      if (N == 3){f <- 1.05}
      if (N >= 4){f <- 1.08}
    }
  }
  else {
    if (design_level == 'C' | design_level == 'D'){
      if (N == 1){f <- 1.00}
      if (N == 2){f <- 1.02}
      if (N == 3){f <- 1.10}
      if (N >= 4){f <- 1.15}
    }
    if (design_level == 'E'){
      if (N == 1){f <- 1.00}
      if (N == 2){f <- 1.00}
      if (N == 3){f <- 1.05}
      if (N >= 4){f <- 1.08}
    }
  }
  f
}
