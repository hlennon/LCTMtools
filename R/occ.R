#' The odds of correct classification is the ratio of the odds of classification based on the maximum posterior probablity classification rule and the estimated class membership proportions (pi_k).
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @param pi is the estimated proportion of class membership of length K
#' @return  The odds of correct classification
#' @examples
#' \dontrun{occ(p, pi)}
#' @export

occ <- function(p, pi) {
  app <- appa(p)
  numerator <- app / (1 - app)
  denominator <- pi / (1 - pi)
  occ <- numerator / denominator
  return(occ)
}
