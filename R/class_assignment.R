#' Maximum Class assignment
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @return  This function computes the Average Posterior Probability Assignment (APPA) for a K latent class trajectory model.
#' @examples
#' \dontrun{class_assignment(p)}
#' @export

class_assignment <- function(p) {
  as.numeric(apply(p, 1, which.max))
}
