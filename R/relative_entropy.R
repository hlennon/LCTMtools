#' The Relative Entropy
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @return  Relative Entropy - where values close to 1 indicate lowest classification uncertainty. In the special case when there is most uncertainty and each individual has equal probability of belonging to each class, E_K=0. Jedidi et al., describes relative entropy as a relative measure of ‘fuzziness’, and suggested cause concern when close to zero, as this implies that the latent class centroids are not sufficiently separated.
#' @examples
#' \dontrun{relative_entropy(p)}
#' @export


relative_entropy <- function(p) {
  K <- ncol(p)
  n <- nrow(p)

  relEntropy <- 1 + (sum(p * log(p)) / (n * log(K)))

  return(relEntropy)
}
