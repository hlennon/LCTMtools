#' Computes the Average Posterior Probability Assignment (APPA) for a K latent class trajectory model.
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @return The Average Posterior Probability Assignment (APPA) for each class
#' @examples
#' \dontrun{appa(p)}
#' @export

appa <- function(p) {
  # determine class size
  K <- ncol(p)

  # determine class
  group <- class_assignment(p)

  # save vector for appa values
  app <- rep(NA, times = K)

  # Compute average posterior probabilites
  for (i in 1:K) {
    classp <- p[group == i, i]
    if (length(classp) != 0) app[i] <- mean(classp)
  }
  return(app)
}
