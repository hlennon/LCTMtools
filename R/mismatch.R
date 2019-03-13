#' Computes the mismatch of the posterior probabilities (mismatch=actual-estimated)
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @param pi is the estimated proportion of class membership of length K
#' @return  The mismatch of posterior probabilities
#' @examples
#' \dontrun{mismatch(p, pi)}
#' @export

mismatch <- function(p, pi) {
    # determine class
    group <- class_assignment(p)
    K <- ncol(p)
    
    # Tabulate the actual proportions
    actprop <- tabulate(group, nbins = K)/nrow(p)
    
    # Compute the mismatch (mismatch=actual-estimated)
    return(actprop - pi)
}
