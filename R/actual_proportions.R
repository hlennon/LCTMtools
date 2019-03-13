#' The proportion within each class
#'
#' \code{actual_proportions}
#' The proportion within each class AFTER class assignment (using max posterior rule)
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @return  The proportion within each class AFTER class assignment (using max posterior rule)
#' @examples
#' \dontrun{actual_proportions(p)}
#' @export

actual_proportions <- function(p) {
    K <- ncol(p)

    # determine class
    group <- class_assignment(p)

    # Tabulate the actual proportions
    actprop <- tabulate(group, nbins = K)/nrow(p)

    # return the proportion of individuals assignes using the max post prob rule
    return(actprop)
}
