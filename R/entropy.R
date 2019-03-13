#' Entropy
#'
#' A global measure of uncertainty with values close to zero implying a good model. Entropy is a global measure of classification uncertainty, which takes into account all N × K posterior probabilities. The entropy of a model is defined as which takes values from [0,infinity), with higher values indicating a larger amount of uncertainty. Entropy values closest to 0 correspond to models with least classification uncertainty.
#'
#' @param p  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @return  Entropy value between (0, infinity)
#' @examples
#' \dontrun{entropy(p)}
#' @export

entropy <- function(p) {
    ent <- -1 * sum(p * log(p), na.rm = TRUE)
    
    return(ent)
}
