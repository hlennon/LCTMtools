#' Converts a SAS proc traj model to the format of R's hlme class
#'
#' @param oe contains model parameter estimates and maximised likelihood, AIC, BIC values
#' @param of contains posterior probabilities
#' @param op contains predictors
#' @param os containts fixed effect  and class membershop parameter estimates
#' @return  A format to feed into the LCTMtoolkit() R function
#' @examples
#' \dontrun{sastraj_to_lctm(oe, of, op, os)
#' os$PI/100
#' exp(c(M$best[1:4], 0))/(sum(exp(c(M$best[1:4], 0))))
#' }
#' @export

sastraj_to_lctm <- function(oe, of, op, os) {
  n <- nrow(of)
  K <- nrow(os)
  mod <- list(NULL)
  mod$pprob <- as.data.frame(of[, c("ID", "GROUP", paste0("GRP", 1:K, "PRB", sep = ""))])
  colnames(mod$pprob) <- c("ID", "class", paste0("prob", 1:K, sep = ""))
  mod$call <- "SAS"
  # model$par <- os$PI
  model <- mod
  return(model)
}
