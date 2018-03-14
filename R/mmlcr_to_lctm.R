#' Converts an R mmclr model output to the format of R's hlme class
#'
#' @param model contains model parameter estimates and maximised likelihood, AIC, BIC values
#' @return  A format to feed into the LCTMtoolkit() R function
#' @examples
#' \dontrun{mmlcr_to_lctm(model)}
#' @export

mmlcr_to_lctm <- function(model)
{
                    mod <- list(NULL)
                    n   <- length(unique(model$components[[1]]$data$ident))
                    K   <- ncol(model$post.prob) - 1

                    mod$pprob <- as.data.frame(model$post.prob[,c("groupe", paste0("PostProb",1:K, sep=""))])
                    mod$pprob <- data.frame("ID"=row.names(model$post.prob), mod$pprob)
                    colnames(mod$pprob) <- c("ID", "class", paste0("prob",1:K, sep=""))

                    mod$call <- "Rmmlcr"
                    mod$par <- exp(model$gamma.matrix)/(sum(exp(c(model$gamma.matrix))))
                    mod$n <- n
                    mod$K <- K
                    mod$logLik <- model$loglikelihood
                    mod$BIC <- model$BIC
                    mod$AIC <- model$AIC
                    mod$best <- unlist(model$components[[1]]$coef)

                    return(mod)
}
#
# os$PI/100
# exp(c(M$best[1:4], 0))/(sum(exp(c(M$best[1:4], 0))))
