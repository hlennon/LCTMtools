#' A model comparison toolkit.
#' \code{LCTMcompare}
#'
#' The function LCTMcompare gives a summary of comparison between fitted LCTM models.
#'
#' @param model is the output from hlme() R model or model is the output of SASmodelbuilder(oe, os, op, of) passed through it
#' @return A selection of model adequacy tests, including the APPA (average posterior probability of assignment), the OCC (odds of correct classification), entropy $E$, Relative entropy ($E_k$),
#' @examples
#' data(bmi_long, package="LCTMtools")
#' require(lcmm)
#' modelA <- hlme(BMI ~Age, mixture= ~Age, random= ~Age,
#' nwg=TRUE, ng=2, subject="ID", data=bmi_long[1:500, ])
#' postprob(model2class)
#' modelB <- hlme(BMI ~Age, mixture= ~1, random= ~1,
#' nwg=FALSE, ng=2, subject="ID", data=bmi_long[1:500, ])
#' postprob(modelA, modelB)
#' LCTMcompare(modelA, modelB)
#' @export

LCTMcompare <- function(modelA, modelB){


                    n <- nrow(model$pprob)
                    K <- ncol(model$pprob)-2
                    p <- model$pprob[,c(paste0("prob",1:K, sep=""))]


                    if(class(model$call)=="SAS"){
                                        PI <- os$PI/100
                    }else{
                                        PI <- exp(c(model$best[1:(K-1)], 0))/(sum(exp(c(model$best[1:(K-1)], 0))))
                                        }


                    outputs <- matrix(0, nrow=3, ncol=K)
                    colnames(outputs) <- paste0("Class_", 1:K, sep="")
                    rownames(outputs) <- c("APPA", "OCC", "Mismatch")
                    outputs[1, ] <- appa(p)
                    outputs[2, ] <- occ(p, PI)
                    outputs[3, ] <- mismatch(p, PI)
                    Recommendation <- c("Greater than 0.7", "Greater than 5", "Close to zero")
                    outputs <- data.frame(round(outputs,3),Recommendation)

                    outputs1 <- t(data.frame("Entropy"=entropy(p), "Relative_entropy"=relative_entropy(p), "BIC"=model$BIC, "AIC"=model$AIC))
                    Recommendation <- c("Close to zero","Close to 1")
                    outputs1 <- round(outputs1,3)
                    outputs1 <- data.frame(Model=outputs1,Recommendation=Recommendation)

                    outputs2 <- list(outputs, outputs1)
                    names(outputs2) <- c('Class-specific', 'Model-specific')
                    return(outputs2)
}