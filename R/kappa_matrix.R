#' Kappa matrix
#'
#' Kappa matrix of cohen's kappa values
#'
#' @param ConfMatrix  A confusion matrix made using confusion_matrix(model1, model2)
#' @param acc The accuracy of the results, defaults to 2 decimal places
#' @return  Unweighted and weighted Kappa value computed using the cohen.kappa() function from the psych R package
#'
#' @examples
#' data(bmi_long, package="LCTMtools")
#' require(lcmm)
#' require(psych)
#' model1 <- hlme(BMI ~Age, mixture= ~Age, random= ~Age, nwg=T, ng=2, subject="ID", data=bmi_long[1:500, ])
#' model2 <- hlme(BMI ~Age, mixture= ~Age, random= ~1, nwg=F, ng=2, subject="ID", data=bmi_long[1:500, ])
#' ConfMatrix <- confusion_matrix(model1, model2)
#' kappa_matrix(ConfMatrix)
#' @export

kappa_matrix <- function(ConfMatrix, acc=2){
                    K <- nrow(ConfMatrix)-1
                    x <- cohen.kappa(ConfMatrix[1:K, 1:K])
                    mxu <- round(x$kappa,acc)
                    lou <- round(x$confid[1,1],acc)
                    upu <- round(x$confid[1,3],acc)
                    mxw <- round(x$weighted.kappa,acc)
                    low <- round(x$confid[2,1],acc)
                    upw <- round(x$confid[2,3],acc)
                    unw <- c(paste0("Kappa Values: (unweighted) ", mxu, " (", lou,", ",upu, ") "))
                    w   <- c(paste0("Kappa Values: (weighted) ", mxw, " (", low,", ",upw, ") "))

                    return(list(unweighted_kappa=unw, weighted_kappa=w))
}
