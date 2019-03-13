#' A model comparison toolkit.
#' \code{LCTMcompare}
#'
#' The function LCTMcompare gives a summary of comparison between fitted LCTM models.
#'
#' @param modelA is the output from hlme() R model or model is the output of SASmodelbuilder(oe, os, op, of) passed through it
#' @param modelB the model to be compared which is the output from hlme() R model or model is the output of SASmodelbuilder(oe, os, op, of) passed through it
#' @return A selection of model adequacy tests, including the APPA (average posterior probability of assignment), the OCC (odds of correct classification), entropy $E$, Relative entropy ($E_k$),
#' @references \url{https://bmjopen.bmj.com/content/8/7/e020683}
#' @examples
#' data(bmi_long, package='LCTMtools')
#' require(lcmm)
#' set.seed(999)
#' data(bmi_long, package = 'LCTMtools' )
#' # Use the hlme function from the 'lcmm' R package to fit a 2 class latent class trajectory model
#' model2classes <- lcmm::hlme(fixed = bmi ~ age + I(age^2),
#'                       mixture= ~ age,
#'                       random = ~ age,
#'                       ng = 2,
#'                       nwg = TRUE,
#'                       subject = "id",
#'                       data = bmi_long[1:500, ] )
#' # Compute model adequacy measures
#' LCTMtoolkit(model2classes)
#' # Compare with a 3 class model
#' model3classes <- lcmm::hlme(fixed = bmi ~ age + I(age^2),
#'                       mixture= ~ age,
#'                       random = ~ age,
#'                       ng = 3,
#'                       nwg = TRUE,
#'                       subject = "id",
#'                       data = bmi_long[1:500, ] )
#' LCTMtoolkit(model3classes)
#' LCTMcompare(model2classes, model3classes)
#' @export

LCTMcompare <- function(modelA, modelB) {


    model <- modelA
    n <- nrow(model$pprob)
    K <- ncol(model$pprob) - 2
    p <- model$pprob[, c(paste0("prob", 1:K, sep = ""))]


    if (class(model$call) == "SAS") {
        PI <- os$PI/100
    } else {
        PI <- exp(c(model$best[1:(K - 1)], 0))/(sum(exp(c(model$best[1:(K - 1)],
            0))))
    }



    output1 <- t(data.frame(Entropy = entropy(p), Relative_entropy = relative_entropy(p),
        BIC = model$BIC, AIC = model$AIC))



    model <- modelB
    n <- nrow(model$pprob)
    K <- ncol(model$pprob) - 2
    p <- model$pprob[, c(paste0("prob", 1:K, sep = ""))]


    if (class(model$call) == "SAS") {
        PI <- os$PI/100
    } else {
        PI <- exp(c(model$best[1:(K - 1)], 0))/(sum(exp(c(model$best[1:(K - 1)],
            0))))
    }

    output2 <- t(data.frame(Entropy = entropy(p), Relative_entropy = relative_entropy(p),
        BIC = model$BIC, AIC = model$AIC))

    output1 <- round(output1, 3)
    output2 <- round(output2, 3)
    Recommendation <- c("Close to zero", "Close to 1", "-", "-")
    output <- data.frame(output1, output2, Recommendation)

    return(output)
}
