#' A confusion matrix
#' @usage confusion_matrix(model1, model2)
#' @param model1  A fitted model from the lcmm R package (or from SAS passed through the SASmodelbuilder() function)
#' @param model2  is the posterior probabilities of assignment of dimensions, K columns and N rows
#' @param name1 optional paramter to pre-specify name of model
#' @param name2 optional paramter to pre-specify name of model
#' @return  A confusion matrix between two models with the same number of classes
#' @examples
#' \dontrun{
#' data(bmi_long, package="LCTMtools")
#' require(lcmm)
#' model1 <- hlme(BMI ~Age, mixture= ~Age, random= ~Age, nwg=TRUE, ng=2, subject="ID", data=bmi_long[1:500, ])
#' model2 <- hlme(BMI ~Age, mixture= ~Age, random= ~1, nwg=FALSE, ng=2, subject="ID", data=bmi_long[1:500, ])
#' confusion_matrix(model1, model2)}
#' @export

confusion_matrix <- function(model1, model2, name1 = "Model_1", name2 = "Model_2") {
  # model1 <- model1[sort(model1$ID),]
  R <- model1$pprob$class
  Sas <- model2$pprob$class
  K <- ncol(model1$pprob) - 2


  a <- eval(parse(text = paste0("c(", paste0("sum(R==1&Sas==", 1:K, ")", collapse = ","), ")")))

  for (i in 2:K) {
    b <- eval(parse(text = paste0("c(", paste0("sum(R==", i, "&Sas==", 1:K, ")", collapse = ","), ")")))
    a <- rbind(a, b)
  }

  a <- cbind(a, rowSums(a))
  a <- rbind(a, colSums(a))
  colnames(a) <- c(paste0("Class_", 1:K), name1)
  rownames(a) <- c(paste0("Class_", 1:K), name2)
  return(a)
}
