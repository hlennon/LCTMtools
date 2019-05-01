#' residualplot_step1
#'
#' A wrapper funciton to implement step 1 of the 8 step framework. This is a wrapper fuction to the lcmm/hlme model fit to examine the class-specific residuals in order to aid choice of random effect distribution.
#'
#' @param model  fitted hlme or lcmm model using the lcmm R package
#' @param nameofoutcome Name of the longitudinal variable in the dataset
#' @param ylimit Plot y-axis limits
#' @return  Class-specific residual plots in ggplot style
#' @examples
#' library(ggplot2)
#' data(bmi_long, package = "LCTMtools")
#' require(lcmm)
#' model2class <- lcmm::hlme(
#'   fixed = bmi ~ age,
#'   mixture = ~age,
#'   random = ~ -1,
#'   nwg = TRUE, ng = 2, subject = "id",
#'   data = bmi_long[1:500, ]
#' )
#' ### residualplot_step1(model2class, nameofoutcome="bmi", type = "line")
#' @export

residualplot_step1 <- function(model, nameofoutcome = "bmi", data = bmi_long, type = "point") {
  library(dplyr)

  k <- model$ng
  preds <- model$pred
  names(preds)[6] <- nameofoutcome
  nameofid <- names(model$pred)[1]

  test <- dplyr::left_join(preds, model$pprob, .by = nameofid)
  test <- dplyr::left_join(test, data, .by = c(nameofid, nameofoutcome))
  test <- test %>%
    group_by(class) %>%
    mutate(Std_resid = resid_ss / sqrt((1 / (length(resid_ss) - 1)) * sum(resid_ss^2)))

  library(ggplot2)

  if (type != "point") {
    p <- ggplot(
      data = test,
      aes(x = age, y = Std_resid, group = id)
    ) +
      geom_line(alpha = 0.3) +
      geom_smooth(mapping = aes(x = age, y = Std_resid, group = NULL), method = "loess") +
      facet_wrap(~class)
  } else {
    p <- ggplot(
      data = test,
      aes(x = age, y = Std_resid, group = id)
    ) +
      geom_poin(alpha = 0.7) +
      geom_smooth(mapping = aes(x = age, y = Std_resid, group = NULL), method = "loess") +
      facet_wrap(~class)
  }
  p
}
