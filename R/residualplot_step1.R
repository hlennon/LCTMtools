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
#' data(bmi_long, package='LCTMtools')
#' require(lcmm)
#' model2class <- lcmm::hlme(fixed = bmi ~ age,
#' mixture= ~ age,
#' random= ~ -1,
#' nwg=TRUE, ng=2, subject="id",
#' data=bmi_long[1:500, ])
#' ###residualplot_step1(model, nameofoutcome="bmi", ylimit=c(-5,5))
#' @export

residualplot_step1 <- function(model, nameofoutcome="bmi", ylimit=c(-5,5)){


                    k     <- model1$ng
                    preds <- model1$pred
                    names(preds)[6] <- nameofoutcome
                    nameofid        <- names(model$pred)[1]
                    test <- dplyr::left_join(preds, model$pprob, .by=nameofid)
                    test <- dplyr::left_join(test, bmi_long, .by=c(nameofid, nameofoutcome))

                    plotvalues <- NULL

                    for(i in 1:k){

                                        newplotvalues <- test %>% filter(class==i) %>% mutate(Residuals=bmi-eval(parse(text=paste0("pred_ss",i))))
                                        plotvalues <- rbind(plotvalues, newplotvalues)

                                        plotvaluessub <- plotvalues %>% filter(class==i)



                                        pname <- paste0("p", i)
                                        assign(pname,  ggplot(data = plotvaluessub, aes(x = age, y = Residuals, group = class))+
                                                                   theme(axis.text=element_text(size=16),text = element_text(size=16)) +
                                                                   geom_point() +
                                                                   stat_summary(fun.y=mean, geom="line", size = 3, col="blue", group=1) +
                                                                   ggtitle("Residuals in class", i) +
                                                                   ylim(ylimit))

                    }
                    return(print(eval(parse(text=(paste0("p",1:k))))))

                    # print(ggarrange(eval(parse(text=(paste0("p",1:k)))), width=1, ncol = 2))
}
