#' plotLCTM
#'
#' A global measure of uncertainty with values close to zero implying a good model. Entropy is a global measure of classification uncertainty, which takes into account all N × K posterior probabilities. The entropy of a model is defined as which takes values from [0,infinity), with higher values indicating a larger amount of uncertainty. Entropy values closest to 0 correspond to models with least classification uncertainty.
#'
#' @param m  fitted hlme or lcmm model using the lcmm R package
#' @return  A plot in ggplot style
#' @examples
#' library(ggplot2)
#' data(bmi_long, package='LCTMtools')
#' require(lcmm)
#' model2class <- lcmm::hlme(fixed = bmi ~ age,
#' mixture= ~ age,
#' random= ~ age,
#' nwg=TRUE, ng=2, subject="id",
#' data=bmi_long[1:500, ])
#' plotLCTM(model2class, shape="linear")
#' plotLCTM(m, shape="linear")
#' model2class_splines <- lcmm::hlme(fixed = bmi ~ ns(age, knots=2),
#' mixture= ~ ns(age, knots=2),
#' random= ~ age,
#' nwg=TRUE, ng=2, subject="id",
#' data=bmi_long[1:500, ])
#' newdat <-  data.frame(age=seq(xlimit[1], xlimit[2], length=100))
#' plotLCTM(model2class_splines, shape="splines", splinesnewdata = newdat)
#' @export
plotLCTM <- function(m, shape, xlimit=c(0, 4.7), ylimit=c(20, 40), splinesnewdata=NULL){


                    # Number of classes
                    k <- m$ng
                    cols <- gg_color_hue(k)
                    ests <- as.numeric(m$best)
                    if(shape=="linear"){     s <- 2 }
                    if(shape=="quadratic"){  s <- 3 }

                    age  <- seq(from = xlimit[1], to = xlimit[2], length = 10)

                    # plot(agex, f[1,1] + f[1,2]*agex, ylim=ylimit, type="l")
                    # for(i in 2:k) lines(agex, f[i,1] + f[i,2]*agex)

                    ymeantraj <- NULL
                    if(shape=="linear"){
                                        f <- ests[k:((k-1)+k*s)]
                                        # arrange as 1 row per class
                                        f <- matrix(f, ncol=k, nrow=s)
                                        for(i in 1:k) ymeantraj[[i]] <- f[i,1] + f[i,2]*age
                                        ymeantrajmat <- sapply(ymeantraj, cbind)
                                        colnames(ymeantrajmat) <- paste("Class", 1:k)


                                        ymeantraj <- tidyr::gather(as.data.frame(ymeantrajmat), key="class", value=y)
                                        ymeantraj <- bind_cols(age=rep(age, k), y=ymeantraj)
                                        ymeantraj$class <- as.factor(ymeantraj$class)
                    }

                    if(shape=="quadratic"){
                                        f <- ests[k:((k-1)+k*s)]
                                        # arrange as 1 row per class
                                        f <- matrix(f, ncol=k, nrow=s)
                                        for(i in 1:k) ymeantraj[[i]] <- f[i,1] + f[i,2]*age + f[i,3]*(age^2)
                                        ymeantrajmat <- sapply(ymeantraj, cbind)
                                        colnames(ymeantrajmat) <- paste("Class", 1:k)
                                        ymeantraj <- tidyr::gather(as.data.frame(ymeantrajmat), key="class", value=y)
                                        ymeantraj <- bind_cols(age=rep(age, k), y=ymeantraj)
                                        ymeantraj$class <- as.factor(ymeantraj$class)
                    }

                    if(shape=="splines"){

                                        preds <- predictY(m, newdata = newdat, var.time = "AGEFUP_C")

                                        predy <- preds$pred
                                        colnames(predy) <- paste0("Class", 1:k)
                                        ymeantraj <- tidyr::gather(as.data.frame(predy), key="class", value=y)
                                        ymeantraj <- bind_cols(age=c(rep(unlist(preds$times), k)), y=ymeantraj)



                    }


                    g <- ggplot(ymeantraj, aes(x=age, y=y, colour=cols, group=class))
                    g <- g + geom_line(aes(colour=class), size=1)
                    g <- g + xlab("Age (years)")
                    g <- g + ylab (expression(paste("BMI (kg/",m^2,")", sep="")))
                    g <- g + ylim(ylimit)
                    g <- g + scale_x_continuous(breaks=c(0, 1.7, 3.5, 4.7), labels=c(18, 35, 50, 65))
                    g
                    return(g)

}
