#' Kappa matrix
#'
#' Kappa matrix of cohen's kappa values
#'
#' @param x  Either a two by n data with categorical values from 1 to p or a p x p table. If a data array, a table will be found.
#' @return  Unweighted kappa
#'
#' @examples
#' require(psych)
#' data(bmi_long, package="LCTMtools") # NO EXAMPLE SUPPLIED
#' @export


cohen.kappa <- function(x, w = NULL, n.obs = NULL, alpha = 0.05, levels = NULL) {
  # THIS FUNCTION IS TAKEN FROM 'psych' R Package
  # Revelle, W. (2018) psych: Procedures for Personality and Psychological Research,
  # Northwestern University, Evanston, Illinois, USA,
  # https://CRAN.R-project.org/package=psych Version = 1.8.4.
  #
  cl <- match.call()
  p <- dim(x)[1]
  len <- p
  bad <- FALSE
  if ((dim(x)[2] == p) || (dim(x)[2] < 3)) {
    result <- cohen.kappa1(x,
      w = w, n.obs = n.obs, alpha = alpha,
      levels = levels
    )
  }
  else {
    nvar <- dim(x)[2]
    ck <- matrix(NA, nvar, nvar)
    if (!is.null(colnames(x))) {
      colnames(ck) <- rownames(ck) <- colnames(x)
    }
    else {
      colnames(ck) <- rownames(ck) <- paste("R", 1:nvar,
        sep = ""
      )
    }
    diag(ck) <- 1
    result <- list(cohen.kappa = ck)
    k <- 2
    for (i in 2:nvar) {
      for (j in 1:(i - 1)) {
        x1 <- data.frame(x[, i], x[, j])
        x1 <- na.omit(x1)
        ck1 <- cohen.kappa1(x1,
          w = w, n.obs = n.obs,
          alpha = alpha, levels = levels
        )
        result[[paste(colnames(ck)[j], rownames(ck)[i])]] <- ck1
        if (ck1$bad) {
          warning(
            "No variance detected in cells ", i,
            "  ", j
          )
          bad <- TRUE
        }
        ck[i, j] <- result[[k]]$kappa
        ck[j, i] <- result[[k]]$weighted.kappa
        k <- k + 1
      }
    }
    result[[1]] <- ck
    av.kappa <- mean(ck[lower.tri(ck)], na.rm = TRUE)
    av.wt <- mean(ck[upper.tri(ck)], na.rm = TRUE)
    result$av.kappa <- av.kappa
    result$av.wt <- av.wt
  }
  if (bad) {
    message("At least one item had no variance.  Try describe(your.data) to find the problem.")
  }
  class(result) <- c("psych", "kappa")
  return(result)
}
