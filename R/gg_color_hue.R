#' gg_color_hue
#'
#' Emulate ggplot2 default colour palette
#'
#' @param n Number of coloures
#' @return a vector of n colour codes
#' @references https://www.rdocumentation.org/packages/iprior/versions/0.7.1/topics/gg_colour_hue
#' @examples
#' gg_color_hue(2)
#' gg_color_hue(4)
#' plot(1:10, pch=12, col=gg_color_hue(10), lwd=30, xaxt="n", yaxt="n", ylab="", xlab="")
#' @export
#'
gg_color_hue <- function(n) {
                    hues = seq(15, 375, length = n + 1)
                    hcl(h = hues, l = 65, c = 100)[1:n]
}
