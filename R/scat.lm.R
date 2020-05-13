#' @title Scatter Plot with a Regression Line

#' @description To create a scatter plot with a linear regression line.


#' @usage scat.lm(x, y, mt, xl, yl, w = c(7, 5), ...)

#' @param x Vector of x variable.
#' @param y Vector of y variable.
#' @param mt Title of the plot.
#' @param xl Label of x-axis.
#' @param yl Label of y-axis.
#' @param w Size of graphic window. Default: c(7, 5)
#' @param ... Other graphic parameters.
#'
#' @return None.
#' @examples
#' scat.lm(mtcars$wt, mtcars$mpg, mt = "Weight vs. MPG")
#' @export

scat.lm <-
  function(x, y, mt, xl, yl, w = c(7, 5), ...) {
    if (missing(xl)) {
      xl <- deparse(substitute(x))
    }
    if (missing(yl)) {
      yl <- deparse(substitute(y))
    }
    if (missing(mt)) {
      mt <- paste(xl, ":", yl, "scatter plot")
    }
    win.graph(w[1], w[2])
    plot(x, y,
      main = mt, xlab = xl, ylab = yl, pch = 19, cex = 1.2,
      ...
    )
    grid(col = 3)
    sr <- lm(y ~ x)
    ssr <- summary(sr)
    abline(sr, lty = 2, lwd = 2, col = 2)
    b <- sr$coef[[2]]
    a <- sr$coef[[1]]
    sign <- ifelse(b < 0, "", "+")
    pos <- ifelse(b < 0, "topright", "topleft")
    pv <- 1 - pf(ssr$f[1], ssr$f[2], ssr$f[3])
    legend(pos,
      legend = c(paste(
        "Y =", round(a, 4), sign,
        round(b, 4), "X"
      ), paste("R-sq =", round(
        ssr$r.sq,
        4
      )), paste("P-v =", format(pv, digits = 3, scientific = T))),
      text.col = c(2, 4, 1), cex = 1
    )
  }
