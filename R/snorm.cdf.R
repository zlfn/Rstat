#' @title Plot Standard Normal Cumulative Probability
#' @description Plot Standard Normal Cumulative Probability P(Z<z)
#' @usage snorm.cdf(zp, lo = -4, up = 4, mt, dig = 4)
#' @param zp Vector of z-axis values (default=-2:2)
#' @param lo Lower limit of z-axis, Default: -4
#' @param up Upper limit of z-axis, Default: 4
#' @param mt Graph title, Default: 'Cumulative Probabilities of the Standard Normal Distribution'
#' @param dig Number of digits below the decimal point (default=4), Default: 4
#'
#' @return None.
#' @examples
#' zp <- seq(-2, 2, by = 0.5)
#' snorm.cdf(zp)
#' @export

snorm.cdf <-
  function(zp, lo = -4, up = 4, mt, dig = 4) {
    if (missing(mt)) {
      mt <- "Cumulative Probabilities of the Standard Normal Distribution"
    }
    x <- seq(lo, up, length = 100)
    lo1 <- lo - 0.12 * (up - lo)
    lo2 <- (lo1 * 2 + lo) / 3
    win.graph(7, 6)
    plot(x, pnorm(x), type = "n", main = mt, ylim = c(
      -0.05,
      1
    ), xlim = c(lo1, up), ylab = bquote(Phi(z)), xlab = "z")
    abline(h = 0, col = "green2")
    lines(x, pnorm(x), type = "l", lwd = 2, col = 2)
    if (missing(zp)) {
      zp <- -2:2
    }
    yp <- pnorm(zp)
    segments(zp, 0, zp, yp, lty = 2, col = 4)
    segments(zp, yp, lo, yp, lty = 2, col = 4)
    text(zp, 0, labels = zp, pos = 1, cex = 0.8)
    text(lo2, yp, labels = format(yp, digits = dig), cex = 0.8)
  }
