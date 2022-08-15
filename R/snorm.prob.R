#' @title Central Probability of the Standard Normal Distribution
#' @description Central Probability of the Standard Normal Distribution
#' @usage snorm.prob(zp, lo = -4, up = 4, mt, dig = 4)
#' @param zp Vector of z-axis values (default=1:4)
#' @param lo Lower limit of z-axis, Default: -4
#' @param up Upper limit of z-axis, Default: 4
#' @param mt Graph title (default="Central Probability of the Standard Normal Distribution")
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' zp <- 1:3
#' snorm.prob(zp)
#' @export

snorm.prob <-
  function(zp, lo = -4, up = 4, mt, dig = 4) {
    if (missing(mt)) {
      mt <- "Central Probability of the Standard Normal Distribution"
    }
    x <- seq(lo, up, length = 100)
    ymax <- max(dnorm(x))
    nzp <- length(zp)
    y1 <- 0.11 * nzp * ymax
    dev.new(7, 5)
    plot(x, dnorm(x), type = "n", main = mt, ylim = c(
      -y1,
      ymax
    ), xlim = c(lo, up), ylab = bquote(phi(z)), xlab = "z")
    abline(h = 0, col = "green4")
    lines(x, dnorm(x), type = "l", lwd = 2, col = 2)
    if (missing(zp)) {
      zp <- 1:4
    }
    prz <- pnorm(zp) - pnorm(-zp)
    abline(v = c(-zp, zp), lty = 2, col = 4)
    yp <- -0.11 * ymax * (1:nzp)
    arrows(-zp, yp, zp, yp, length = 0.1, code = 3, col = 4)
    text(0, yp,
      labels = paste0(
        "P(", -zp, "<Z<",
        zp, ")=", format(prz, digits = dig)
      ), pos = 3,
      cex = 0.8
    )
  }
