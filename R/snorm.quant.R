#' @title Quantile Plot of the Standard Normal Distribution

#' @description Quantile Plot of the Standard Normal Distribution


#' @usage snorm.quant(pv, pv2, mt, dig = 4)

#' @param pv Vector of probability values
#' @param pv2 Vector of specific probability values
#' @param mt Graph title (default="Quantiles of the Standard Normal Distribution")
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#'
#' @return None.
#' @examples
#' pv <- c(0.005, 0.01, 0.025, 0.05, 1:9 / 10, 0.95, 0.975, 0.99, 0.995)
#' snorm.quant(pv, pv)
#' @export

snorm.quant <-
  function(pv, pv2, mt, dig = 4) {
    if (missing(mt)) {
      mt <- "Quantiles of the Standard Normal Distribution"
    }
    zv <- qnorm(pv)
    names(zv) <- pv
    print(round(zv, dig))
    zv2 <- qnorm(pv2)
    x1 <- min(-3, qnorm(min(pv)))
    x2 <- max(3, qnorm(max(pv)))
    x <- seq(x1, x2, length = 100)
    ymax <- max(dnorm(x))
    nzp <- min(4, ceiling(length(pv2) / 4))
    y1 <- 0.1 * nzp * ymax
    win.graph(7, 5)
    plot(x, dnorm(x),
      type = "n", main = mt, ylim = c(
        -y1,
        ymax * 1.2
      ), xlim = c(x1, x2), ylab = bquote(phi(z)),
      xlab = "z"
    )
    abline(h = 0, col = "green4")
    lines(x, dnorm(x), type = "l", lwd = 2, col = 2)
    fzv2 <- format(zv2, digits = dig)
    yp <- -0.08 * ymax * (1:nzp)
    yp2 <- ymax * 1.2 + yp
    segments(zv2, yp, zv2, yp2, lty = 2, col = 4)
    text(zv2, yp2, pv2, cex = 0.8, pos = 3, col = 2)
    text(zv2, yp, fzv2, pos = 1, cex = 0.8)
  }
