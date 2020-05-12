#' @title CDF of a Continuous Random Variable
#' @description Cumulative Distribution Function of a Continuous Random Variable
#' @usage cont.cdf(FUN, low, up, xs, mt, dig = 4, pos = "bottomright")
#' @param FUN Probability density function
#' @param low Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param xs Specific values of X for displaying the probability
#' @param mt Graph Title of the CDF
#' @param dig Number of digits below the decimal point, Default: 4
#' @param pos Legend location, Default: 'bottomright'
#'
#' @return None.
#' @examples
#' pdf <- function(x) 2 * exp(-2 * x) * (x > 0)
#' cont.cdf(pdf, low = -1, up = 3, xs = c((1:5) * 0.2, 2))
#' @export
cont.cdf <- function(FUN, low, up, xs, mt, dig = 4, pos = "bottomright") {
  if (missing(mt)) {
    mt <- "Continuous Cumulative Distribution Function(CDF)"
  }
  Fx <- function(x) integrate(FUN, low, x)$value
  VFx <- Vectorize(Fx, "x")
  xrange <- seq(low, up, length = 100)
  win.graph(7, 5)
  plot(xrange, VFx(xrange),
    type = "l", lwd = 3, main = mt,
    col = 2, xlab = "x", ylab = "F(x)"
  )
  grid(col = 3)
  abline(h = 0)
  if (!missing(xs)) {
    n <- length(xs)
    lp <- low + (up - low) * 0.2
    segments(lp, VFx(xs), xs, VFx(xs), lty = 2, col = 4)
    segments(xs, 0, xs, VFx(xs), lty = 2, col = 4)
    text(rep(low, n), VFx(xs),
      labels = paste0(
        "F(",
        xs, ")=", round(VFx(xs), dig)
      ), cex = 0.8,
      col = 1, pos = 4
    )
  }
  xfx <- function(x) x * FUN(x)
  x2fx <- function(x) x^2 * FUN(x)
  Ex <- integrate(xfx, low, Inf)$value
  Ex2 <- integrate(x2fx, low, Inf)$value
  Vx <- Ex2 - Ex^2
  Dx <- sqrt(Vx)
  legend(pos, c(paste("E(X) =", round(Ex, dig)), paste(
    "D(X) =",
    round(Dx, dig)
  )), bg = "white")
}
