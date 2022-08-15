#' @title Cumulative Probability of the F-distribution

#' @description Cumulative Probability of the F-distribution
#' @usage f.prob(nu1 = 5, nu2 = 5, xp, mt, dig = 4)

#' @param nu1 Numerator degree of freedom, Default: 5
#' @param nu2 Denominator degree of freedom, Default: 5
#' @param xp Vector of x-axis values
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' k <- 1:7
#' nu1 <- 8
#' nu2 <- 5
#' f.prob(nu1, nu2, k)
#' @export
f.prob <- function(nu1 = 5, nu2 = 5, xp, mt, dig = 4) {
  if (missing(mt)) {
    mt <- bquote("Cumulative Probabilities" ~ F(x) ==
      P(F[.(nu1) ~ "," ~ .(nu2)] < x))
  }
  up <- qf(0.99, nu1, nu2)
  x <- seq(0, up, length = 100)
  ymax <- max(df(x, nu1, nu2))
  nxp <- length(xp)
  prx <- pf(xp, nu1, nu2)
  names(prx) <- xp
  print(round(prx, dig))
  y1 <- 0.1 * nxp * ymax
  wc <- ifelse(nxp > 5, 6, 5)
  dev.new(7, wc)
  plot(x, df(x, nu1, nu2), type = "n", main = mt, ylim = c(
    -y1,
    ymax
  ), xlim = c(0, up), ylab = "f(x)", xlab = "x")
  abline(h = 0, col = "green4")
  lines(x, df(x, nu1, nu2), type = "l", lwd = 2, col = 2)
  abline(v = 0, col = "green4")
  abline(v = xp, lty = 2, col = 4)
  yp <- -0.1 * ymax * (1:nxp)
  arrows(0, yp, xp, yp, length = 0.1, code = 2, col = 4)
  text(xp, yp, labels = paste0(
    "F(", xp, ")=",
    round(prx, dig)
  ), pos = 4, cex = 0.8)
}
