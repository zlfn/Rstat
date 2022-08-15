#' @title Cumulative Probability of the Chi-square Distribution
#' @description Cumulative Probability of the Chi-square Distribution
#' @usage chi.prob(nu, xp, pup = 0.995, mt, dig = 4)
#' @param nu Degree of freedom of the chi-square distribution
#' @param xp Vector of specific x-axis values
#' @param pup Upper limit of probabilities for quantiles, Default: 0.995
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' k <- 1:10
#' nu <- 5
#' chi.prob(nu, k)
#' @export

chi.prob <- function(nu, xp, pup = 0.995, mt, dig = 4) {
  if (missing(mt)) {
    mt <- bquote("Cumulative Probabilities " ~ F(x) ==
      P(chi[.(nu)]^2 < x))
  }
  up <- qchisq(pup, nu)
  x <- seq(0, up, length = 100)
  ymax <- max(dchisq(x, nu))
  nxp <- length(xp)
  prx <- pchisq(xp, nu)
  names(prx) <- xp
  print(round(prx, dig))
  y1 <- 0.1 * nxp * ymax
  wc <- ifelse(nxp > 5, 6, 5)
  dev.new(7, wc)
  plot(x, dchisq(x, nu), type = "n", main = mt, ylim = c(
    -y1,
    ymax
  ), xlim = c(0, up), ylab = "f(x)", xlab = "x")
  abline(h = 0, col = "green4")
  lines(x, dchisq(x, nu), type = "l", lwd = 2, col = 2)
  abline(v = c(0, nu), col = 3)
  abline(v = xp, lty = 2, col = 4)
  yp <- -0.1 * ymax * (1:nxp)
  arrows(0, yp, xp, yp, length = 0.1, code = 2, col = 4)
  text(xp, yp, labels = paste0(
    "F(", xp, ")=",
    round(prx, dig)
  ), pos = 4, cex = 0.8)
}
