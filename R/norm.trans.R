#' @title Standardization of the Normal Distribution
#' @description Check Probability Conservation in Standardizing the Normal Distribution
#' @usage norm.trans(mu, sig, a, b, mt1, mt0, dig = 4, span = 3, np = 100)
#' @param mu Mean of the normal distribution
#' @param sig Standard deviation of the normal distribution
#' @param a Lower limit of X for calculating probability P(a<X<b)
#' @param b Upper limit of X for calculating probability P(a<X<b)
#' @param mt1 Title of the normal distribution probability plot
#' @param mt0 Title of the standard normal distribution probability plot
#' @param dig Number of digits below the decimal point, Default: 4
#' @param span Range of x-axis in (mu-span*sig, mu+span*sig), Default: 3
#' @param np Number of plotting points, Default: 100
#'
#'
#' @return None.
#' @examples
#' norm.trans(2, 2, -1, 4)
#' @export

norm.trans <- function(mu, sig, a, b, mt1, mt0, dig = 4, span = 3, np = 100) {
  px <- pnorm(b, mu, sig) - pnorm(a, mu, sig)
  cat(
    paste0("Pr(", a, " < X < ", b, ") = "),
    px, "\n"
  )
  c <- (a - mu) / sig
  d <- (b - mu) / sig
  pz <- pnorm(d) - pnorm(c)
  cat(paste0("Pr(", round(c, dig), " < Z < ", round(
    d,
    dig
  ), ") = "), pz, "\n")
  lo <- mu - span * sig
  up <- mu + span * sig
  x1 <- seq(lo, up, length = np)
  x0 <- seq(lo - mu, up - mu, length = np)
  fx <- matrix(c(dnorm(x0, 0, 1), dnorm(x1, mu, sig)),
    ncol = 2,
    byrow = F
  )
  ymax <- max(fx)
  if (missing(mt1)) {
    mt1 <- bquote(N(mu == .(mu), sigma^2 == .(sig^2)))
  }
  if (missing(mt0)) {
    mt0 <- bquote(N(mu == 0, sigma^2 == 1))
  }
  win.graph(7, 6)
  par(mfrow = c(2, 1))
  par(mar = c(3, 4, 3, 1))
  plot(x1, fx[, 2], type = "n", main = mt1, ylim = c(
    0,
    ymax
  ), ylab = "f(x)", xlab = "")
  cord.x <- c(a, seq(a, b, 0.01), b)
  cord.y <- c(0, dnorm(seq(a, b, 0.01), mu, sig), 0)
  polygon(cord.x, cord.y, col = "lightcyan")
  ab <- (a + b) / 2
  text(ab, 0.4 * dnorm(ab, mu, sig), labels = paste0(
    "P(",
    a, "<X<", b, ")\n=", round(px, dig)
  ))
  lines(x1, fx[, 2], lwd = 2, col = 2)
  plot(x0, fx[, 1], type = "n", main = mt0, ylim = c(
    0,
    ymax
  ), ylab = "f(x)", xlab = "")
  cord.x <- c(c, seq(c, d, 0.01), d)
  cord.y <- c(0, dnorm(seq(c, d, 0.01)), 0)
  polygon(cord.x, cord.y, col = "lightcyan")
  cd <- (c + d) / 2
  text(cd, 0.4 * dnorm(cd), labels = paste0("P(", round(
    c,
    dig
  ), "<Z<", round(d, dig), ")\n=", round(
    pz,
    dig
  )))
  lines(x0, fx[, 1], lwd = 2, col = 2)
}
