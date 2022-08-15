#' @title Hypothesis Test for a Mean (known variance)
#' @description Hypothesis Test for a Population Mean (known variance)
#' @usage meantest1.plot(xb, mu0, sig, n, prng, side = "two", mt, dig = 4, xlab = "Sample Mean")
#'
#' @param xb Sample mean, or sample data
#' @param mu0 Population mean value under the null hypothesis
#' @param sig Population standard deviation
#' @param n Sample size (unnecessary when sample data are given)
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param xlab Label of x-axis, Default: 'Sample Mean'
#'
#'
#'
#' @return None.
#' @examples
#' meantest1.plot(xb = 12.64, mu0 = 12.5, sig = 0.5, n = 40, side = "up")
#' meantest1.plot(rnorm(40, 12.6, 0.5), mu0 = 12.5, sig = 0.5, side = "up")
#' @export

meantest1.plot <- function(xb, mu0, sig, n, prng, side = "two", mt, dig = 4,
                           xlab = "Sample Mean") {
  if (length(xb) > 1) {
    n <- length(xb)
    xb <- mean(xb)
  }
  se <- sig / sqrt(n)
  z0 <- (xb - mu0) / se
  cat(paste0(
    "Z0 = (", round(xb, dig), " - ", round(
      mu0,
      dig
    ), ") / (", round(sig, dig), "/√", n,
    ") = ", round(z0, dig)
  ), "\n")
  if (missing(prng)) {
    prng <- c(mu0 - 4 * se, mu0 + 4 * se)
  }
  if (missing(mt)) {
    mt <- paste0(
      "Dist. of ", xlab, " under H0 ~ N(",
      mu0, ", ", round(se, 3), "²)"
    )
  }
  xa <- seq(prng[1], prng[2], length.out = 101)
  dev.new(7, 5)
  plot(xa, dnorm(xa, mu0, se),
    type = "n", xlab = xlab,
    ylab = "pdf", ylim = c(-0.1, 1) * max(dnorm(
      xa,
      mu0, se
    )), main = mt
  )
  if (side == "up") {
    pv <- pnorm(xb, mu0, se, lower.tail = FALSE)
    cat("P-v = P(Z > Z0) =", round(pv, dig), "\n")
    cord.x <- c(xb, seq(xb, prng[2], length.out = 20), prng[2])
    cord.y <- c(0, dnorm(
      seq(xb, prng[2], length.out = 20),
      mu0, se
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(xb, 0, xb, dnorm(xb, mu0, se), lwd = 2, col = 2)
    text(xb, dnorm(xb, mu0, se) * 0.9, round(pv, 4),
      pos = 4,
      col = 2
    )
    text(xb, 0, round(xb, 4), pos = 1, col = 4)
  }
  else if (side == "low") {
    pv <- pnorm(xb, mu0, se)
    cat("P-v = P(Z < Z0) =", round(pv, dig), "\n")
    cord.x <- c(
      prng[1], seq(prng[1], xb, length.out = 20),
      xb
    )
    cord.y <- c(0, dnorm(
      seq(prng[1], xb, length.out = 20),
      mu0, se
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(xb, 0, xb, dnorm(xb, mu0, se), lwd = 2, col = 2)
    text(xb, dnorm(xb, mu0, se) * 0.9, round(pv, 4),
      pos = 2,
      col = 2
    )
    text(xb, 0, round(xb, 4), pos = 1, col = 4)
  }
  else {
    mlow <- ifelse(xb > mu0, 2 * mu0 - xb, xb)
    mup <- ifelse(xb > mu0, xb, 2 * mu0 - xb)
    pv <- 2 * pnorm(mlow, mu0, se)
    cat("P-v = 2×P(Z > |Z0|) =", round(pv, dig), "\n")
    cord.x <- c(mup, seq(mup, prng[2], length.out = 20), prng[2])
    cord.y <- c(0, dnorm(
      seq(mup, prng[2], length.out = 20),
      mu0, se
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    cord.x <- c(
      prng[1], seq(prng[1], mlow, length.out = 20),
      mlow
    )
    cord.y <- c(0, dnorm(
      seq(prng[1], mlow, length.out = 20),
      mu0, se
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(c(mlow, mup), 0, c(mlow, mup), dnorm(
      xb, mu0,
      se
    ), lwd = 2, col = 2)
    text(c(mlow, mup), dnorm(xb, mu0, se) * 0.9, round(
      pv / 2,
      4
    ), pos = c(2, 4), col = 2)
    text(c(mlow, mup), 0, round(c(mlow, mup), 4),
      pos = 1,
      col = 4
    )
  }
  abline(h = 0)
  abline(v = mu0, lty = 2, lwd = 2, col = "green3")
  lines(xa, dnorm(xa, mu0, se),
    type = "l", lwd = 2,
    col = 4
  )
}
