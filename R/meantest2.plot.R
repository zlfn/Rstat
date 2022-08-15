#' @title Hypothesis Test for a Mean (unknown variance)

#' @description Hypothesis Test for a Population Mean (unknown variance)


#' @usage meantest2.plot(xb, mu0, sig, n, prng = c(-4, 4), side = "two", mt, dig = 4)
#'
#' @param xb Sample mean, or sample data
#' @param mu0 Population mean value under the null hypothesis
#' @param sig Sample standard deviation (unnecessary when sample data are given)
#' @param n Sample size (unnecessary when sample data are given)
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' meantest2.plot(xb = 12.65, mu0 = 12.5, sig = 0.57, n = 40, side = "up")
#' meantest2.plot(rnorm(40, 12.65, 0.57), side = "two")
#' @export
meantest2.plot.R <- function(xb, mu0, sig, n, prng = c(-4, 4), side = "two",
                             mt, dig = 4) {
  if (length(xb) > 1) {
    n <- length(xb)
    sig <- sd(xb)
    xb <- mean(xb)
  }
  se <- sig / sqrt(n)
  t0 <- (xb - mu0) / se
  cat(paste0(
    "T0 = (", round(xb, dig), " - ", round(
      mu0,
      dig
    ), ") / (", round(sig, dig), "/√", n,
    ") = ", round(t0, dig)
  ), "\n")
  df <- n - 1
  if (missing(mt)) {
    mt <- paste0(
      "Dist. of the Test Statistic under H0 ~ t(",
      round(df, 3), ")"
    )
  }
  xa <- seq(prng[1], prng[2], length.out = 101)
  dev.new(7, 5)
  plot(xa, dt(xa, df),
    type = "n", xlab = "Test Statistic",
    ylab = "pdf", ylim = c(-0.1, 1) * max(dt(xa, df)),
    main = mt
  )
  if (side == "up") {
    pv <- pt(t0, df, lower.tail = FALSE)
    cat("P-v = P(T > T0) =", round(pv, dig), "\n")
    cord.x <- c(t0, seq(t0, prng[2], length.out = 20), prng[2])
    cord.y <- c(
      0, dt(seq(t0, prng[2], length.out = 20), df),
      0
    )
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(t0, 0, t0, dt(t0, df), lwd = 2, col = 2)
    text(t0, dt(t0, df) * 0.9, round(pv, dig), pos = 4, col = 2)
    text(t0, 0, round(t0, dig), pos = 1, col = 4)
  }
  else if (side == "low") {
    pv <- pt(t0, df)
    cat("P-v = P(T < T0) =", round(pv, dig), "\n")
    cord.x <- c(
      prng[1], seq(prng[1], t0, length.out = 20),
      t0
    )
    cord.y <- c(
      0, dt(seq(prng[1], t0, length.out = 20), df),
      0
    )
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(t0, 0, t0, dt(t0, df), lwd = 2, col = 2)
    text(t0, dt(t0, df) * 0.9, round(pv, dig), pos = 2, col = 2)
    text(t0, 0, round(t0, dig), pos = 1, col = 4)
  }
  else {
    mlow <- ifelse(t0 > 0, -t0, t0)
    mup <- ifelse(t0 > 0, t0, -t0)
    pv <- 2 * pt(mlow, df)
    cat("P-v = 2×P(T > |T0|) =", round(pv, dig), "\n")
    cord.x <- c(mup, seq(mup, prng[2], length.out = 20), prng[2])
    cord.y <- c(0, dt(
      seq(mup, prng[2], length.out = 20),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    cord.x <- c(
      prng[1], seq(prng[1], mlow, length.out = 20),
      mlow
    )
    cord.y <- c(0, dt(
      seq(prng[1], mlow, length.out = 20),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(c(mlow, mup), 0, c(mlow, mup), dt(t0, df),
      lwd = 2,
      col = 2
    )
    text(c(mlow, mup), dt(t0, df) * 0.9, round(pv / 2, dig),
      pos = c(2, 4), col = 2
    )
    text(c(mlow, mup), 0, round(c(mlow, mup), dig),
      pos = 1,
      col = 4
    )
  }
  abline(h = 0)
  abline(v = 0, lty = 2, lwd = 2, col = "green3")
  lines(xa, dt(xa, df), type = "l", lwd = 2, col = 4)
}
