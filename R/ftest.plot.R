#' @title Plot the PDF of the F-test Statistic
#' @description Plot the PDF of the F-test Statistic


#' @usage ftest.plot(fstat, deg, pmax = 0.995, side = "two")

#' @param fstat F-test statistic for the ratio of two population variances
#' @param deg Vector of degree of freedoms
#' @param pmax Maximum probability for quantiles in x-axis, Default: 0.995
#' @param side Type of the alternative hypothesis, Default: 'two'
#'
#'
#' @return None.
#' @examples
#' vo <- var.test(rnorm(20, 10, 2.4), rnorm(25, 12, 1.8))
#' ftest.plot(fstat = vo$stat, deg = as.vector(vo$para), pmax = 0.9999, side = "two")
#' @export


ftest.plot <- function(fstat, deg, pmax = 0.995, side = "two") {
  prng <- c(0, qf(pmax, deg[1], deg[2]))
  xa <- seq(prng[1], prng[2], length.out = 101)
  plot(xa, df(xa, deg[1], deg[2]),
    type = "n", xlab = "F-statistic",
    ylab = "pdf", ylim = c(-0.1, 1) * max(df(
      xa, deg[1],
      deg[2]
    )), main = paste0(
      "Distribution of F-statistic under H0: F(",
      deg[1], ", ", deg[2], ")"
    )
  )
  plow <- pf(fstat, deg[1], deg[2])
  if (side == "up" | grepl(side, "greater")) {
    pv <- 1 - plow
    cat("P-v =", pv, "\n")
    cord.x <- c(
      fstat, seq(fstat, prng[2], length.out = 20),
      prng[2]
    )
    cord.y <- c(0, df(
      seq(fstat, prng[2], length.out = 20),
      deg[1], deg[2]
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(fstat, 0, fstat, df(fstat, deg[1], deg[2]),
      lwd = 2, col = 2
    )
    text(fstat, df(fstat, deg[1], deg[2]) * 0.9, round(
      pv,
      4
    ), pos = 4, col = 2)
    text(fstat, 0, round(fstat, 4), pos = 1, col = 4)
  }
  else if (side == "low" | grepl(side, "less")) {
    pv <- plow
    cat("P-v =", pv, "\n")
    cord.x <- c(
      prng[1], seq(prng[1], fstat, length.out = 20),
      fstat
    )
    cord.y <- c(0, df(
      seq(prng[1], fstat, length.out = 20),
      deg[1], deg[2]
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(fstat, 0, fstat, df(fstat, deg[1], deg[2]),
      lwd = 2, col = 2
    )
    text(fstat, df(fstat, deg[1], deg[2]) * 0.9, round(
      pv,
      4
    ), pos = 2, col = 2)
    text(fstat, 0, round(fstat, 4), pos = 1, col = 4)
  }
  else if (grepl(side, "two sided")) {
    pv <- 2 * min(plow, 1 - plow)
    cat("P-v =", pv, "\n")
    mlow <- qf(pv / 2, deg[1], deg[2])
    mup <- qf(1 - pv / 2, deg[1], deg[2])
    cord.x <- c(mup, seq(mup, prng[2], length.out = 20), prng[2])
    cord.y <- c(0, df(
      seq(mup, prng[2], length.out = 20),
      deg[1], deg[2]
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    cord.x <- c(
      prng[1], seq(prng[1], mlow, length.out = 20),
      mlow
    )
    cord.y <- c(0, df(
      seq(prng[1], mlow, length.out = 20),
      deg[1], deg[2]
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(c(mlow, mup), 0, c(mlow, mup), df(
      c(mlow), deg[1],
      deg[2]
    ), lwd = 2, col = 2)
    text(c(mlow, mup), df(c(mlow), deg[1], deg[2]), round(
      pv / 2,
      4
    ), pos = c(2, 4), col = 2)
    text(c(mlow, mup), 0, round(c(mlow, mup), 4),
      pos = 1,
      col = 4
    )
  }
  abline(h = 0)
  abline(v = qf(0.5, deg[1], deg[2]), lty = 2, lwd = 2, col = "green3")
  lines(xa, df(xa, deg[1], deg[2]),
    type = "l", lwd = 2,
    col = 4
  )
}
