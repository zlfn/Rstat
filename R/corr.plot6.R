#' @title Six Scatter Plots
#' @description Create 6 types of scatter plots.
#' @usage corr.plot6(m1 = 60, s1 = 10, m2 = 60, s2 = 10, r = 0.7, r2 = 0.8, n = 50)
#' @param m1 Mean 1. Default: 60
#' @param s1 Standard deviation 1. Default: 10
#' @param m2 Mean 2. Default: 60
#' @param s2 Standard deviation 2. Default: 10
#' @param r Correlation Coefficient 1. Default: 0.7
#' @param r2 Correlation Coefficient 2. Default: 0.8
#' @param n Number of samples. Default: 50
#'
#'
#'
#' @return None.
#' @examples
#' Scorr.plot6()
#' corr.plot6(r = 0.6, r2 = 0.9, n = 100)
#' @export
corr.plot6 <- function(m1 = 60, s1 = 10, m2 = 60, s2 = 10, r = 0.7, r2 = 0.8,
                       n = 50) {
  x1 <- floor(m1 - 3 * s1)
  x2 <- ceiling(m1 + 3 * s1)
  y1 <- floor(m2 - 3 * s2)
  y2 <- ceiling(m2 + 3 * s2)
  xa <- seq(m1 - 2.5 * s1, m1 + 2.5 * s1, length = n)
  set.seed(9857)
  d1 <- rbivariate(m1, s1, m2, s2, r, n)
  d2 <- rbivariate(m1, s1, m2, s2, -r, n)
  d3 <- list(xa, (m2 + 2 * s2) - 0.05 * (xa - m1)^2 + rnorm(
    n,
    0, s2 * 0.6
  ))
  d4 <- list(rnorm(n, m1, s1), rnorm(n, m2, s2))
  d81 <- rbivariate(m1, s1, m2 - 1.5 * s2, s2, r, n / 2)
  d82 <- rbivariate(m1, s1, m2 + 1.5 * s2, s2, -r2, n / 2)
  d8 <- list(c(d81[[1]], d82[[1]]), c(d81[[2]], d82[[2]]))
  win.graph(9, 6)
  par(mfrow = c(2, 3))
  plot(d1[[1]], d1[[2]],
    pch = 19, cex = 1.2, xlab = "(a) Positive Correlation",
    cex.lab = 1.5, ylab = "", xlim = c(x1, x2), ylim = c(
      y1,
      y2
    )
  )
  abline(lm(d1[[2]] ~ d1[[1]]), lwd = 2, lty = 2, col = 2)
  plot(d2[[1]], d2[[2]],
    pch = 19, cex = 1.2, xlab = "(b) Negative Correlation",
    cex.lab = 1.5, ylab = "", xlim = c(x1, x2), ylim = c(
      y1,
      y2
    )
  )
  abline(lm(d2[[2]] ~ d2[[1]]), lwd = 2, lty = 2, col = 2)
  plot(d4[[1]], d4[[2]],
    pch = 19, cex = 1.2, xlab = "(c) Little Correlation",
    cex.lab = 1.5, ylab = "", xlim = c(x1, x2), ylim = c(
      y1,
      y2
    )
  )
  abline(lm(d4[[2]] ~ d4[[1]]), lwd = 2, lty = 2, col = 2)
  plot(d3[[1]], d3[[2]],
    pch = 19, cex = 1.2, xlab = "(d) Quadratic Relation",
    cex.lab = 1.5, ylab = "", xlim = c(x1, x2), ylim = c(
      y1,
      y2
    )
  )
  abline(lm(d3[[2]] ~ d3[[1]]), lwd = 2, lty = 2, col = 2)
  o1 <- c(d1[[1]][1:(n - 2)], m1 - 2 * s1, m1 + 2 * s1)
  o2 <- c(d1[[2]][1:(n - 2)], m2 + 2 * s2, m2 - 2 * s2)
  plot(o1, o2,
    pch = 19, cex = 1.2, xlab = "(e) Outlier",
    cex.lab = 1.5, ylab = "", xlim = c(x1, x2), ylim = c(
      y1,
      y2
    )
  )
  points(c(m1 - 2 * s1, m1 + 2 * s1), c(m2 + 2 * s2, m2 - 2 *
    s2), pch = 0, cex = 2, col = 2)
  abline(lm(o2 ~ o1), lwd = 2, lty = 2, col = 2)
  plot(d8[[1]], d8[[2]],
    pch = 19, cex = 1.2, xlab = "(f) Stratification",
    cex.lab = 1.5, ylab = "", xlim = c(x1, x2), ylim = c(
      y1,
      y2
    )
  )
  abline(lm(d8[[2]] ~ d8[[1]]), lwd = 2, lty = 2, col = 2)
  abline(lm(d81[[2]] ~ d81[[1]]), lwd = 2, lty = 2, col = 4)
  abline(lm(d82[[2]] ~ d82[[1]]), lwd = 2, lty = 2, col = 4)
}
