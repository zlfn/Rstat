#' @title Simulate the Confidence Interval for a Mean
#' @description Simulate the Confidence Interval for a Population Mean
#' @usage cimean.sim(n, mu = 0, sig = 1, alp = 0.05, N = 100, seed = 9857, dig = 4, plot = TRUE)
#'
#' @param n Sample size
#' @param mu Population mean value, Default: 0
#' @param sig Population standard deviation, Default: 1
#' @param alp Level of significance, Default: 0.05
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot confidence intervals? Default: TRUE
#' @param n Sample size, Default: 100
#'
#' @return None.
#' @examples
#' cimean.sim(n = 16, mu = 10, sig = 2)
#' cimean.sim(n = 16, mu = 10, sig = 2, N = 10000, plot = FALSE)
#' @export
cimean.sim <- function(n, mu = 0, sig = 1, alp = 0.05, N = 100, seed = 9857,
                       dig = 4, plot = TRUE) {
  ci <- matrix(0, nrow = N, ncol = 3)
  ir <- 1:N
  tv <- qt(1 - alp / 2, n - 1)
  set.seed(seed)
  for (i in ir) {
    x <- rnorm(n, mu, sig)
    xm <- mean(x)
    xs <- sd(x)
    lcl <- xm - tv * xs / sqrt(n)
    ucl <- xm + tv * xs / sqrt(n)
    ci[i, ] <- c(lcl, xm, ucl)
  }
  if (plot) {
    win.graph(7, 4)
    plot(ir, ci[, 2],
      type = "p", pch = 19, cex = 0.6,
      col = 1, ylim = c(min(ci), max(ci)), main = "Confidence Intervals for a Population Mean",
      ylab = "Confidence Interval", xlab = "Iteration"
    )
    abline(h = mu, col = 2)
    arrows(ir, ci[, 1], ir, ci[, 3],
      length = 0.03, code = 3,
      angle = 90, lwd = 1.5, col = ifelse((ci[, 1] > mu |
        ci[, 3] < mu), 2, 4)
    )
  }
  nup <- sum(ci[, 1] > mu)
  nlow <- sum(ci[, 3] < mu)
  cat(paste0(
    "P(LCL > ", mu, ") = ", nup, " / ",
    N, " = ", nup / N, "\t P(UCL < ", mu, ") = ",
    nlow, " / ", N, " = ", nlow / N
  ), "\n")
}
