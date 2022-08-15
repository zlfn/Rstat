#' @title Simulate the Confidence Interval for a the ratio of Variances
#' @description Simulate the Confidence Interval for a the ratio of Variances
#' @usage civar2.sim(n1, n2, sig1, sig2, alp = 0.05, N = 100, seed = 9857, dig = 4, plot = TRUE)
#'
#' @param n1 Sample size of population1
#' @param n2 Sample size of population2
#' @param sig1 Standard deviation of population1
#' @param sig2 Standard deviation of population2
#' @param alp Level of significance, Default: 0.05
#' @param N Number of iterations, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Logical value for plot, Default: TRUE
#'
#' @return None.
#' @examples
#' civar2.sim(n1 = 25, n2 = 16, sig1 = sqrt(8), sig2 = 2)
#' civar2.sim(n1 = 25, n2 = 16, sig1 = sqrt(8), sig2 = 2, N = 10000, plot = F)
#' @export
civar2.sim <- function(n1, n2, sig1, sig2, alp = 0.05, N = 100, seed = 9857,
                       dig = 4, plot = TRUE) {
  vr0 <- sig1^2 / sig2^2
  ci <- matrix(0, nrow = N, ncol = 3)
  ir <- 1:N
  fv1 <- qf(alp / 2, n1 - 1, n2 - 1)
  fv2 <- qf(1 - alp / 2, n1 - 1, n2 - 1)
  set.seed(seed)
  for (i in ir) {
    x <- rnorm(n1, 0, sig1)
    y <- rnorm(n2, 0, sig2)
    xv <- var(x)
    yv <- var(y)
    xm <- xv / yv
    lcl <- xm / fv2
    ucl <- xm / fv1
    ci[i, ] <- c(lcl, xm, ucl)
  }
  if (plot) {
    dev.new(7, 4)
    plot(ir, ci[, 2],
      type = "p", pch = 19, cex = 0.6,
      col = 1, ylim = c(min(ci), max(ci)), main = "Confidence Intervals for Ratio of Population Variances",
      ylab = "Confidence Interval", xlab = "Iteration"
    )
    abline(h = vr0, col = 2)
    arrows(ir, ci[, 1], ir, ci[, 3],
      length = 0.03, code = 3,
      angle = 90, lwd = 1.5, col = ifelse((ci[, 1] > vr0 |
        ci[, 3] < vr0), 2, 4)
    )
  }
  nup <- sum(ci[, 1] > vr0)
  nlow <- sum(ci[, 3] < vr0)
  cat(paste0(
    "P(LCL > ", vr0, ") = ", nup, "/",
    N, " = ", nup / N, "\t P(UCL < ", vr0, ") = ",
    nlow, "/", N, " = ", nlow / N
  ), "\n")
}
