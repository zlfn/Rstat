#' @title Simulate the Confidence Interval for a Variance
#' @description Simulate the Confidence Interval for a Variance
#' @usage civar.sim(n, mu = 0, sig = 1, alp = 0.05, N = 100, seed = 9857, dig = 4, plot = TRUE)
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
#' civar.sim(n = 16, mu = 10, sig = 2)
#' civar.sim(n = 16, mu = 10, sig = 2, N = 10000, plot = FALSE)
#' @export

civar.sim <- function(n, mu = 0, sig = 1, alp = 0.05, N = 100, seed = 9857,
                      dig = 4, plot = TRUE) {
  ci <- matrix(0, nrow = N, ncol = 3)
  ir <- 1:N
  cv1 <- qchisq(alp / 2, n - 1)
  cv2 <- qchisq(1 - alp / 2, n - 1)
  set.seed(seed)
  for (i in ir) {
    x <- rnorm(n, mu, sig)
    xm <- var(x)
    xss <- xm * (n - 1)
    lcl <- xss / cv2
    ucl <- xss / cv1
    ci[i, ] <- c(lcl, xm, ucl)
  }
  if (plot) {
    dev.new(7, 4)
    plot(ir, ci[, 2],
      type = "p", pch = 19, cex = 0.6,
      col = 1, ylim = c(min(ci), max(ci)), main = "Confidence Intervals for a Population Variance",
      ylab = "Confidence Interval", xlab = "Iteration"
    )
    abline(h = sig^2, col = 2)
    arrows(ir, ci[, 1], ir, ci[, 3],
      length = 0.03, code = 3,
      angle = 90, lwd = 1.5, col = ifelse((ci[, 1] > sig^2 |
        ci[, 3] < sig^2), 2, 4)
    )
  }
  nup <- sum(ci[, 1] > sig^2)
  nlow <- sum(ci[, 3] < sig^2)
  cat(paste0(
    "P(LCL > ", sig^2, ") = ", nup, " / ",
    N, " = ", nup / N, "\t P(UCL < ", sig^2, ") = ",
    nlow, " / ", N, " = ", nlow / N
  ), "\n")
}
