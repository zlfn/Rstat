#' @title Simulate the Confidence Interval for a Ratio
#' @description Simulate the Confidence Interval for a Ratio
#' @usage ciprob.sim(n, p = 0.5, alp = 0.05, N = 100, seed = 9857, dig = 4, plot = TRUE)
#'
#' @param n Sample size
#' @param p Population ratio value, Default: 0
#' @param alp Level of significance, Default: 0.05
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot confidence intervals? Default: TRUE
#' @param n Sample size, Default: 100
#'
#' @return None.
#' @examples
#' ciprob.sim(n = 16, p = 0.6, alp = 0.05, N = 100)
#' ciprob.sim(n = 16, p = 0.6, alp = 0.05, N = 10000, plot = FALSE)
#' @export

ciprob.sim <- function(n, p = 0.5, alp = 0.05, N = 100, seed = 9857, dig = 4,
                       plot = TRUE) {
  ir <- 1:N
  zv <- qnorm(1 - alp / 2)
  set.seed(seed)
  xm <- rbinom(N, n, p)
  xp <- xm / n
  xv <- xp * (1 - xp) / n
  lcl <- pmax(0, xp - zv * sqrt(xv))
  ucl <- pmin(1, xp + zv * sqrt(xv))
  ci <- cbind(lcl, xp, ucl)
  if (plot) {
    win.graph(7, 4)
    plot(ir, ci[, 2],
      type = "p", pch = 19, cex = 0.6,
      col = 1, ylim = c(min(ci), max(ci)), main = "Confidence Intervals for a Population Ratio",
      ylab = "Confidence Interval", xlab = "Iteration"
    )
    abline(h = p, col = 2)
    arrows(ir, ci[, 1], ir, ci[, 3],
      length = 0.03, code = 3,
      angle = 90, lwd = 1.5, col = ifelse((ci[, 1] > p |
        ci[, 3] < p), 2, 4)
    )
  }
  nup <- sum(ci[, 1] > p)
  nlow <- sum(ci[, 3] < p)
  cat(paste0(
    "P(LCL > ", p, ") = ", nup, " / ",
    N, " = ", nup / N, "\t P(UCL < ", p, ") = ",
    nlow, " / ", N, " = ", nlow / N
  ), "\n")
}
