#' @title Confidence Interval for a Population Ratio
#' @description Confidence Interval for a Population Ratio
#' @usage prob.ci(n, x, alp = 0.05, dig = 4)
#' @param n Sample size
#' @param x Number of successes in a sample
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'

#'
#'
#' @return The probability of an event.
#' @examples
#' prob.ci(n = 200, x = 15)
#' @export

prob.ci <- function(n, x, alp = 0.05, dig = 4) {
  p <- x / n
  err <- qnorm(1 - alp / 2) * sqrt(p * (1 - p) / n)
  cat(paste0(
    "[", p, " ± ", round(
      qnorm(1 - alp / 2),
      dig
    ), "×√(", round(p, dig), "×", round(1 -
      p, dig), "/", n, ")] = [", p, " ± ",
    round(err, dig), "] = [", round(p - err, dig),
    ", ", round(p + err, dig), "]"
  ), "\n")
}
