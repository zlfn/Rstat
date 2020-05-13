#' @title Confidence Interval for the Difference of Ratios

#' @description Confidence Interval for the Difference of Population Ratios


#' @usage prob2.ci(n1, x1, n2, x2, alp = 0.05, dig = 4)

#' @param n1 Sample size of population1
#' @param x1 Number of successes in samples from population1
#' @param n2 Sample size of population2
#' @param x2 Number of successes in samples from population2
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'


#'
#' @return None.
#' @examples
#' prob2.ci(n1 = 160, x1 = 12, n2 = 200, x2 = 13, alp = 0.1)
#' prob2.ci(n1 = 160, x1 = 12, n2 = 200, x2 = 13, alp = 0.05)
#' @export
prob2.ci <- function(n1, x1, n2, x2, alp = 0.05, dig = 4) {
  p1 <- x1 / n1
  p2 <- x2 / n2
  pd <- p1 - p2
  err <- qnorm(1 - alp / 2) * sqrt(p1 * (1 - p1) / n1 + p2 * (1 -
    p2) / n2)
  cat(paste0(
    "[(", round(p1, dig), " - ", round(
      p2,
      dig
    ), ") ± ", round(qnorm(1 - alp / 2), dig), " × √(",
    round(p1, dig), " × ", round(1 - p1, dig), "/",
    n1, " + ", round(p2, dig), " × ", round(1 -
      p2, dig), "/", n2, ")]\n = [", round(
      pd,
      dig
    ), " ± ", round(err, dig), "] = [",
    round(pd - err, dig), ", ", round(pd + err, dig),
    "]"
  ), "\n")
}
