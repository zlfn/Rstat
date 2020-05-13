#' @title Confidence Interval for the Mean (unknown variance)
#' @description Confidence Interval for the Population Mean (unknown variance)
#' @usage pmean.ci2(xb, sig, n, alp = 0.05, dig = 4)
#' @param xb Sample mean, or sample data
#' @param sig Sample standard deviation (unnecessary when sample data are given)
#' @param n Sample size (unnecessary when sample data are given)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' pmean.ci2(xb = 199.5, sig = 5, n = 16, alp = 0.05, dig = 3)
#' pmean.ci2(rnorm(20, 199.5, 5))
#' @export
pmean.ci2 <- function(xb, sig, n, alp = 0.05, dig = 4) {
  if (length(xb) > 1) {
    n <- length(xb)
    sig <- sd(xb)
    xb <- mean(xb)
  }
  err <- qt(1 - alp / 2, n - 1) * sig / sqrt(n)
  cat(paste0(
    "[", xb, " ± ", round(qt(
      1 - alp / 2,
      n - 1
    ), dig), "×", round(sig, dig), "/√",
    n, "] = [", xb, " ± ", round(err, dig),
    "] = [", round(xb - err, dig), ", ", round(xb +
      err, dig), "]"
  ), "\n")
}
