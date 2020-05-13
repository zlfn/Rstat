#' @title Confidence Interval for a Mean (known variance)

#' @description Confidence Interval for a Population Mean (known variance)


#' @usage pmean.ci(xb, sig, n, alp = 0.05, dig = 4)

#' @param xb Sample mean, or sample data
#' @param sig Population standard deviation
#' @param n Sample size (unnecessary when sample data are given)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' pmean.ci(xb = 199.5, sig = 5, n = 50)
#' pmean.ci(xb = 199.5, sig = 5, n = 50, dig = 3)
#' pmean.ci(rnorm(100, 20, 3), sig = 3)
#' @export
pmean.ci <- function(xb, sig, n, alp = 0.05, dig = 4) {
  if (length(xb) > 1) {
    n <- length(xb)
    xb <- mean(xb)
  }
  err <- qnorm(1 - alp / 2) * sig / sqrt(n)
  cat(paste0(
    "[", round(xb, dig), " ± ", round(qnorm(1 -
      alp / 2), dig), "×", round(sig, dig), "/√",
    n, "] = [", round(xb, dig), " ± ", round(
      err,
      dig
    ), "] = [", round(xb - err, dig), ", ",
    round(xb + err, dig), "]"
  ), "\n")
}
