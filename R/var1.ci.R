#' @title Confidence Interval for a Variance
#' @description Confidence Interval for a Population Variance
#' @usage var1.ci(x, alp = 0.05, dig = 4)
#' @param x Data vector
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' x <- c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' var1.ci(x, dig = 3)
#' var1.ci(rnorm(36))
#' @export
var1.ci <- function(x, alp = 0.05, dig = 4) {
  n <- length(x)
  xss <- sum(x^2) - sum(x)^2 / n
  xv <- var(x)
  cv1 <- qchisq(alp / 2, n - 1)
  cv2 <- qchisq(1 - alp / 2, n - 1)
  cat(paste0((1 - alp) * 100, "% CI = [", round(
    xss,
    dig
  ), " / ", round(cv2, dig), ", ", round(
    xss,
    dig
  ), " / ", round(cv1, dig), "] = [", round(
    xss / cv2,
    dig
  ), ", ", round(xss / cv1, dig), "]"), "\n")
  invisible(list(var = xv, conf = xss / c(cv2, cv1)))
}
