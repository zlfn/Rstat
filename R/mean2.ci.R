#' @title CI for the Difference of Two Means


#' @description Confidence Interval for the Difference of Two Population Means




#' @usage mean2.ci(xb1, xb2, s1, s2, n1, n2, pvar = "equal", alp = 0.05, dig = 4)

#' @param xb1 Sample mean of population1 (or sample data)
#' @param xb2 Sample mean of population2 (or sample data)
#' @param s1 Standard deviation of population1 (optional for unknown variance)
#' @param s2 Standard deviation of population2 (optional for unknown variance)
#' @param n1 Sample size of population1 (unnecessary if data are given)
#' @param n2 Sample size of population2 (unnecessary if data are given)
#' @param pvar Status of variance (one of "known", "equal", "unequal"), Default: 'equal'
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'

#'
#' @return None.


#' @examples
#' mean2.ci(xb1 = 198.5, xb2 = 201.3, s1 = 5, s2 = 5, n1 = 25, n2 = 34, pvar = "known")
#' mean2.ci(xb1 = 198.5, xb2 = 201.3, s1 = 4.8, s2 = 5.1, n1 = 25, n2 = 34, pvar = "equal")
#' mean2.ci(xb1 = 198.5, xb2 = 201.3, s1 = 2.8, s2 = 5.5, n1 = 25, n2 = 34, pvar = "unequal")
#'
#' x <- rnorm(20, 199, 2)
#' y <- rnorm(25, 200, 2)
#' mean2.ci(x, y, pvar = "equal")
#' @export

mean2.ci <- function(xb1, xb2, s1, s2, n1, n2, pvar = "equal", alp = 0.05,
                     dig = 4) {
  if (length(xb1) > 1) {
    indata <- TRUE
    n1 <- length(xb1)
    n2 <- length(xb2)
    sig1 <- sd(xb1)
    sig2 <- sd(xb2)
    xb1 <- mean(xb1)
    xb2 <- mean(xb2)
    cat(
      "xb1 =", round(xb1, dig), "\t xb2 =",
      round(xb2, dig), "\n"
    )
    cat(
      "std1 =", round(sig1, dig), "\t std2 =",
      round(sig2, dig), "\n"
    )
  }
  else {
    indata <- FALSE
  }
  if (grepl(pvar, "known")) {
    err <- qnorm(1 - alp / 2) * sqrt(s1^2 / n1 + s2^2 / n2)
    xd <- xb1 - xb2
    cat(paste0(
      "[(", round(xb1, dig), " - ",
      round(xb2, dig), ") ± ", round(
        qnorm(1 - alp / 2),
        dig
      ), " × √(", round(s1^2, dig), "/",
      n1, " + ", round(s2^2, dig), "/", n2,
      ")]\n = [", round(xd, dig), " ± ", round(
        err,
        dig
      ), "] = [", round(xd - err, dig), ", ",
      round(xd + err, dig), "]"
    ), "\n")
  }
  else if (grepl(pvar, "equal")) {
    if (indata) {
      s1 <- sig1
      s2 <- sig2
    }
    df <- n1 + n2 - 2
    sp2 <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / df
    sp <- sqrt(sp2)
    err <- qt(1 - alp / 2, df) * sp * sqrt(1 / n1 + 1 / n2)
    xd <- xb1 - xb2
    cat(
      "Sp² =", round(sp2, dig), "\t Sp =",
      round(sp, dig), "\t df =", df, "\n"
    )
    cat(paste0(
      "[(", round(xb1, dig), " - ",
      round(xb2, dig), ") ± ", round(qt(
        1 - alp / 2,
        df
      ), dig), " × ", round(sp, dig), " × √(",
      "1/", n1, "+1/", n2, ")]\n = [",
      round(xd, dig), " ± ", round(err, dig), "] = [",
      round(xd - err, dig), ", ", round(
        xd + err,
        dig
      ), "]"
    ), "\n")
  }
  else {
    if (indata) {
      s1 <- sig1
      s2 <- sig2
    }
    df <- (s1^2 / n1 + s2^2 / n2)^2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 -
      1))
    tdf <- qt(1 - alp / 2, df)
    err <- tdf * sqrt(s1^2 / n1 + s2^2 / n2)
    xd <- xb1 - xb2
    cat(
      "nu* =", round(df, dig), "\t t(nu*) =",
      round(tdf, dig), "\n"
    )
    cat(paste0(
      "[(", round(xb1, dig), " - ",
      round(xb2, dig), ") ± ", round(tdf, dig),
      " × √(", round(s1^2, dig), "/", n1,
      " + ", round(s2^2, dig), "/", n2, ")]\n = [",
      round(xd, dig), " ± ", round(err, dig), "] = [",
      round(xd - err, dig), ", ", round(
        xd + err,
        dig
      ), "]"
    ), "\n")
  }
}
