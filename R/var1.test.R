#' @title Hypothesis Test for a Variance

#' @description Hypothesis Test for a Population Variance


#' @usage var1.test(x, n, var0, alp = 0.05, side = "two", dig = 4)

#' @param x Data vector (or sample variance)
#' @param n Sample size (necessary when the sample variance is given)
#' @param var0 Population variance value under the null hypothesis
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' var1.test(x = 1.24, n = 25, var0 = 0.8, side = "up")
#' x <- c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' var1.test(x = x, var0 = 2, side = "two")
#' @export
var1.test <-
  function(x, n, var0, alp = 0.05, side = "two", dig = 4) {
    if (length(x) > 1) {
      n <- length(x)
      xss <- sum(x^2) - sum(x)^2 / n
      svar <- var(x)
      cat(paste0("n = ", n, "\tSxx = ", round(
        xss,
        dig
      ), "\tVar(X) = ", round(svar, dig)), "\n")
    }
    else {
      svar <- x
      xss <- (n - 1) * svar
    }
    nside <- grep(side, c("less", "greater", "two.sided"))
    if (length(nside) == 0) {
      nside <- grep(side, c("low", "upp", "two-sided"))
    }
    chi0 <- (n - 1) * svar / var0
    pv <- switch(nside, pchisq(chi0, n - 1), pchisq(chi0, n -
      1, lower = F), 2 * min(pchisq(chi0, n - 1), pchisq(chi0,
      n - 1,
      lower = F
    )))
    cat(paste0(
      "Chi0 = ", n - 1, " × ", round(
        svar,
        dig
      ), " / ", var0, " = ", round(chi0, dig),
      "\t P-v = ", round(pv, dig)
    ), "\n")
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
    invisible(list(var = svar, stat = chi0, df = n - 1, conf = xss / c(
      cv2,
      cv1
    )))
  }
