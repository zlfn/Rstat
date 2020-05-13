#' @title Diagnosis of Normality
#' @description Investigate the Normality of Data
#' @usage norm.diag(x, xrng, by = 1, dig = 4, dc = c("cyan", 2, 4))

#'
#' @param x Data vector
#' @param xrng Range of x-axis, Default: c(mean-3stdev, mean+3stdev)
#' @param by Histogram class interval, Default: 1
#' @param dig Number of digits below the decimal point, Default: 4
#' @param dc Color vector, Default: c("cyan", 2, 4)
#'
#'
#'
#' @return None.
#' @examples
#' (x <- c(1, 2, 5, 7, rep(8, 7), rep(9, 5), rep(10, 4)))
#' norm.diag(x)
#' @export

norm.diag <-
  function(x, xrng, by = 1, dig = 4, dc = c("cyan", 2, 4)) {
    xm <- mean(x)
    xs <- sd(x)
    if (missing(xrng)) {
      x1 <- min(x, xm - 3 * xs)
      x2 <- max(x, xm + 3 * xs)
      xrng <- c(x1, x2)
    }
    win.graph(8, 4)
    par(mfrow = c(1, 2))
    hist(x,
      prob = T, breaks = seq(xrng[1], xrng[2], by = by),
      col = dc[1]
    )
    lines(density(x), lwd = 2, col = dc[2])
    xax <- seq(xrng[1], xrng[2], length = 100)
    lines(xax, dnorm(xax, xm, xs), lwd = 2, col = dc[3])
    qqnorm(x,
      pch = 19, xlab = "Theoretical Quantile",
      ylab = "Sample Quantile"
    )
    grid(col = 3)
    qqline(x, col = dc[2])
    shap <- shapiro.test(x)
    cat("Normality Test (Shapiro-Wilk's Test) -----\n")
    cat(
      "Test Statistic =", round(shap$stat, dig), "\t P-value =",
      shap$p.val, "\n"
    )
  }
