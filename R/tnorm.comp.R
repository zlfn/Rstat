#' @title Compare T-Dist. with the Standard Normal

#' @description Compare T-Dist. with the Standard Normal


#' @usage tnorm.comp(nu = c(10, 30), lo = -3.5, up = 3.5, dig = 4, dcol)

#' @param nu Degree of freedom for the chi-sq. dist. Default: c(10, 30)
#' @param lo Lower limit of x-axis, Default: -3.5
#' @param up Upper limit of x-axis, Default: 3.5
#' @param dig Number of digits below the decimal point, Default: 4
#' @param dcol Color of plot lines
#'
#'
#' @return None.
#' @examples
#' nu <- c(1, 5, 10, 30)
#' tnorm.comp(nu)
#' @export

tnorm.comp <- function(nu = c(10, 30), lo = -3.5, up = 3.5, dig = 4, dcol) {
  x <- seq(lo, up, length = 100)
  x1 <- lo * 1.1
  x2 <- up * 1.1
  if (missing(dcol)) {
    dcol <- c(
      1, 2, 4, "green2", "purple", "pink",
      "cyan", "orange"
    )
  }
  win.graph(7, 6)
  par(mfrow = c(2, 1))
  par(mar = c(4, 4, 4, 2))
  plot(x, dnorm(x),
    type = "l", main = "N(0,1) & T-dist.",
    lwd = 2, xlim = c(x1, x2), ylab = "f(x)", xlab = "(a)"
  )
  abline(v = 0, lty = 2, col = 3)
  for (i in 1:4) {
    lines(x, dt(x, nu[i]), lwd = 1, col = dcol[i +
      1])
  }
  legend("topright", c("N(0,1)", paste0(
    "t(",
    nu, ")"
  )), lwd = 2, cex = 0.8, col = dcol)
  plot(x, dnorm(x),
    type = "l", log = "y", main = "N(0,1) & T-dist. (Log scale)",
    lwd = 2, xlim = c(x1, x2), ylab = "log[f(x)]",
    xlab = "(b)"
  )
  abline(v = 0, lty = 2, col = 3)
  for (i in 1:4) {
    lines(x, dt(x, nu[i]), lwd = 1, col = dcol[i +
      1])
  }
}
