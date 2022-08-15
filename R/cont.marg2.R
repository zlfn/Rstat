#' @title Marginal PDF Plot of Two Continuous Random Variables
#' @description Marginal Probability Distribution Plot of Two Continuous Random Variables
#' @usage cont.marg2(FUN, lo1, up1, lo2, up2, xs, ys)
#' @param FUN Continuous joint PDF function
#' @param lo1 Lower limit of x-axis
#' @param up1 Upper limit of x-axis
#' @param lo2 Lower limit of y-axis
#' @param up2 Upper limit of y-axis
#' @param xs Specific value of X for displaying the probability density
#' @param ys Specific value of Y for displaying the probability density
#'
#' @return Marginal PDF
#' @examples
#' pdf <- function(x, y) 2 / 7 * (2 * x + 5 * y) * (x >= 0 & x <= 1) * (y >= 0 & y <= 1)
#' cont.marg2(pdf, -0.2, 1.2, -0.2, 1.2, 0:2 / 2, 0:2 / 2)
#' @export
cont.marg2 <- function(FUN, lo1, up1, lo2, up2, xs, ys) {
  fx <- function(x) {
    integrate(function(y) {
      sapply(y, function(y) FUN(x, y))
    }, lo2, up2)$value
  }
  fy <- function(y) {
    integrate(function(x) {
      sapply(x, function(x) FUN(x, y))
    }, lo1, up1)$value
  }
  Vfx <- Vectorize(fx, "x")
  Vfy <- Vectorize(fy, "y")
  xa <- seq(lo1, up1, length = 500)
  ya <- seq(lo2, up2, length = 500)
  dev.new(7, 6)
  par(mfrow = c(2, 1))
  par(mar = c(3, 4, 4, 2))
  plot(xa, Vfx(xa),
    type = "l", main = "Marginal Probability Density Function of X",
    ylim = c(0, max(Vfx(xa)) * 1.1), xlab = "", ylab = "f(x)",
    lwd = 3, col = 2
  )
  if (!missing(xs)) {
    fxv <- Vfx(xs)
    segments(lo1 + 0.07 * (up1 - lo1), fxv, xs, fxv,
      lty = 2,
      col = 4
    )
    text(rep(lo1, length(xs)), fxv, round(fxv, 4), col = 4)
  }
  plot(ya, Vfy(ya),
    type = "l", main = "Marginal Probability Density Function of Y",
    pch = 16, lwd = 3, ylim = c(0, max(Vfy(ya)) * 1.1), xlab = "",
    ylab = "f(y)", col = 2
  )
  if (!missing(ys)) {
    fyv <- Vfy(ys)
    segments(lo2 + 0.07 * (up2 - lo2), fyv, ys, fyv,
      lty = 2,
      col = 4
    )
    text(rep(lo2, length(ys)), fyv, round(fyv, 4), col = 4)
  }
  invisible(list(fx = Vfx(xa), fy = Vfy(ya)))
}
