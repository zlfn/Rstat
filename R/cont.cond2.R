#' @title Conditional PDF Plot of Two Continuous Random Variables
#' @description Conditional Probability Distribution Plot of Two Continuous Random Variables
#' @usage cont.cond2(FUN, xc, yc, xs, ys, lo, up)
#' @param FUN Continuous joint PDF function
#' @param xc Conditioning value of X
#' @param yc Conditioning value of Y
#' @param xs Specific value of X for displaying the density (given yc)
#' @param ys Specific value of Y for displaying the density (given xc)
#' @param lo Lower limit of the conditioned random variable
#' @param up Upper limit of the conditioned random variable
#'
#' @return Conditional PDF
#' @examples
#' pdf <- function(x, y) (x + y) * (x >= 0 & x <= 1) * (y >= 0 & y <= 1)
#' cont.cond2(pdf, yc = 0.1, xs = 0:2 / 2, lo = -0.2, up = 1.2)
#' @export
cont.cond2 <- function(FUN, xc, yc, xs, ys, lo, up) {
  fx <- function(x) {
    integrate(function(y) {
      sapply(y, function(y) FUN(x, y))
    }, lo, up)$value
  }
  fy <- function(y) {
    integrate(function(x) {
      sapply(x, function(x) FUN(x, y))
    }, lo, up)$value
  }
  Vfx <- Vectorize(fx, "x")
  Vfy <- Vectorize(fy, "y")
  Vfxy <- Vectorize(FUN, c("x", "y"))
  if (missing(xc)) {
    cpdf <- function(d) Vfxy(d, yc) / fy(yc)
    Cs <- yc
    Cn <- "Y"
    Dn <- "X"
    yla <- "f(x|y)"
  }
  else {
    cpdf <- function(d) Vfxy(xc, d) / fx(xc)
    Cs <- xc
    Cn <- "X"
    Dn <- "Y"
    yla <- "f(y|x)"
  }
  da <- seq(lo, up, length = 500)
  dev.new(7, 4)
  par(mar = c(3, 4, 4, 2))
  plot(da, cpdf(da), type = "l", main = paste0(
    "Conditional PDF of ",
    Dn, " | ", Cn, "=", Cs
  ), ylim = c(0, max(cpdf(da)) *
    1.1), xlab = "", ylab = yla, lwd = 3, col = 2)
  if (!missing(xs)) {
    fxv <- cpdf(xs)
    segments(lo + 0.07 * (up - lo), fxv, xs, fxv,
      lty = 2,
      col = 4
    )
    text(rep(lo, length(xs)), fxv, round(fxv, 3), col = 4)
  }
  if (!missing(ys)) {
    fyv <- cpdf(ys)
    segments(lo + 0.07 * (up - lo), fyv, ys, fyv,
      lty = 2,
      col = 4
    )
    text(rep(lo, length(ys)), fyv, round(fyv, 3), col = 4)
  }
  invisible(cpdf(da))
}
