#' @title Independence of Two Continuous Random Variables
#' @description Determine Independence of Two Continuous Random Variables
#' @usage cont.ind2(FUN, lo1, up1, lo2, up2, n = 11, ep = 1e-06, prt = FALSE)
#' @param FUN Continuous joint PDF
#' @param lo1 Lower limit of X
#' @param up1 Upper limit of X
#' @param lo2 Lower limit of Y
#' @param up2 Upper limit of Y
#' @param n Number of checking points between lower and upper limit, Default: 11
#' @param ep Error bound for comparing probablities, Default: 1e-06
#' @param prt Print detailed output? Default: FALSE
#' @return Joint PDF
#' @examples
#' pdf <- function(x, y) (x + y) * (x >= 0 & x <= 1) * (y >= 0 & y <= 1)
#' cont.ind2(pdf, lo1 = 0, up1 = 1, lo2 = 0, up2 = 1)
#' @export
cont.ind2 <- function(FUN, lo1, up1, lo2, up2, n = 11, ep = 1e-06, prt = FALSE) {
  fx <- function(x) {
    integrate(function(y) {
      sapply(y, function(y) FUN(x, y))
    }, -Inf, Inf)$value
  }
  fy <- function(y) {
    integrate(function(x) {
      sapply(x, function(x) FUN(x, y))
    }, -Inf, Inf)$value
  }
  Vfx <- Vectorize(fx, "x")
  Vfy <- Vectorize(fy, "y")
  Vfxy <- FUN
  xa <- seq(lo1, up1, length = n)
  ya <- seq(lo2, up2, length = n)
  xa2 <- rep(xa, n)
  ya2 <- rep(ya, each = n)
  jpdf <- matrix(Vfxy(xa2, ya2), n, n)
  ppdf <- Vfx(xa) %o% Vfy(ya)
  rownames(jpdf) <- rownames(ppdf) <- xa
  colnames(jpdf) <- colnames(ppdf) <- ya
  if (prt == TRUE) {
    cat("Joint probability density f(x,y) ---------\n")
    print(jpdf)
    cat("|f(x,y)-f(x)f(y)| ---------------------\n")
    print(abs(jpdf - ppdf))
  }
  err <- abs(max(jpdf - ppdf))
  errf <- format(err, scientific = TRUE, digits = 4)
  if (err < ep) {
    cat(
      "f(x,y)=f(x)f(y) ⇒ Independent. (Error bound =",
      errf, ")\n"
    )
  }
  else {
    cat("max|f(x,y)-f(x)f(y)| =", errf, "⇒ Not Independent.\n")
  }
  invisible(list(jpdf = jpdf, ppdf = ppdf))
}
