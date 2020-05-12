#' @title Expected Values of Continuous Random Variables
#' @description Expected Values of Continuous Random Variables
#' @usage cont.exp(FUN, lo, up, mt, dig = 3, xn = "X", prt = FALSE, plot = FALSE, pos = "center")
#' @param FUN Continuous probability density function
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 3
#' @param xn Random variable name, Default: 'X'
#' @param prt Print expected values and variances? Default: FALSE
#' @param plot Plot probability density function? Default: FALSE
#' @param pos Legend location, Default: 'center'
#'
#'
#' @return list(ev=Ex, std=Dx)
#' @examples
#' fx = function(x) dnorm(x,10,2)
#' cont.exp(fx, 0, 20, prt=T, plot=T)
#' @export
cont.exp <- function (FUN, lo, up, mt, dig = 3, xn = "X", prt = FALSE,
                      plot = FALSE, pos = "center")
{
  Xn = toupper(xn)
  if (missing(mt))
    mt = paste0("PDF and Expected Value of ", Xn)
  ex = function(x) x * FUN(x)
  ex2 = function(x) x^2 * FUN(x)
  Ex = integrate(ex, lo, up)[[1]]
  Ex2 = integrate(ex2, lo, up)[[1]]
  Vx = Ex2 - Ex^2
  Dx = sqrt(Vx)
  if (prt == TRUE) {
    cat(paste0("E(", Xn, ") = ", round(Ex, dig)),
        "\t ")
    cat(paste0("V(", Xn, ") = ", round(Vx, dig)),
        "\t ")
    cat(paste0("D(", Xn, ") = ", round(Dx, dig)),
        "\n")
  }
  if (plot == TRUE) {
    xa = seq(lo, up, length = 200)
    plot(xa, FUN(xa), type = "l", main = mt, ylim = c(0,
                                                      max(FUN(xa)) * 1.1), xlab = "", ylab = paste0("f(",
                                                                                                    xn, ")"), lwd = 2, col = 2)
    abline(v = Ex, lty = 2, col = 4)
    legend(pos, c(paste0("E(", Xn, ")=", round(Ex,
                                               dig)), paste0("D(", Xn, ")=", round(Dx,
                                                                                   dig))), bg = "white")
  }
  invisible(list(ev = Ex, std = Dx))
}
