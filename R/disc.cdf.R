#' @title CDF of a Discrete Random Variable
#' @description Cumulative Distribution Function of a Discrete Random Variable
#' @usage disc.cdf(xv, xp, mt, cpt = 1.2, cex = 1, dig = 3)
#'
#' @param xv Vector of values of the discrete random variable
#' @param xp Vector of the discrete PDF
#' @param mt Graph Title of the CDF
#' @param cpt Text size of the limit points, Default: 1.2
#' @param cex Text size of the probability, Default: 1
#' @param dig Number of digits below the decimal point, Default: 3
#'
#'
#' @return None.
#' @examples
#' disc.cdf(0:3, choose(3, 0:3))
#' @export

disc.cdf <- function(xv, xp, mt, cpt = 1.2, cex = 1, dig = 3) {
  xname <- deparse(substitute(xv))
  if (sum(xp) > 1) {
    xp <- xp / sum(xp)
  }
  xcdf <- c(0, cumsum(xp))
  sf <- stepfun(xv, xcdf)
  if (missing(mt)) {
    mt <- paste0(
      "Cumulative distribution function(CDF) of ",
      xname
    )
  }
  dev.new(7, 5)
  plot(sf,
    main = mt, verticals = F, pch = 19, lwd = 2, cex = 1.2,
    col = 2, xlab = "x", ylab = "F(x)"
  )
  grid(col = 3)
  points(xv, xcdf[-length(xcdf)], col = 2, cex = cpt)
  text(xv, xcdf[-1],
    labels = round(xcdf[-1], dig), cex = cex,
    col = 4, pos = 2
  )
  EX <- sum(xv * xp)
  EX2 <- sum(xv^2 * xp)
  VX <- EX2 - EX^2
  DX <- sqrt(VX)
  legend("bottomright", c(paste("E(X) =", round(
    EX,
    4
  )), paste("D(X) =", round(DX, 4))), bg = "white")
}
