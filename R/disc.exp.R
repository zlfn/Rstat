#' @title Expected Value of a Discrete Random Variable
#' @description Expected Value of a Discrete Random Variable
#' @usage disc.exp(xv, xf, mt, dig = 3, prt = TRUE, plot = FALSE, pos = "topright")
#'
#' @param xv Vector of random variable values
#' @param xf Vector of probability (or frequency) distribution
#' @param mt graph title
#' @param dig Number of digits below the decimal point (default=3)
#' @param prt Print detailed output? Default: FALSE
#' @param plot Plot the PDF? Default: FALSE
#' @param pos Legend location, Default: 'topright'
#'
#'
#'
#' @return list(Ex=expected value, Dx=standard deviation, Vx=variance)
#' @examples
#' p <- c(1, 3, 3, 1)
#' x <- c(-3, -1, 1, 3) * 100
#' disc.exp(x, p)
#' @export

disc.exp <- function(xv, xf, mt, dig = 3, prt = TRUE, plot = FALSE, pos = "topright") {
  Xn <- toupper(deparse(substitute(xv)))
  N <- sum(xf)
  xp <- xf / N
  ex <- sum(xv * xp)
  ex2 <- sum(xv^2 * xp)
  vx <- ex2 - ex^2
  dx <- sqrt(vx)
  if (prt) {
    if (N < 1.01) {
      cat(paste0("E(", Xn, ") = ", round(
        ex,
        dig
      )), "\n")
      cat(paste0(
        "V(", Xn, ") = ", round(
          ex2,
          dig
        ), " - ", round(abs(ex), dig), "² = ",
        round(vx, dig)
      ), "\n")
    }
    else {
      cat(
        paste0("E(", Xn, ") = ", sum(xv *
          xf), "/", N, " = ", round(ex, dig)),
        "\n"
      )
      cat(paste0("V(", Xn, ") = ", sum(xv^2 *
        xf), "/", N, " - ", round(
        abs(ex),
        dig
      ), "² = ", round(vx, dig)), "\n")
    }
    cat(paste0("D(", Xn, ") = √(", round(
      vx,
      dig
    ), ") = ", round(dx, dig)), "\n")
  }
  if (plot) {
    if (missing(mt)) {
      mt <- paste("Probability Distribution of", Xn)
    }
    dev.new(7, 5)
    x1 <- min(xv)
    x2 <- max(xv)
    xr <- x2 - x1
    plot(xv, xp,
      type = "h", main = mt, pch = 19, lwd = 5,
      ylim = c(0, max(xp) * 1.1), xlim = c(
        x1 - 0.1 * xr,
        x2 + 0.1 * xr
      ), col = 2, xlab = "x", ylab = "f(x)"
    )
    grid(col = 3)
    text(xv, xp, round(xp, dig), pos = 3, col = 4, cex = 0.8)
    ym <- 0.5 * max(xp)
    segments(ex, 0, ex, ym, lty = 2, col = 4)
    text(ex, ym, paste0("E(", Xn, ")=", round(
      ex,
      dig
    )), pos = 3, col = 4)
    x1 <- ex - dx
    x2 <- ex + dx
    arrows(ex, ym, x2, ym, length = 0.1, lty = 2, col = 4)
    arrows(ex, ym, x1, ym, length = 0.1, lty = 2, col = 4)
    legend(pos, c(paste0("E(", Xn, ") = ", round(
      ex,
      dig
    )), paste0("D(", Xn, ") = ", round(
      dx,
      dig
    ))), bg = "white")
  }
  invisible(list(Ex = ex, Dx = dx, Vx = vx))
}
