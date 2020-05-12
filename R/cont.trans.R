#' @title Transformed PDF of a Continuous Random Variable
#' @description Transformed PDF of a Continuous Random Variable
#' @usage cont.trans(fx, TF, FTF, a, b, lo = 0, up = 1, plot = FALSE, ...)
#' @param fx PDF of the original random variable
#' @param TF List of transform functions (1~8)
#' @param FTF List of transformed PDF (1~8)
#' @param a Lower limit of the original random variable for calculating P(a<X<b)
#' @param b Upper limit of the original random variable for calculating P(a<X<b)
#' @param lo Lower limit of the original random variable, Default: 0
#' @param up Upper limit of the original random variable, Default: 1
#' @param plot Plot the PDF? Default: FALSE
#' @param ... Graphic parameters
#'
#'
#'
#' @return None.
#' @examples
#' fx <- function(x) 2 * x * (x >= 0 & x <= 1)
#' ty <- function(x) 10 * x - 4
#' tw <- function(x) -10 * x + 4
#' fy <- function(y) fx((y + 4) / 10) / 10
#' fw <- function(w) fx((-w + 4) / 10) / 10
#' cont.trans(fx, list(y = ty, w = tw), list(fy, fw), 0.3, 0.7, plot = TRUE, cex = 1.3)
#' @export
cont.trans <- function(fx, TF, FTF, a, b, lo = 0, up = 1, plot = FALSE, ...) {
  N <- length(FTF)
  vn <- names(TF)
  Vn <- toupper(vn)
  px <- integrate(fx, a, b)[[1]]
  cat(
    paste0("Pr(", a, " < X < ", b, ") ="),
    px, "\n"
  )
  py <- rep(NA, N)
  for (k in 1:N) {
    a2 <- min(TF[[k]](c(a, b)))
    b2 <- max(TF[[k]](c(a, b)))
    py[k] <- integrate(FTF[[k]], a2, b2)[[1]]
    cat(paste0(
      "Pr(", a2, " < ", Vn[k], " < ",
      b2, ") ="
    ), py[k], "\n")
  }
  if (plot == TRUE) {
    divw <- switch(N, c(2, 1), c(3, 1), c(2, 2), c(2, 3),
      c(2, 3), c(3, 3), c(3, 3), c(3, 3)
    )
    dimw <- switch(N, c(7, 5), c(7, 7), c(8, 5), c(8, 7),
      c(8, 7), c(9, 7), c(9, 7), c(9, 7)
    )
    win.graph(dimw[1], dimw[2])
    par(mfrow = divw)
    x1 <- lo - 0.2 * (up - lo)
    x2 <- up + 0.2 * (up - lo)
    k1 <- x1 * 200
    k2 <- x2 * 200
    xm <- (a + b) / 2
    ym <- fx(xm) * 0.5
    xa <- k1:k2 / 200
    plot(xa, fx(xa),
      type = "n", las = 1, ylab = "f(x)",
      xlab = "", main = "pdf of X"
    )
    cord.x <- c(a, seq(a, b, 0.01), b)
    cord.y <- c(0, fx(seq(a, b, 0.01)), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    text(xm, ym, labels = paste0(
      "P(", a, "<X<",
      b, ")\n=", round(px, 4)
    ), ...)
    lines(xa, fx(xa), lwd = 2, col = 2)
    for (k in 1:N) {
      a2 <- min(TF[[k]](c(a, b)))
      b2 <- max(TF[[k]](c(a, b)))
      lo2 <- min(TF[[k]](c(lo, up)))
      up2 <- max(TF[[k]](c(lo, up)))
      x1 <- lo2 - 0.2 * (up2 - lo2)
      x2 <- up2 + 0.2 * (up2 - lo2)
      k1 <- x1 * 200
      k2 <- x2 * 200
      xm <- (a2 + b2) / 2
      ym <- FTF[[k]](xm) * 0.5
      xa <- k1:k2 / 200
      plot(xa, FTF[[k]](xa),
        type = "n", las = 1,
        ylab = paste0("f(", vn[k], ")"),
        xlab = "", main = paste0(
          "pdf of ",
          Vn[k]
        )
      )
      cord.x <- c(a2, seq(a2, b2, 0.01), b2)
      cord.y <- c(0, FTF[[k]](seq(a2, b2, 0.01)), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      text(xm, ym, labels = paste0(
        "P(", a2, "<",
        Vn[k], "<", b2, ")\n=", round(
          py[k],
          4
        )
      ), ...)
      lines(xa, FTF[[k]](xa), lwd = 2, col = 2)
    }
  }
}
