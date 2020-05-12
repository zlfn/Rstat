#' @title Plot PDF of Continuous Random Variables
#' @description Plot (separately) the PDF of Continuous Random Variables
#' @usage cont.spdf(dist, lo, up, para, para2, ymax, xl, yl, dcol, np = 100, xp)
#' @param dist Distribution name ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param para First parameter vector of the distribution
#' @param para2 Second parameter vector (except "exp", "t", "chisq")
#' @param ymax Upper limit of y-axis
#' @param xl Label vector of x-axis
#' @param yl Label vector of y-axis
#' @param dcol Graph color vector
#' @param np Number of plot points, Default: 100
#' @param xp Location vector for vertical lines
#'
#'
#' @return None.
#' @examples
#' mu <- c(0, 0, 2, 2)
#' sig <- c(1, 2, 1, 2)
#' cont.spdf("norm", -7, 7, mu, sig, xp = mu)
#' @export
cont.spdf <- function(dist, lo, up, para, para2, ymax, xl, yl, dcol, np = 100,
                      xp) {
  N <- length(para)
  pn <- deparse(substitute(para))
  if (missing(para2)) {
    para2 <- para
  }
  else {
    pn2 <- deparse(substitute(para2))
    N2 <- length(para2)
    if (N == 1 & N2 > 1) {
      para <- rep(para, N2)
      pn <- deparse(substitute(para2))
    }
    if (N > 1 & N2 == 1) {
      para2 <- rep(para2, N)
    }
    N <- max(N, N2)
  }
  dlist <- c(
    "exp", "gamma", "weibull", "beta",
    "norm", "t", "chisq", "f"
  )
  if (missing(dist)) {
    cat(paste(dlist, collapse = ", "), "\n")
    stop("In put one of the distribution names above....")
  }
  distnum <- grep(dist, dlist)
  if (length(distnum) != 1) {
    stop("probability distribution name Error...")
  }
  dist <- dlist[distnum]
  lab <- list()
  for (k in 1:N) {
    lab[[k]] <- switch(distnum, bquote(Exp(lambda ==
      .(para[k]))), bquote(Gamma(alpha == .(para[k]), theta ==
      .(para2[k]))), bquote(Weib(alpha == .(para[k]), theta ==
      .(para2[k]))), bquote(Beta(alpha == .(para[k]), beta ==
      .(para2[k]))), bquote(N(mu == .(para[k]), sigma^2 ==
      .(para2[k]^2))), bquote(T(nu == .(para[k]))), bquote(chi^2 ~
    (nu == .(para[k]))), bquote(F(nu[1] == .(para[k]), nu[2] ==
      .(para2[k]))))
  }
  xa <- seq(lo, up, length = np)
  pdf <- getpdf(dist, xa, para, para2)
  if (missing(dcol)) {
    dcol <- rep(2, N)
  }
  if (missing(yl)) {
    yl <- rep("f(x)", N)
  }
  if (missing(xl)) {
    xl <- paste0("(", letters[1:N], ")")
  }
  nr <- switch(N, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  nc <- switch(N, 1, 2, 2, 2, 3, 3, 3, 3, 3)
  win.graph(3.5 * nc, 3 * nr)
  par(mfrow = c(nr, nc))
  if (missing(ymax)) {
    ymax <- max(pdf)
  }
  for (k in 1:N) {
    plot(xa, pdf[, k],
      type = "l", main = lab[[k]],
      xlim = c(lo, up), ylim = c(0, ymax), lwd = 2, col = dcol[k],
      ylab = yl[k], xlab = xl[k]
    )
    if (!missing(xp)) {
      abline(v = xp[k], lty = 2, col = 4)
    }
  }
}
