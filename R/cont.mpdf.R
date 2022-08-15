#' @title PDF and CDF for Continuous Random Variables
#' @description PDF and CDF for Continuous Random Variables
#' @usage cont.mpdf(dist, lo, up, para, para2, ymax, mt, dcol, np = 100, pos1 = "topright", pos2 = "bottomright", xp1, xp2)
#' @param dist Name of continuous probability distribution (one of the follows) ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param para First parameter vector of PDF
#' @param para2 Second parameter vector of PDF (if necessary)
#' @param ymax Upper limit of y-axis
#' @param mt Graph title
#' @param dcol Graph color vector (default as follows) c("red", "blue", "orange2", "green4", "purple", "cyan2")
#' @param np Number of plot points, Default: 100
#' @param pos1 Legend location of PDF, Default: 'topright'
#' @param pos2 Legend location of CDF, Default: 'bottomright'
#' @param xp1 Vector of specific x values for PDF (ignore legend)
#' @param xp2 Vector of specific x values for CDF (ignore legend)
#'
#' @return None.
#' @examples
#' lamb <- 1:5
#' cont.mpdf("exp", 0, 3, para = lamb, ymax = 5)
#'
#' alp <- c(0.5, 1, 2, 3)
#' rate <- 1
#' cont.mpdf("gamma", 0, 8, para = alp, para2 = rate, ymax = 1.2)
#'
#' th <- 1
#' alp <- c(0.5, 1, 2, 3)
#' cont.mpdf("weibull", 0, 5, para = alp, para2 = th, ymax = 1.2)
#' @export
cont.mpdf <- function(dist, lo, up, para, para2, ymax, mt, dcol, np = 100,
                      pos1 = "topright", pos2 = "bottomright", xp1,
                      xp2) {
  N <- length(para)
  pn <- deparse(substitute(para))
  vpar <- para
  if (missing(para2)) {
    para2 <- para
  }
  else {
    pn2 <- deparse(substitute(para2))
    N2 <- length(para2)
    if (N == 1 & N2 > 1) {
      para <- rep(para, N2)
      pn <- deparse(substitute(para2))
      vpar <- para2
    }
    if (N > 1 & N2 == 1) {
      para2 <- rep(para2, N)
    }
    N <- max(N, N2)
  }
  parnum <- grep(pn, c(
    "lambda", "alpha", "beta",
    "theta", "mu", "sigma", "nu"
  ))
  if (length(parnum) == 0) {
    stop("Error in Parameter Names ...")
  }
  lab <- list()
  for (k in 1:N) {
    lab[[k]] <- switch(parnum, bquote(lambda ==
      .(vpar[k])), bquote(alpha == .(vpar[k])), bquote(beta ==
      .(vpar[k])), bquote(theta == .(vpar[k])), bquote(mu ==
      .(vpar[k])), bquote(sigma == .(vpar[k])), bquote(nu ==
      .(vpar[k])))
  }
  leg <- lab[[1]]
  if (N >= 2) {
    for (i in 2:N) leg <- c(leg, lab[[i]])
  }
  dlist <- c(
    "exp", "gamma", "weibull", "beta",
    "norm", "t", "chisq", "f"
  )
  dname <- paste0(c(
    "Exponential", "Gamma", "Weibull",
    "Beta", "Normal", "T-", "Chi-square",
    "F-"
  ), " Dist.")
  if (missing(dist)) {
    cat(paste(dlist, collapse = ", "), "\n")
    stop("Input one of the distribution name above ....")
  }
  distname <- dname[which(dlist %in% dist)]
  mt1 <- paste0("PDF of ", distname)
  mt2 <- paste0("CDF of ", distname)
  xa <- seq(lo, up, length = np)
  dum <- getdf(dist, xa, para, para2)
  pdf <- dum$pdf
  cdf <- dum$cdf
  if (missing(dcol)) {
    dcol <- c(
      "red", "blue", "orange2",
      "green4", "purple", "cyan2"
    )
  }
  dev.new(9, 5)
  par(mfrow = c(1, 2))
  if (missing(ymax)) {
    ymax <- max(pdf)
  }
  plot(xa, pdf[, 1],
    type = "l", main = mt1, lwd = 2,
    col = dcol[1], ylab = "f(x)", xlab = "(a)",
    ylim = c(0, ymax)
  )
  grid(col = 3)
  if (N >= 2) {
    for (i in 2:N) lines(xa, pdf[, i], lwd = 2, col = dcol[i])
  }
  if (!missing(xp1)) {
    yp1 <- getdf2(dist, xp1, para, para2)$pdf
    text(xp1, yp1, paste0(
      "(", para, ",", para2,
      ")"
    ))
  }
  else {
    legend(pos1, sapply(leg, as.expression), lwd = 2, col = dcol[1:N])
  }
  plot(xa, cdf[, 1],
    type = "l", main = mt2, lwd = 2,
    col = dcol[1], ylim = c(0, 1), ylab = "F(x)", xlab = "(b)"
  )
  grid(col = 3)
  if (N >= 2) {
    for (i in 2:N) lines(xa, cdf[, i], lwd = 2, col = dcol[i])
  }
  if (!missing(xp2)) {
    yp2 <- getdf2(dist, xp2, para, para2)$cdf
    text(xp2, yp2, paste0(
      "(", para, ",", para2,
      ")"
    ))
  }
  else {
    legend(pos2, sapply(leg, as.expression), lwd = 2, col = dcol[1:N])
  }
}
