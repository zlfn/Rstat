#' @title Wilcoxon Rank Sum Test
#' @description Wilcoxon Rank Sum Test with a Plot
#' @usage ranksum.plot(x, y, side = "two", xlab = "Rank Sum statistic", dig = 4)
#' @param x Data vector in group 1
#' @param y Data vector in group 2
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param xlab Label of x-axis, Default: 'Rank Sum statistic'
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' x <- c(1, 5, 7, 8, 8, 8, 9)
#' y <- c(2, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10)
#' ranksum.plot(x, y)
#' @export
ranksum.plot <- function(x, y, side = "two", xlab = "Rank Sum statistic",
                         dig = 4) {
  n1 <- length(x)
  n2 <- length(y)
  U1 <- sum(rank(c(x, y))[1:n1]) - n1 * (n1 + 1) / 2
  U2 <- sum(rank(c(x, y))[(n1 + 1):(n1 + n2)]) - n2 * (n2 +
    1) / 2
  pv1 <- pwilcox(U2, n1, n2)
  pv2 <- pwilcox(U1, n1, n2)
  U <- min(U1, U2)
  pv <- 2 * pwilcox(U, n1, n2)
  cat(paste0(
    "n1=", n1, "\t n2=", n2, "\t U1=",
    U1, "\t U2=", U2
  ), "\n")
  mu <- n1 * n2 / 2
  sigsq <- n1 * n2 * (n1 + n2 + 1) / 12
  sig <- sqrt(sigsq)
  if (any(grepl(side, c("up", "greater")))) {
    pv <- pv1
    apv <- pnorm(U2, mu, sig)
    Z0 <- (U2 - mu) / sig
    cat(paste0("U-stat=", U2), "\t ")
  }
  else if (any(grepl(side, c("low", "less")))) {
    pv <- pv2
    apv <- pnorm(U1, mu, sig)
    Z0 <- (U1 - mu) / sig
    cat(paste0("U-stat=", U1), "\t ")
  }
  else {
    pv <- 2 * min(pv1, pv2)
    apv <- 2 * pnorm(U, mu, sig)
    Z0 <- (U - mu) / sig
    cat(paste0("U-stat=", U), "\t ")
  }
  cat(paste0("P-value=", round(pv, dig)), "\n")
  cat(paste0(
    "Normal appr.\t E(U)=", mu, "\t Var(U)=",
    round(sigsq, 4)
  ), "\n")
  cat(paste0(
    "Z0=", round(Z0, dig), "\t appr. p-value =",
    round(apv, 4)
  ), "\n")
  xmax <- ((n1 + n2) * (n1 + n2 + 1) / 2 - n1 * (n1 + 1) / 2) / 2
  xa <- 0:xmax
  xca <- (0:(10 * xmax)) / 10
  pdf <- dwilcox(xa, n1, n2)
  ymax <- max(pdf) * 1.05
  ymin <- -0.1 * max(pdf)
  dev.new(7, 5)
  plot(xa, pdf,
    type = "n", xlab = xlab, ylab = "f(u)",
    ylim = c(ymin, ymax), main = paste0(
      "Wilcoxon Rank Sum Test (n1=",
      n1, ", n2=", n2, ")"
    )
  )
  lines(xa, dwilcox(xa, n1, n2),
    type = "h", lwd = 3,
    col = grey(0.5)
  )
  segments(mu, 0, mu, dnorm(mu, mu, sig), lty = 2, col = 2)
  text(mu, ymin / 2, labels = mu, col = 4)
  if (any(grepl(side, c("up", "greater")))) {
    text(U1, dwilcox(U1, n1, n2), labels = U1, col = 4, pos = 3)
    lines(U1:xmax, dwilcox(U1:xmax, n1, n2),
      type = "h",
      col = 2, lwd = 3
    )
    text((U1 + xmax) / 2, ymin / 2,
      labels = round(pv, dig),
      col = 2
    )
  }
  else if (any(grepl(side, c("low", "less")))) {
    text(U1, dwilcox(U1, n1, n2), labels = U1, col = 4, pos = 3)
    lines(0:U1, dwilcox(0:U1, n1, n2),
      type = "h",
      col = 2, lwd = 3
    )
    text(U1 / 2, ymin / 2, labels = round(pv, dig), col = 2)
  }
  else {
    text(U1, dwilcox(U1, n1, n2), labels = U1, col = 4, pos = 3)
    lines(0:U1, dwilcox(0:U1, n1, n2),
      type = "h",
      col = 2, lwd = 3
    )
    text(U1 / 2, ymin / 2, labels = round(pv / 2, dig), col = 2)
    text(U2, dwilcox(U2, n1, n2), labels = U2, col = 4, pos = 3)
    lines(U2:xmax, dwilcox(U2:xmax, n1, n2),
      type = "h",
      col = 2, lwd = 3
    )
    text((U2 + xmax) / 2, ymin / 2,
      labels = round(pv / 2, dig),
      col = 2
    )
  }
  lines(xca, dnorm(xca, mu, sig), col = "green4")
  abline(h = 0)
}
