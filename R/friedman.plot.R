#' @title Friedman Test
#' @description Friedman Test with a Plot
#' @usage friedman.plot(x, a, b, dig = 4, plot = FALSE)
#' @param x Data vector
#' @param a Vector of factor levels
#' @param b Vector of block levels
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot test results? Default: FALSE
#'
#' @return None.
#' @examples
#' x <- c(71, 77, 75, 79, 88, 70, 94, 74, 74, 83, 72, 95, 77, 80, 90, 94, 64, 76, 76, 89)
#' a <- rep(1:4, each = 5)
#' b <- rep(1:5, 4)
#' friedman.plot(x, a, b, plot = TRUE)
#' @export

friedman.plot <- function(x, a, b, dig = 4, plot = FALSE) {
  nn <- length(x)
  kk <- length(unique(a))
  rr <- length(unique(b))
  af <- as.factor(a)
  bf <- as.factor(b)
  rx <- tapply(x, b, rank)
  urx <- unlist(rx)
  rxm <- matrix(urx, kk, rr)
  a2 <- rep(1:kk, rr)
  ra <- tapply(urx, a2, sum)
  rtab <- cbind(rxm, ra)
  rownames(rtab) <- paste0("Group", 1:kk)
  colnames(rtab) <- c(1:rr, "Sum")
  cat("Rank Sum within each Group -----------\n")
  print(rtab)
  F <- 12 / kk / (kk + 1) / rr * sum(ra^2) - 3 * rr * (kk + 1)
  pv <- pchisq(F, kk - 1, lower.tail = FALSE)
  cat("Friedman Test ----------\n")
  cat(paste0(
    "F = (12 / ", kk, " / ", kk + 1, " / ",
    rr, ") × ", sum(ra^2), " - 3 × ", rr, " × ",
    kk + 1, " = ", round(F, dig)
  ), "\n")
  cat(paste0("df=", kk - 1, "\t p-value=", round(
    pv,
    dig
  )), "\n")
  if (plot) {
    chitest.plot2(stat = F, df = kk - 1, side = "up")
  }
}
