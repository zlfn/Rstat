#' @title Exact Binomial Test
#' @description Plot the Result of Exact Binomial Test
#' @usage bntest.plot(x, n, p0, alp = 0.05, side = "two", dig = 4, dcol)
#' @param x Vector of number of successes
#' @param n Sample size
#' @param p0 Population ratio value under the null hypothesis
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @param dcol Colors of the probability bars
#' @return None.
#' @examples
#' bntest.plot(x = 2:4, n = 10, p0 = 0.1, side = "up")
#' bntest.plot(x = 6:4, n = 20, p0 = 0.5, side = "two")
#' @export


bntest.plot <- function(x, n, p0, alp = 0.05, side = "two", dig = 4,
                        dcol) {
  nx <- length(x)
  nside <- grep(side, c("lower", "upper", "two.sided"))
  side2 <- switch(nside, "less", "greater", "two.sided")
  simb1 <- simb2 <- rep("", nx)
  tag1 <- tag2 <- rep("", nx)
  pv1 <- pbinom(x, n, p0)
  pv2 <- 1 - pbinom(pmax(0, x - 1), n, p0)
  pv3 <- rep(NA, nx)
  for (k in 1:nx) {
    pv3[k] <- sum(dbinom(0:n, n, p0)[dbinom(
      0:n,
      n, p0
    ) <= dbinom(x[k], n, p0)])
  }
  pval <- switch(nside, pv1, pv2, pv3)
  for (k in 1:nx) {
    if (nside == 3) {
      tag1[k] <- paste0("Σ[f(y) ≤ f(", x[k], ")] = ")
      tag2[k] <- paste0(
        "Σ[f(y)≤", "f(",
        x[k], ")]="
      )
    }
    simb1[k] <- switch(nside, paste0(
      "P(X ≤ ", x[k],
      ") = "
    ), paste0("P(X ≥ ", x[k], ") = "),
    tag1[k]
    )
    simb2[k] <- switch(nside, paste0(
      "P(X≤", x[k],
      ")="
    ), paste0("P(X≥", x[k], ")="),
    tag2[k]
    )
    bo <- binom.test(x[k], n, p0, alt = side2, conf = 1 -
      alp)
    cat(paste0(
      "X = ", x[k], "\t P-v = ", simb1[k],
      round(pval[k], dig), "\t ", 100 * (1 - alp),
      "%-CI = [", round(bo$conf[1], dig), ", ",
      round(bo$conf[2], dig), "]"
    ), "\n")
  }
  xa <- 0:n
  if (missing(dcol)) {
    dcol <- c(4, 2, "green4", "orange", "purple")
  }
  win.graph(7, 5)
  plot(xa, dbinom(xa, n, p0),
    type = "h", lwd = 7, col = grey(0.7),
    ylim = c(0, 1.1 * max(dbinom(xa, n, p0))), main = paste0(
      "B(",
      n, ", ", p0, ") Distribution & P-value"
    ),
    xlab = "x", ylab = "f(x)"
  )
  if (n <= 12) {
    text(xa, dbinom(xa, n, p0), round(
      dbinom(xa, n, p0),
      dig
    ), col = 1, pos = 3, cex = 0.8)
  }
  lab <- rep("", nx)
  for (k in 1:nx) {
    xa2 <- switch(nside, 0:x[k], x[k]:n, (0:n)[dbinom(
      0:n,
      n, p0
    ) <= dbinom(x[k], n, p0)])
    lines(xa2, dbinom(xa2, n, p0),
      type = "h", lwd = 5,
      col = dcol[k]
    )
    lab[k] <- paste0(simb2[k], round(pval[k], dig))
  }
  legend("topright", lab, text.col = dcol[1:nx])
}
