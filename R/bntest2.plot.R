#' @title Three Types of Binomial Tests
#' @description Plot the Results of Three Types of Binomial Tests
#' @usage bntest2.plot(x, n, p0, alp = 0.05, side = "two", dig = 4)
#' @param x Number of successes
#' @param n Sample size
#' @param p0 Population ratio value under the null hypothesis
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' bntest2.plot(x = 15, n = 200, p0 = 0.1, alp = 0.05, side = "low")
#' bntest2.plot(x = 15, n = 200, p0 = 0.1, alp = 0.05, side = "two")
#' @export

bntest2.plot <- function(x, n, p0, alp = 0.05, side = "two", dig = 4) {
  nside <- grep(side, c("lower", "upper", "two.sided"))
  side2 <- switch(nside, "less", "greater", "two.sided")
  bo <- binom.test(x, n, p0, alt = side2)
  pve1 <- pbinom(x, n, p0)
  pve2 <- 1 - pbinom(pmax(0, x - 1), n, p0)
  pve3 <- sum(dbinom(0:n, n, p0)[dbinom(0:n, n, p0) <= dbinom(
    x,
    n, p0
  )])
  pvale <- switch(nside, pve1, pve2, pve3)
  tag1 <- paste0("Σ[f(y) ≤ f(", x, ")] = ")
  tag2 <- paste0("Σ[f(y)≤", "f(", x, ")]=")
  simb <- switch(nside, "P(Z < Z0) = ", "P(Z > Z0) = ",
    "2×P(Z > |Z0|) = "
  )
  simbe1 <- switch(nside, paste0("P(X ≤ ", x, ") = "),
    paste0("P(X ≥ ", x, ") = "), tag1
  )
  simbe2 <- switch(nside, paste0("P(X≤", x, ")="),
    paste0("P(X≥", x, ")="), tag2
  )
  bo <- binom.test(x, n, p0, alt = side2, conf = 1 - alp)
  cat(
    paste0("Exact Binomial Test:\t x/n = ", round(
      bo$est,
      dig
    ), "\t P-v = ", simbe1, round(bo$p.val, dig)),
    "\n"
  )
  ph <- x / n
  se <- sqrt(n * p0 * (1 - p0))
  tstat <- (x - n * p0) / se
  pv <- switch(nside, pnorm(tstat), 1 - pnorm(tstat), 2 * (1 -
    pnorm(abs(tstat))))
  cat(paste0("Normal approximation:\t Z0 = ", round(
    tstat,
    dig
  ), "\t P-v = ", simb, round(pv, dig)), "\n")
  tstat2 <- switch(nside, (x + 0.5 - n * p0) / se, (x - 0.5 -
    n * p0) / se, ifelse(x < n * p0, (x + 0.5 - n * p0) / se,
    (x - 0.5 - n * p0) / se
  ))
  pv2 <- switch(nside, pnorm(tstat2), 1 - pnorm(tstat2), 2 *
    (1 - pnorm(abs(tstat2))))
  cat(paste0("Continuity correction:\t Z0 = ", round(
    tstat2,
    dig
  ), "\t P-v = ", simb, round(pv2, dig)), "\n")
  x1 <- max(0, floor(n * p0 - 4 * se))
  x2 <- min(n, ceiling(n * p0 + 4 * se))
  xa <- x1:x2
  ymax <- dbinom(n * p0, n, p0) * 1.1
  dev.new(7, 5)
  plot(xa, dbinom(xa, n, p0),
    type = "h", lwd = 5, col = grey(0.7),
    ylim = c(0, ymax), main = paste0(
      "B(", n, ", ",
      p0, ") Distribution & P-value"
    ), xlab = "x",
    ylab = "f(x)"
  )
  abline(h = 0, col = grey(0.4))
  xa2 <- switch(nside, x1:x, x:x2, (x1:x2)[dbinom(
    x1:x2, n,
    p0
  ) <= dbinom(x, n, p0)])
  lines(xa2, dbinom(xa2, n, p0),
    type = "h", lwd = 5,
    col = 2
  )
  xa3 <- seq(x1, x2, length = 100)
  lines(xa3, dnorm(xa3, n * p0, se), col = "green2")
  xo <- round(2 * n * p0 - x, 0)
  if (x < n * p0) {
    txa4 <- c(x, x + 0.5, xo, xo - 0.5)
  }
  else {
    txa4 <- c(xo, xo + 0.5, x, x - 0.5)
  }
  xa4 <- switch(nside, c(x, x + 0.5), c(x, x - 0.5), txa4)
  if (nside == 1) {
    abline(v = xa4, lty = 2, col = c(4, "purple"))
  }
  if (nside == 2) {
    abline(v = xa4, lty = 2, col = c(4, "purple"))
  }
  if (nside == 3) {
    abline(v = xa4, lty = 2, col = c(4, "purple"))
  }
  lab1 <- paste("Exact :", round(pvale, dig))
  lab2 <- paste("Normal :", round(pv, dig))
  lab3 <- paste("Correct :", round(pv2, dig))
  legend("topright", c(lab1, lab2, lab3), text.col = c(
    2,
    4, "purple"
  ))
}
