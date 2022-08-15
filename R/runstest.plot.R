#' @title Runs Test
#' @description Runs Test with a Plot
#' @usage runstest.plot(x, n1, n2, alp = 0.05, side = "two", dig = 4, plot = TRUE)
#' @param x Data vector (or number of runs)
#' @param n1 Number of data in group 1 (required if raw data are not given)
#' @param n2 Number of data in group 2 (required if raw data are not given)
#' @param alp Level of significance, Default: 0.05
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot runs test results? Default: TRUE
#'
#'
#' @return None.
#' @examples
#' require(randomizeBE)
#' x <- c(1, 1, 0, 0, 1, 0, rep(1, 7), rep(0, 7))
#' runstest.plot(x)
#'
#' x <- rep(0, 50)
#' x[c(1:2, 8:10, 17:18, 21, 26:27, 29:31, 36:37, 41:44, 49)] <- 1
#' runstest.plot(x)
#' @export
runstest.plot <- function(x, n1, n2, alp = 0.05, side = "two", dig = 4,
                          plot = TRUE) {
  nn <- length(x)
  if (nn > 1) {
    dval <- sort(unique(x))
    n1 <- sum(x == dval[1])
    n2 <- sum(x == dval[2])
    r1 <- length(rle(x)[[1]])
  }
  else {
    r1 <- x
  }
  mu <- 2 * n1 * n2 / (n1 + n2) + 1
  vr <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / (n1 + n2)^2 / (n1 +
    n2 - 1)
  if (any(grepl(side, c("low", "less")))) {
    pv <- pruns.exact(r1, n1, n2, tail = "lower")
    h1 <- "One-sided (positive correlation)"
    area <- 2:r1
    xpt <- r1
    pos <- 2
    z0 <- (r1 + 0.5 - mu) / sqrt(vr)
    pv2 <- pnorm(z0)
    ppv <- pv
  }
  else if (any(grepl(side, c("up", "greater")))) {
    pv <- pruns.exact(r1, n1, n2, tail = "upper")
    h1 <- "One-sided (negative correlation)"
    area <- r1:(n1 + n2)
    xpt <- r1
    pos <- 4
    z0 <- (r1 - 0.5 - mu) / sqrt(vr)
    pv2 <- 1 - pnorm(z0)
    ppv <- pv
  }
  else {
    pv <- pruns.exact(r1, n1, n2, tail = "2-sided")
    h1 <- "Two-sided"
    r2 <- mu + (mu - r1)
    area <- c(2:min(r1, r2), max(r1, r2):(n1 + n2))
    xpt <- sort(c(r1, r2))
    pos <- c(2, 4)
    z0 <- ifelse(r1 < mu, (r1 + 0.5 - mu) / sqrt(vr), (r1 -
      0.5 - mu) / sqrt(vr))
    pv2 <- 2 * pnorm(-abs(z0))
    plow <- pruns.exact(min(r1, r2), n1, n2, tail = "lower")
    pupp <- pruns.exact(max(r1, r2), n1, n2, tail = "upper")
    ppv <- c(plow, pupp)
  }
  cat(paste0(
    "Runs test (n1=", n1, ",  n2=", n2,
    ") : ", h1
  ), "\n")
  cat("Number of Runs =", r1, "\t p-value =", round(
    pv,
    dig
  ), "\n")
  cat(
    "E(R) =", round(mu, dig), "\t\t Var(R) =",
    round(vr, dig), "\n"
  )
  cat("Normal appr. (cont. corr.) \t Z0 =", round(
    z0,
    dig
  ), "\t p-value =", round(pv2, dig), "\n")
  if (plot) {
    xa <- 2:(n1 + n2)
    mt <- paste0(
      "Runs Test (n1=", n1, ",  n2=",
      n2, ") : ", h1
    )
    ya <- Vdruns.exact(xa, n1, n2)
    ymax <- max(ya, dnorm(mu, mu, sqrt(vr)))
    dev.new(7, 5)
    plot(xa, ya,
      type = "h", lwd = 5, ylab = "f(r)",
      xlab = "Number of Runs", ylim = c(0, ymax),
      main = mt, col = grey(0.5)
    )
    xa2 <- seq(2, n1 + n2, length = 100)
    lines(xa2, dnorm(xa2, mu, sqrt(vr)), lty = 1, col = "green4")
    lines(area, Vdruns.exact(area, n1, n2),
      type = "h",
      lwd = 5, col = 2
    )
    text(xpt, druns.exact(r1, n1, n2), labels = round(
      ppv,
      4
    ), col = 4, pos = pos)
  }
}
