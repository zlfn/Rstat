#' @title Sign Test (Binomial Test)
#' @description Sign Test (Binomial Test) with a Plot
#' @usage signtest.plot(x, mu0 = 0, side = "two", dig = 4)
#' @param x Data vector
#' @param mu0 Mean value under the null hypothesis, Default: 0
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' (x <- c(1, 2, 5, 7, rep(8, 7), rep(9, 5), rep(10, 4)))
#' signtest.plot(x = x, mu0 = 7, side = "up")
#' @export

signtest.plot <-
  function(x, mu0 = 0, side = "two", dig = 4) {
    d <- x - mu0
    np <- sum(d > 0)
    n <- sum(d != 0)
    pv1 <- 1 - pbinom(np - 1, n, 0.5)
    pv2 <- pbinom(np, n, 0.5)
    cat("Total number of data =", length(x), "\n")
    cat("Number of data except", mu0, "=", n, "\n")
    cat(
      "Number of data greater than", mu0, "=",
      np, "\n"
    )
    mu <- n / 2
    sigsq <- n / 4
    sig <- sqrt(sigsq)
    apv1 <- pnorm(np - 0.5, mu, sig, lower.tail = F)
    apv2 <- pnorm(np + 0.5, mu, sig, lower.tail = T)
    if (any(grepl(side, c("up", "greater")))) {
      pv <- pv1
      apv <- apv1
    }
    else if (any(grepl(side, c("low", "less")))) {
      pv <- pv2
      apv <- apv2
    }
    else {
      pv <- 2 * min(pv1, pv2)
      apv <- 2 * min(apv1, apv2)
    }
    cat("Exact p-value (binomial distribution) =", round(
      pv,
      dig
    ), "\n")
    cat(
      "Normal approx. p-value (continuity correction) =",
      round(apv, dig), "\n"
    )
    xa <- 0:n
    xca <- (0:(10 * n)) / 10
    pdf <- dbinom(xa, n, 0.5)
    ymax <- max(pdf) * 1.05
    ymin <- -0.1 * max(pdf)
    win.graph(7, 5)
    plot(xa, pdf,
      type = "n", xlab = "Sign Statistic (positive sign)",
      ylab = "f(x)", ylim = c(ymin, ymax), main = paste0(
        "Distribution of Sign Teat Statistic (n=",
        n, ")"
      )
    )
    lines(xca, dnorm(xca, mu, sig), col = 4)
    abline(h = 0)
    lines(xa, dbinom(xa, n, 0.5),
      type = "h", lwd = 7,
      col = grey(0.5)
    )
    segments(mu, 0, mu, dnorm(mu, mu, sig), lty = 2, col = 2)
    text(mu, ymin / 2, labels = mu, col = 4)
    segments(np, 0, np, dbinom(np, n, 0.5), lwd = 2, col = 2)
    text(np, dbinom(np, n, 0.5), labels = np, col = 2, pos = 3)
    if (any(grepl(side, c("up", "greater")))) {
      lines(np:n, dbinom(np:n, n, 0.5),
        type = "h", col = 2,
        lwd = 7
      )
      text(n, ymin / 2, labels = paste0("pv=", round(
        pv,
        4
      )), col = 4, pos = 2)
    }
    else if (any(grepl(side, c("low", "less")))) {
      lines(0:np, dbinom(0:np, n, 0.5),
        type = "h", col = 2,
        lwd = 7
      )
      text(0, ymin / 2, labels = paste0("pv=", round(
        pv,
        4
      )), col = 4, pos = 4)
    }
    else {
      lines(np:n, dbinom(np:n, n, 0.5),
        type = "h", col = 2,
        lwd = 7
      )
      lines(0:(n - np), dbinom(0:(n - np), n, 0.5),
        type = "h",
        col = 2, lwd = 7
      )
      text(n, ymin / 2, labels = paste0("pv1=", round(
        pv / 2,
        4
      )), col = 4, pos = 2)
      text(0, ymin / 2, labels = paste0("pv2=", round(
        pv / 2,
        4
      )), col = 4, pos = 4)
    }
  }
