#' @title Wilcoxon Signed Rank Test
#' @description Wilcoxon Signed Rank Test with a Plot
#' @usage signrank.plot(x, y, mu0 = 0, side = "two", xlab = "Signed Rank Sum", dig = 4)
#' @param x Data vector in group 1
#' @param y Data vector in group 2
#' @param mu0 Mean difference under the null hypothesis, Default: 0
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param xlab Label of x-axis, Default: 'Signed Rank Sum'
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' x <- c(38, 26, 34, 5, 68, 30, 35, 19, 33, 69)
#' y <- c(28, 21, 31, 11, 59, 28, 28, 23, 32, 38)
#' signrank.plot(x = x, y = y, side = "up")
#' @export

signrank.plot <-
  function(x, y, mu0 = 0, side = "two", xlab = "Signed Rank Sum",
           dig = 4) {
    d <- x - y - mu0
    d <- d[d != 0]
    n <- length(d)
    rv <- rank(abs(d))
    w1 <- sum(rv[d > 0])
    w2 <- sum(rv[d < 0])
    pv1 <- psignrank(w2, n)
    pv2 <- psignrank(w1, n)
    cat(paste0(
      "n=", length(x), "\t effe. n=", n,
      "\t W1=", w1, "\t W2=", w2, "\n"
    ))
    mu <- n * (n + 1) / 4
    sigsq <- n * (n + 1) * (2 * n + 1) / 24
    sig <- sqrt(sigsq)
    apv1 <- pnorm(w2 + 0.5, mu, sig)
    apv2 <- pnorm(w1 + 0.5, mu, sig)
    if (any(grepl(side, c("up", "greater")))) {
      pv <- pv1
      apv <- apv1
      cat(paste0("W-stat=", w1), "\t ")
      Z0 <- (w1 - 0.5 - mu) / sig
    }
    else if (any(grepl(side, c("low", "less")))) {
      pv <- pv2
      apv <- apv2
      cat(paste0("W-stat=", w1), "\t ")
      Z0 <- (w1 + 0.5 - mu) / sig
    }
    else {
      pv <- 2 * min(pv1, pv2)
      apv <- 2 * min(apv1, apv2)
      cat(paste0("W-stat=", min(w1, w2)), "\t ")
      Z0 <- (min(w1, w2) + 0.5 - mu) / sig
    }
    cat(paste0("P-value=", round(pv, dig)), "\n")
    cat(paste0("E(W)=", mu, "\t Var(W)=", round(
      sigsq,
      dig
    )), "\n")
    cat(paste0("Normal appr. (cont. corr.) Z0=", round(
      Z0,
      dig
    ), "\t p-value=", round(apv, dig)), "\n")
    xmax <- n * (n + 1) / 2
    xa <- 0:xmax
    xca <- (0:(10 * xmax)) / 10
    pdf <- dsignrank(xa, n)
    ymax <- max(pdf) * 1.05
    ymin <- -0.1 * max(pdf)
    win.graph(7, 5)
    plot(xa, pdf,
      type = "n", xlab = xlab, ylab = "f(u)",
      ylim = c(ymin, ymax), main = paste0(
        "Wilcoxon Sign Rank Sum Test (n=",
        n, ")"
      )
    )
    lines(xca, dnorm(xca, mu, sig), col = "green4")
    abline(h = 0)
    lines(xa, pdf, type = "h", lwd = 3, col = grey(0.6))
    segments(mu, 0, mu, dnorm(mu, mu, sig), lty = 2, col = 2)
    text(mu, ymin / 2, labels = mu, col = 4)
    if (any(grepl(side, c("up", "greater")))) {
      text(w1, dsignrank(w1, n), labels = w1, col = 4, pos = 3)
      lines(w1:xmax, dsignrank(w1:xmax, n),
        type = "h",
        col = 2, lwd = 3
      )
      text((w1 + xmax) / 2, ymin / 2,
        labels = round(pv, dig),
        col = 2
      )
    }
    else if (any(grepl(side, c("low", "less")))) {
      text(w1, dsignrank(w1, n), labels = w1, col = 4, pos = 3)
      lines(0:w1, dsignrank(0:w1, n),
        type = "h", col = 2,
        lwd = 3
      )
      text(w1 / 2, ymin / 2, labels = round(pv, dig), col = 2)
    }
    else {
      wmin <- min(w1, w2)
      wmax <- max(w1, w2)
      text(wmin, dsignrank(wmin, n),
        labels = wmin, col = 4,
        pos = 3
      )
      lines(0:wmin, dsignrank(0:wmin, n),
        type = "h",
        col = 2, lwd = 3
      )
      text(wmin / 2, ymin / 2, labels = round(pv / 2, dig), col = 2)
      text(wmax, dsignrank(wmax, n),
        labels = wmax, col = 4,
        pos = 3
      )
      lines(wmax:xmax, dsignrank(wmax:xmax, n),
        type = "h",
        col = 2, lwd = 3
      )
      text((wmax + xmax) / 2, ymin / 2,
        labels = round(pv / 2, dig),
        col = 2
      )
    }
  }
