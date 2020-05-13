#' @title Plot the PDF of Test Statistic under the Normal Distribution
#' @description Plot the PDF of Test Statistic under the Normal Distribution
#' @usage normtest.plot(md, mu0 = 0, se = 1, prng = c(-4, 4), side = "two", xlab = "Sample Mean", pvout = TRUE)
#' @param md Difference of sample means or test statistic
#' @param mu0 Difference of population means under the null hypothesis, Default: 0
#' @param se Standard error of the difference of sample means, Default: 1
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param xlab Label of x-axis, Default: 'Sample Mean'
#' @param pvout Print p-value? Default: TRUE
#'
#'
#' @return None.
#' @examples
#' normtest.plot(11.5 - 10.4, se = sqrt(2.2 / 20 + 2.4 / 25))
#' normtest.plot(1.93, xlab = "Test Statistic")
#' @export
normtest.plot <-
  function(md, mu0 = 0, se = 1, prng = c(-4, 4), side = "two",
           xlab = "Sample Mean", pvout = TRUE) {
    xa <- seq(prng[1], prng[2], length.out = 101)
    plot(xa, dnorm(xa, mu0, se),
      type = "n", xlab = xlab,
      ylab = "pdf", ylim = c(-0.1, 1) * max(dnorm(
        xa,
        mu0, se
      )), main = bquote(bold("Distribution of ") ~
      bold(.(xlab)) ~ bold("under H0 :") ~ " N(" ~
      .(mu0) ~ "," ~ .(round(se, 3))^2 ~ ")")
    )
    if (side == "up" | grepl(side, "greater")) {
      pv <- pnorm(md, mu0, se, lower.tail = FALSE)
      if (pvout) {
        cat("P-v =", pv, "\n")
      }
      cord.x <- c(md, seq(md, prng[2], length.out = 20), prng[2])
      cord.y <- c(0, dnorm(
        seq(md, prng[2], length.out = 20),
        mu0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(md, 0, md, dnorm(md, mu0, se), lwd = 2, col = 2)
      text(md, dnorm(md, mu0, se) * 0.9, round(pv, 4),
        pos = 4,
        col = 2
      )
      text(md, 0, round(md, 4), pos = 1, col = 4)
    }
    else if (side == "low" | grepl(side, "less")) {
      pv <- pnorm(md, mu0, se)
      if (pvout) {
        cat("P-v =", pv, "\n")
      }
      cord.x <- c(
        prng[1], seq(prng[1], md, length.out = 20),
        md
      )
      cord.y <- c(0, dnorm(
        seq(prng[1], md, length.out = 20),
        mu0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(md, 0, md, dnorm(md, mu0, se), lwd = 2, col = 2)
      text(md, dnorm(md, mu0, se) * 0.9, round(pv, 4),
        pos = 2,
        col = 2
      )
      text(md, 0, round(md, 4), pos = 1, col = 4)
    }
    else if (grepl(side, "two sided")) {
      mlow <- ifelse(md > mu0, 2 * mu0 - md, md)
      mup <- ifelse(md > mu0, md, 2 * mu0 - md)
      pv <- 2 * pnorm(mlow, mu0, se)
      if (pvout) {
        cat("P-v =", pv, "\n")
      }
      cord.x <- c(mup, seq(mup, prng[2], length.out = 20), prng[2])
      cord.y <- c(0, dnorm(
        seq(mup, prng[2], length.out = 20),
        mu0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      cord.x <- c(
        prng[1], seq(prng[1], mlow, length.out = 20),
        mlow
      )
      cord.y <- c(0, dnorm(
        seq(prng[1], mlow, length.out = 20),
        mu0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(c(mlow, mup), 0, c(mlow, mup), dnorm(
        md, mu0,
        se
      ), lwd = 2, col = 2)
      text(c(mlow, mup), dnorm(md, mu0, se) * 0.9, round(
        pv / 2,
        4
      ), pos = c(2, 4), col = 2)
      text(c(mlow, mup), 0, round(c(mlow, mup), 4),
        pos = 1,
        col = 4
      )
    }
    abline(h = 0)
    abline(v = mu0, lty = 2, lwd = 2, col = "green3")
    lines(xa, dnorm(xa, mu0, se),
      type = "l", lwd = 2,
      col = 4
    )
  }
