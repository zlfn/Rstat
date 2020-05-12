#' @title Plot the Chi-square Test
#' @description Plot the Result of the Chi-square Test
#' @usage chitest.plot2(stat, df, alp = 0.05, side = "two", pup = 0.999, dig = 4, ppt = 20)
#' @param stat Chi-square test statistic
#' @param df Degree of freedom
#' @param alp Level of significance, Default: 0.05
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param pup Maximum probability for the range of x-axis, Default: 0.999
#' @param dig Number of digits below the decimal point, Default: 4
#' @param ppt Num
#'
#' @return None.
#' @examples
#' # Goodness-of-fit Test
#' x <- c(31, 26, 22, 18, 13, 10)
#' (ct <- chisq.test(x))
#' chitest.plot2(stat = ct$stat, df = ct$para, side = "up")
#' # Test of Homogeneity
#' x <- c(20, 16, 29, 21, 14, 14, 22, 26, 25, 13, 18, 24, 32, 18, 8, 8, 18, 33, 16, 25)
#' x <- matrix(x, nrow = 4, ncol = 5, byrow = TRUE)
#' (ct <- chisq.test(x))
#' chitest.plot2(stat = ct$stat, df = ct$para, side = "up")
#' @export


chitest.plot2 <- function(stat, df, alp = 0.05, side = "two", pup = 0.999,
                          dig = 4, ppt = 20) {
  rej <- qchisq(1 - alp, df)
  prng <- c(0, qchisq(pup, df))
  xa <- seq(prng[1], prng[2], length.out = 101)
  ymax <- max(dchisq(xa, df))
  win.graph(7, 5)
  plot(xa, dchisq(xa, df),
    type = "n", xlab = "Chi-square Statistic",
    ylab = "pdf", ylim = c(0, ymax * 1.1), main = bquote(bold("Distribution of the Chi-square Statistic under H0: ") ~
    chi^2 ~ (.(df)))
  )
  abline(h = 0)
  plow <- pchisq(stat, df)
  if (side == "up" | grepl(side, "greater")) {
    pv <- 1 - plow
    cat("P-v =", round(pv, dig), "\n")
    cord.x <- c(
      stat, seq(stat, prng[2], length.out = ppt),
      prng[2]
    )
    cord.y <- c(0, dchisq(
      seq(stat, prng[2], length.out = ppt),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    rejy <- dchisq(rej, df)
    rejy <- (rejy + ymax) / 2
    staty <- (dchisq(stat, df) + ymax) / 2
    segments(stat, 0, stat, staty, lwd = 1, col = 4)
    xpv <- ifelse(stat > qchisq(0.5, df), (stat + prng[2]) / 2,
      stat
    )
    ypv <- ifelse(stat > qchisq(0.5, df), dchisq(stat, df),
      dchisq(stat, df) / 2
    )
    pospv <- ifelse(stat > qchisq(0.5, df), 3, 4)
    text(xpv, ypv, round(pv, dig), pos = pospv, col = 2)
    segments(rej, 0, rej, rejy, lwd = 1, col = 2)
    ry0 <- ifelse(abs(rej - stat) < 2, rejy, 0)
    postat <- ifelse(stat > qchisq(0.5, df), 4, 3)
    text(stat, staty, labels = bquote(chi[0]^2 == .(round(
      stat,
      dig
    ))), pos = postat, col = 4, cex = 1)
    text(rej, rejy,
      labels = bquote(chi[.(paste(
        1 - alp,
        ";", df
      ))]^2 == .(round(rej, dig))), pos = 3,
      col = 2, cex = 1
    )
  }
  else if (side == "low" | grepl(side, "less")) {
    pv <- plow
    cat("P-v =", round(pv, dig), "\n")
    cord.x <- c(
      prng[1], seq(prng[1], stat, length.out = ppt),
      chis
    )
    cord.y <- c(0, dchisq(
      seq(prng[1], stat, length.out = ppt),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(stat, 0, stat, dchisq(stat, df), lwd = 2, col = 2)
    text(stat, dchisq(stat, df), round(pv, dig),
      pos = 2,
      col = 2
    )
    text(stat, 0, round(stat, dig), pos = 1, col = 4)
  }
  else if (grepl(side, "two sided")) {
    pv <- 2 * min(plow, 1 - plow)
    cat("P-v =", round(pv, dig), "\n")
    mlow <- qchisq(pv / 2, df)
    mup <- qchisq(1 - pv / 2, df)
    cord.x <- c(
      mup, seq(mup, prng[2], length.out = ppt),
      prng[2]
    )
    cord.y <- c(0, dchisq(
      seq(mup, prng[2], length.out = ppt),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    cord.x <- c(
      prng[1], seq(prng[1], mlow, length.out = ppt),
      mlow
    )
    cord.y <- c(0, dchisq(
      seq(prng[1], mlow, length.out = ppt),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(c(mlow, mup), 0, c(mlow, mup), dchisq(c(
      mlow,
      mup
    ), df), lwd = 2, col = 2)
    text(c(mlow, mup), dchisq(c(mlow, mup), df), round(
      pv / 2,
      dig
    ), pos = c(2, 4), col = 2)
    text(c(mlow, mup), 0, round(c(mlow, mup), dig),
      pos = 1,
      col = 4
    )
  }
  abline(v = qchisq(0.5, df), lty = 2, lwd = 2, col = "green3")
  lines(xa, dchisq(xa, df), type = "l", lwd = 2, col = 4)
}
