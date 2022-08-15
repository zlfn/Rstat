#' @title Plot the Chi-square Test
#' @description Plot the Result of the Chi-square Test
#' @usage chitest.plot(stat, df, prng, side = "two", mt, dig = 4, ppt = 20)
#' @param stat Chi-square test statistic
#' @param df Degree of freedom
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param ppt Nu
#'
#' @return None.
#' @examples
#' chi0 <- var1.test(x = 1.24, n = 25, var0 = 0.8, side = "up")
#' chitest.plot(stat = chi0$stat, df = chi0$df, side = "up")
#' x <- c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' res <- var1.test(x = x, var0 = 2, side = "two")
#' chitest.plot(stat = res$stat, df = res$df, side = "two")
#' @export


chitest.plot <- function(stat, df, prng, side = "two", mt, dig = 4, ppt = 20) {
  if (missing(prng)) {
    prng <- c(0, qchisq(0.999, df))
  }
  if (missing(mt)) {
    mt <- bquote(bold("Chi-Square Test :") ~ chi^2 ~
    (.(df)))
  }
  xa <- seq(prng[1], prng[2], length = 100)
  dev.new(7, 5)
  plot(xa, dchisq(xa, df),
    type = "n", xlab = "Test Statistic(x)",
    ylab = "f(x)", ylim = c(-0.1, 1) * max(dchisq(
      xa,
      df
    )), main = mt
  )
  plow <- pchisq(stat, df)
  if (any(grepl(side, c("greater", "up")))) {
    pv <- 1 - plow
    cat(paste0(
      "Chi0 = ", round(stat, dig), "\t P-v = ",
      round(pv, dig)
    ), "\n")
    cord.x <- c(
      stat, seq(stat, prng[2], length.out = ppt),
      prng[2]
    )
    cord.y <- c(0, dchisq(
      seq(stat, prng[2], length.out = ppt),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(stat, 0, stat, dchisq(stat, df), lwd = 2, col = 2)
    text(stat, dchisq(stat, df) * 0.9, round(pv, 4),
      pos = 4,
      col = 2
    )
    text(stat, 0, round(stat, 4), pos = 1, col = 4)
  }
  else if (any(grepl(side, c("less", "low")))) {
    pv <- plow
    cat(paste0(
      "Chi0 = ", round(stat, dig), "\t P-v = ",
      round(pv, dig)
    ), "\n")
    cord.x <- c(
      prng[1], seq(prng[1], stat, length.out = ppt),
      stat
    )
    cord.y <- c(0, dchisq(
      seq(prng[1], stat, length.out = ppt),
      df
    ), 0)
    polygon(cord.x, cord.y, col = "lightcyan")
    segments(stat, 0, stat, dchisq(stat, df), lwd = 2, col = 2)
    text(stat, dchisq(stat, df) * 0.9, round(pv, 4),
      pos = 2,
      col = 2
    )
    text(stat, 0, round(stat, 4), pos = 1, col = 4)
  }
  else if (any(grepl(side, c("two.sided", "two-sided")))) {
    pv <- 2 * min(plow, 1 - plow)
    cat(paste0(
      "Chi0 = ", round(stat, dig), "\t P-v = ",
      round(pv, dig)
    ), "\n")
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
    text(c(mlow, mup), dchisq(c(mlow, mup), df) * 0.9, round(
      pv / 2,
      4
    ), pos = c(2, 4), col = 2)
    text(c(mlow, mup), 0, round(c(mlow, mup), 4),
      pos = 1,
      col = 4
    )
  }
  abline(h = 0)
  abline(v = qchisq(0.5, df), lty = 2, lwd = 2, col = "green3")
  lines(xa, dchisq(xa, df), type = "l", lwd = 2, col = 4)
}
