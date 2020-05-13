#' @title Plot the PDF of Test Statistic with the T-distribution

#' @description Plot the PDF of Test Statistic with the T-distribution


#' @usage ttest.plot(md, deg, prng = c(-4, 4), side = "two", dig = 4, mt, pvout = TRUE)
#' @param md T-test statistic for the difference of population means
#' @param deg Degree of freedom
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @param mt Plot title
#' @param pvout Print p-value? Default: TRUE
#'
#' @return None.
#' @examples
#' ttest.plot(1.96, deg = 24)
#' @export

ttest.plot <-
  function(md, deg, prng = c(-4, 4), side = "two", dig = 4,
           mt, pvout = TRUE) {
    xa <- seq(prng[1], prng[2], length.out = 101)
    if (missing(mt)) {
      mt <- paste0(
        "Distribution of the Test Statistic under H0: t(",
        round(deg, 3), ")"
      )
    }
    plot(xa, dt(xa, deg),
      type = "n", xlab = "Test Statistic",
      ylab = "pdf", ylim = c(-0.1, 1) * max(dt(xa, deg)),
      main = mt
    )
    if (side == "up" | grepl(side, "greater")) {
      pv <- pt(md, deg, lower.tail = FALSE)
      if (pvout) {
        cat("P-v =", pv, "\n")
      }
      cord.x <- c(md, seq(md, prng[2], length.out = 20), prng[2])
      cord.y <- c(
        0, dt(seq(md, prng[2], length.out = 20), deg),
        0
      )
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(md, 0, md, dt(md, deg), lwd = 2, col = 2)
      text(md, dt(md, deg) * 0.9, round(pv, dig),
        pos = 4,
        col = 2
      )
      text(md, 0, round(md, dig), pos = 1, col = 4)
    }
    else if (side == "low" | grepl(side, "less")) {
      pv <- pt(md, deg)
      if (pvout) {
        cat("P-v =", pv, "\n")
      }
      cord.x <- c(
        prng[1], seq(prng[1], md, length.out = 20),
        md
      )
      cord.y <- c(
        0, dt(seq(prng[1], md, length.out = 20), deg),
        0
      )
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(md, 0, md, dt(md, deg), lwd = 2, col = 2)
      text(md, dt(md, deg) * 0.9, round(pv, dig),
        pos = 2,
        col = 2
      )
      text(md, 0, round(md, dig), pos = 1, col = 4)
    }
    else if (grepl(side, "two side")) {
      mlow <- ifelse(md > 0, -md, md)
      mup <- ifelse(md > 0, md, -md)
      pv <- 2 * pt(mlow, deg)
      if (pvout) {
        cat("P-v =", pv, "\n")
      }
      cord.x <- c(mup, seq(mup, prng[2], length.out = 20), prng[2])
      cord.y <- c(0, dt(
        seq(mup, prng[2], length.out = 20),
        deg
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      cord.x <- c(
        prng[1], seq(prng[1], mlow, length.out = 20),
        mlow
      )
      cord.y <- c(0, dt(
        seq(prng[1], mlow, length.out = 20),
        deg
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(c(mlow, mup), 0, c(mlow, mup), dt(md, deg),
        lwd = 2, col = 2
      )
      text(c(mlow, mup), dt(md, deg) * 0.9, round(pv / 2, dig),
        pos = c(2, 4), col = 2
      )
      text(c(mlow, mup), 0, round(c(mlow, mup), dig),
        pos = 1,
        col = 4
      )
    }
    abline(h = 0)
    abline(v = 0, lty = 2, lwd = 2, col = "green3")
    lines(xa, dt(xa, deg), type = "l", lwd = 2, col = 4)
  }
