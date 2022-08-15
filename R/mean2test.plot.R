#' @title Hypothesis Test for the Difference of Two Means
#' @description Hypothesis Test for the Difference of Two Population Means
#' @usage mean2test.plot(xb1, xb2, s1, s2, n1, n2, d0 = 0, prng, side = "two", pvar = "equal", mt, dig = 4, xlab)
#' @param xb1 Sample mean of population1 (or sample data)
#' @param xb2 Sample mean of population2 (or sample data)
#' @param s1 Standard deviation of population1 (optional for unknown variance)
#' @param s2 Standard deviation of population2 (optional for unknown variance)
#' @param n1 Sample size of population1 (unnecessary if data are given)
#' @param n2 Sample size of population2 (unnecessary if data are given)
#' @param d0 Difference of two population means under the null hypothesis, Default: 0
#' @param prng Range of x-axis, Default: (d0-4se, d0+4se)
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param pvar Status of variance (one of "known", "equal", "unequal"), Default: 'equal'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param xlab Label of x-axis
#'
#'
#' @return None.
#' @examples
#' mean2test.plot(xb1 = 198.5, xb2 = 201.3, s1 = 5, s2 = 5, n1 = 25, n2 = 34, pvar = "known")
#' mean2test.plot(xb1 = 198.5, xb2 = 201.3, s1 = 4.8, s2 = 5.1, n1 = 25, n2 = 34, pvar = "equal")
#' mean2test.plot(xb1 = 198.5, xb2 = 201.3, s1 = 2.8, s2 = 5.5, n1 = 25, n2 = 34, pvar = "unequal")
#'
#' x <- rnorm(20, 199, 2)
#' y <- rnorm(25, 200, 2)
#' mean2test.plot(x, y, pvar = "equal")
#' @export

mean2test.plot <- function(xb1, xb2, s1, s2, n1, n2, d0 = 0, prng, side = "two",
                           pvar = "equal", mt, dig = 4, xlab) {
  if (length(xb1) > 1) {
    indata <- TRUE
    n1 <- length(xb1)
    n2 <- length(xb2)
    sig1 <- sd(xb1)
    sig2 <- sd(xb2)
    xb1 <- mean(xb1)
    xb2 <- mean(xb2)
    cat(
      "xb1 =", round(xb1, dig), "\t xb2 =",
      round(xb2, dig), "\n"
    )
    cat(
      "std1 =", round(sig1, dig), "\t std2 =",
      round(sig2, dig), "\n"
    )
  }
  else {
    indata <- FALSE
  }
  if (grepl(pvar, "known")) {
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    xd <- xb1 - xb2
    z0 <- (xd - d0) / se
    cat(paste0(
      "Z0 = (", round(xb1, dig), " - ",
      round(xb2, dig), " - ", d0, ") / √(",
      round(s1^2, dig), "/", n1, " + ", round(
        s2^2,
        dig
      ), "/", n2, ") = ", round(
        z0,
        dig
      )
    ), "\n")
    xmax <- max(abs(xd), 4)
    if (missing(prng)) {
      prng <- c(d0 - xmax * se, d0 + xmax * se)
    }
    if (missing(mt)) {
      mt <- bquote(bold("Distribution of the Mean Difference under H0: ") ~
      N(.(d0) ~ "," ~ .(round(se, 3))^2))
    }
    if (missing(xlab)) {
      xlab <- "Difference of Sample Means"
    }
    xa <- seq(prng[1], prng[2], length.out = 100)
    dev.new(7, 5)
    plot(xa, dnorm(xa, d0, se),
      type = "n", xlab = xlab,
      ylab = "pdf", ylim = c(-0.1, 1) * max(dnorm(
        xa,
        d0, se
      )), main = mt
    )
    if (side == "up" | grepl(side, "greater")) {
      pv <- pnorm(xd, d0, se, lower.tail = FALSE)
      cat("P-v = P(Z > Z0) =", round(pv, dig), "\n")
      cord.x <- c(
        xd, seq(xd, prng[2], length.out = 20),
        prng[2]
      )
      cord.y <- c(0, dnorm(
        seq(xd, prng[2], length.out = 20),
        d0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(xd, 0, xd, dnorm(xd, d0, se), lwd = 2, col = 2)
      text(xd, dnorm(xd, d0, se) * 0.9, round(pv, 4),
        pos = 4,
        col = 2
      )
      text(xd, 0, round(xd, 4), pos = 1, col = 4)
    }
    else if (side == "low" | grepl(side, "less")) {
      pv <- pnorm(xd, d0, se)
      cat("P-v = P(Z < Z0) =", round(pv, dig), "\n")
      cord.x <- c(
        prng[1], seq(prng[1], xd, length.out = 20),
        xd
      )
      cord.y <- c(0, dnorm(
        seq(prng[1], xd, length.out = 20),
        d0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(xd, 0, xd, dnorm(xd, d0, se), lwd = 2, col = 2)
      text(xd, dnorm(xd, d0, se) * 0.9, round(pv, 4),
        pos = 2,
        col = 2
      )
      text(xd, 0, round(xd, 4), pos = 1, col = 4)
    }
    else if (grepl(side, "two sided")) {
      mlow <- ifelse(xd > d0, 2 * d0 - xd, xd)
      mup <- ifelse(xd > d0, xd, 2 * d0 - xd)
      pv <- 2 * pnorm(mlow, d0, se)
      cat(
        "P-v = 2 × P(Z > |Z0|) =", round(pv, dig),
        "\n"
      )
      cord.x <- c(
        mup, seq(mup, prng[2], length.out = 20),
        prng[2]
      )
      cord.y <- c(0, dnorm(
        seq(mup, prng[2], length.out = 20),
        d0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      cord.x <- c(
        prng[1], seq(prng[1], mlow, length.out = 20),
        mlow
      )
      cord.y <- c(0, dnorm(
        seq(prng[1], mlow, length.out = 20),
        d0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(c(mlow, mup), 0, c(mlow, mup), dnorm(
        xd,
        d0, se
      ), lwd = 2, col = 2)
      text(c(mlow, mup), dnorm(xd, d0, se) * 0.9, round(
        pv / 2,
        4
      ), pos = c(2, 4), col = 2)
      text(c(mlow, mup), 0, round(c(mlow, mup), 4),
        pos = 1,
        col = 4
      )
    }
    abline(h = 0)
    abline(v = d0, lty = 2, lwd = 2, col = "green3")
    lines(xa, dnorm(xa, d0, se),
      type = "l", lwd = 2,
      col = 4
    )
    invisible(list(stat = z0, pval = pv))
  }
  else if (grepl(pvar, "equal")) {
    if (indata) {
      s1 <- sig1
      s2 <- sig2
    }
    sp2 <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 -
      2)
    se <- sqrt(sp2 * (1 / n1 + 1 / n2))
    xd <- xb1 - xb2
    t0 <- (xd - d0) / se
    cat(
      "Sp² =", round(sp2, dig), "\t s.e.=",
      round(se, dig), "\n"
    )
    cat(paste0(
      "T0 = (", round(xb1, dig), " - ",
      round(xb2, dig), " - ", d0, ") / √(",
      round(sp2, dig), " × (1/", n1, "+1/",
      n2, ")) = ", round(t0, dig)
    ), "\n")
    df <- n1 + n2 - 2
    xmax <- max(abs(t0), 4)
    if (missing(prng)) {
      prng <- c(-xmax, xmax)
    }
    if (missing(mt)) {
      mt <- paste0(
        "Distribution of the Test Statistic under H0: t(",
        round(df, 3), ")"
      )
    }
    if (missing(xlab)) {
      xlab <- "Test Statistic"
    }
    xa <- seq(prng[1], prng[2], length.out = 101)
    dev.new(7, 5)
    plot(xa, dt(xa, df),
      type = "n", xlab = xlab, ylab = "pdf",
      ylim = c(-0.1, 1) * max(dt(xa, df)), main = mt
    )
    if (side == "up" | grepl(side, "greater")) {
      pv <- pt(t0, df, lower.tail = FALSE)
      cat("P-v = P(T > T0) =", round(pv, dig), "\n")
      cord.x <- c(
        t0, seq(t0, prng[2], length.out = 20),
        prng[2]
      )
      cord.y <- c(0, dt(
        seq(t0, prng[2], length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(t0, 0, t0, dt(t0, df), lwd = 2, col = 2)
      text(t0, dt(t0, df) * 0.9, round(pv, dig),
        pos = 4,
        col = 2
      )
      text(t0, 0, round(t0, dig), pos = 1, col = 4)
    }
    else if (side == "low" | grepl(side, "less")) {
      pv <- pt(t0, df)
      cat("P-v = P(T < T0) =", round(pv, dig), "\n")
      cord.x <- c(
        prng[1], seq(prng[1], t0, length.out = 20),
        t0
      )
      cord.y <- c(0, dt(
        seq(prng[1], t0, length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(t0, 0, t0, dt(t0, df), lwd = 2, col = 2)
      text(t0, dt(t0, df) * 0.9, round(pv, dig),
        pos = 2,
        col = 2
      )
      text(t0, 0, round(t0, dig), pos = 1, col = 4)
    }
    else if (grepl(side, "two sided")) {
      mlow <- ifelse(t0 > 0, -t0, t0)
      mup <- ifelse(t0 > 0, t0, -t0)
      pv <- 2 * pt(mlow, df)
      cat(
        "P-v = 2 × P(T > |T0|) =", round(pv, dig),
        "\n"
      )
      cord.x <- c(
        mup, seq(mup, prng[2], length.out = 20),
        prng[2]
      )
      cord.y <- c(0, dt(
        seq(mup, prng[2], length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      cord.x <- c(
        prng[1], seq(prng[1], mlow, length.out = 20),
        mlow
      )
      cord.y <- c(0, dt(
        seq(prng[1], mlow, length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(c(mlow, mup), 0, c(mlow, mup), dt(t0, df),
        lwd = 2, col = 2
      )
      text(c(mlow, mup), dt(t0, df) * 0.9, round(
        pv / 2,
        dig
      ), pos = c(2, 4), col = 2)
      text(c(mlow, mup), 0, round(c(mlow, mup), dig),
        pos = 1,
        col = 4
      )
    }
    abline(h = 0)
    abline(v = 0, lty = 2, lwd = 2, col = "green3")
    lines(xa, dt(xa, df), type = "l", lwd = 2, col = 4)
    invisible(list(stat = t0, df = df, sp2 = sp2, pval = pv))
  }
  else {
    if (indata) {
      s1 <- sig1
      s2 <- sig2
    }
    df <- (s1^2 / n1 + s2^2 / n2)^2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 -
      1))
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    xd <- xb1 - xb2
    t0 <- (xd - d0) / se
    cat(
      "nu* =", round(df, dig), "\t s.e =",
      round(se, dig), "\n"
    )
    cat(paste0(
      "T0 = (", round(xb1, dig), " - ",
      round(xb2, dig), " - ", d0, ") / √(",
      round(s1^2, dig), "/", n1, " + ", round(
        s2^2,
        dig
      ), "/", n2, ") = ", round(
        t0,
        dig
      )
    ), "\n")
    xmax <- max(abs(t0), 4)
    if (missing(prng)) {
      prng <- c(-xmax, xmax)
    }
    if (missing(mt)) {
      mt <- paste0(
        "Distribution of the Test Statistic under H0: t(",
        round(df, 3), ")"
      )
    }
    if (missing(xlab)) {
      xlab <- "Test Statistic"
    }
    xa <- seq(prng[1], prng[2], length.out = 101)
    dev.new(7, 5)
    plot(xa, dt(xa, df),
      type = "n", xlab = xlab, ylab = "pdf",
      ylim = c(-0.1, 1) * max(dt(xa, df)), main = mt
    )
    if (side == "up" | grepl(side, "greater")) {
      pv <- pt(t0, df, lower.tail = FALSE)
      cat("P-v = P(T > T0) =", round(pv, dig), "\n")
      cord.x <- c(
        t0, seq(t0, prng[2], length.out = 20),
        prng[2]
      )
      cord.y <- c(0, dt(
        seq(t0, prng[2], length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(t0, 0, t0, dt(t0, df), lwd = 2, col = 2)
      text(t0, dt(t0, df) * 0.9, round(pv, dig),
        pos = 4,
        col = 2
      )
      text(t0, 0, round(t0, dig), pos = 1, col = 4)
    }
    else if (side == "low" | grepl(side, "less")) {
      pv <- pt(t0, df)
      cat("P-v = P(T < T0) =", round(pv, dig), "\n")
      cord.x <- c(
        prng[1], seq(prng[1], t0, length.out = 20),
        t0
      )
      cord.y <- c(0, dt(
        seq(prng[1], t0, length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(t0, 0, t0, dt(t0, df), lwd = 2, col = 2)
      text(t0, dt(t0, df) * 0.9, round(pv, dig),
        pos = 2,
        col = 2
      )
      text(t0, 0, round(t0, dig), pos = 1, col = 4)
    }
    else if (grepl(side, "two sided")) {
      mlow <- ifelse(t0 > 0, -t0, t0)
      mup <- ifelse(t0 > 0, t0, -t0)
      pv <- 2 * pt(mlow, df)
      cat(
        "P-v = 2 × P(T > |T0|) =", round(pv, dig),
        "\n"
      )
      cord.x <- c(
        mup, seq(mup, prng[2], length.out = 20),
        prng[2]
      )
      cord.y <- c(0, dt(
        seq(mup, prng[2], length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      cord.x <- c(
        prng[1], seq(prng[1], mlow, length.out = 20),
        mlow
      )
      cord.y <- c(0, dt(
        seq(prng[1], mlow, length.out = 20),
        df
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(c(mlow, mup), 0, c(mlow, mup), dt(t0, df),
        lwd = 2, col = 2
      )
      text(c(mlow, mup), dt(t0, df) * 0.9, round(
        pv / 2,
        dig
      ), pos = c(2, 4), col = 2)
      text(c(mlow, mup), 0, round(c(mlow, mup), dig),
        pos = 1,
        col = 4
      )
    }
    abline(h = 0)
    abline(v = 0, lty = 2, lwd = 2, col = "green3")
    lines(xa, dt(xa, df), type = "l", lwd = 2, col = 4)
    invisible(list(stat = t0, df = df, se = se, pval = pv))
  }
}
