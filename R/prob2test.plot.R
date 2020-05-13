#' @title Test for the Difference of Ratios (Large Sample)
#' @description Test for the Difference of Population Ratios (Large Sample)
#' @usage prob2test.plot(n1, x1, n2, x2, prng, side = "two", mt, dig = 4)
#' @param n1 Sample size of population1
#' @param x1 Number of successes in samples from population1
#' @param n2 Sample size of population2
#' @param x2 Number of successes in samples from population2
#' @param prng Range of x-axis, Default: (d0-4se, d0+4se)
#' @param side Type of the alternative hypothesis, Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' prob2test.plot(n1 = 150, x1 = 12, n2 = 250, x2 = 10, side = "up", dig = 4)
#' prob2test.plot(n1 = 150, x1 = 12, n2 = 250, x2 = 10, side = "two", dig = 4)
#' @export

prob2test.plot <-
  function(n1, x1, n2, x2, prng, side = "two", mt, dig = 4) {
    p1 <- x1 / n1
    p2 <- x2 / n2
    ph <- (x1 + x2) / (n1 + n2)
    se <- sqrt(ph * (1 - ph) * (1 / n1 + 1 / n2))
    xd <- p1 - p2
    z0 <- xd / se
    cat("p1 =", round(p1, dig), "\t p2 =", round(
      p2,
      dig
    ), "\t ph =", round(ph, dig), "\n")
    cat(paste0(
      "se = √(", round(ph, dig), " × ",
      round(1 - ph, dig), " × (1/", n1, " + 1/",
      n2, ")) =", round(se, dig)
    ), "\n")
    cat(paste0("Z0 = (", round(p1, dig), " - ", round(
      p2,
      dig
    ), ") / ", round(se, dig), " = ", round(
      z0,
      dig
    )), "\n")
    if (missing(prng)) {
      prng <- c(-4 * se, 4 * se)
    }
    if (missing(mt)) {
      mt <- bquote(bold("Distribution of Ratio Difference under H0:") ~
      " N(0," ~ .(round(se, 3))^2 ~ ")")
    }
    xa <- seq(prng[1], prng[2], length.out = 100)
    win.graph(7, 5)
    plot(xa, dnorm(xa, 0, se),
      type = "n", xlab = "Difference of Population Ratios",
      ylab = "pdf", ylim = c(-0.1, 1) * max(dnorm(
        xa,
        0, se
      )), main = mt
    )
    if (side == "up" | grepl(side, "greater")) {
      pv <- 1 - pnorm(z0)
      cat("P-v = P(Z > Z0) =", round(pv, dig), "\n")
      cord.x <- c(xd, seq(xd, prng[2], length.out = 20), prng[2])
      cord.y <- c(0, dnorm(
        seq(xd, prng[2], length.out = 20),
        0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(xd, 0, xd, dnorm(xd, 0, se), lwd = 2, col = 2)
      text(xd, dnorm(xd, 0, se) * 0.9, round(pv, 4),
        pos = 4,
        col = 2
      )
      text(xd, 0, round(xd, 4), pos = 1, col = 4)
    }
    else if (side == "low" | grepl(side, "less")) {
      pv <- pnorm(z0)
      cat("P-v = P(Z < Z0) =", round(pv, dig), "\n")
      cord.x <- c(
        prng[1], seq(prng[1], xd, length.out = 20),
        xd
      )
      cord.y <- c(0, dnorm(
        seq(prng[1], xd, length.out = 20),
        0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(xd, 0, xd, dnorm(xd, 0, se), lwd = 2, col = 2)
      text(xd, dnorm(xd, 0, se) * 0.9, round(pv, 4),
        pos = 2,
        col = 2
      )
      text(xd, 0, round(xd, 4), pos = 1, col = 4)
    }
    else if (grepl(side, "two sided")) {
      mlow <- ifelse(xd > 0, -xd, xd)
      mup <- ifelse(xd > 0, xd, -xd)
      pv <- 2 * (1 - pnorm(abs(z0)))
      cat(
        "P-v = 2 × P(Z > |Z0|) =", round(pv, dig),
        "\n"
      )
      cord.x <- c(mup, seq(mup, prng[2], length.out = 20), prng[2])
      cord.y <- c(0, dnorm(
        seq(mup, prng[2], length.out = 20),
        0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      cord.x <- c(
        prng[1], seq(prng[1], mlow, length.out = 20),
        mlow
      )
      cord.y <- c(0, dnorm(
        seq(prng[1], mlow, length.out = 20),
        0, se
      ), 0)
      polygon(cord.x, cord.y, col = "lightcyan")
      segments(c(mlow, mup), 0, c(mlow, mup), dnorm(
        xd, 0,
        se
      ), lwd = 2, col = 2)
      text(c(mlow, mup), dnorm(xd, 0, se) * 0.9, round(
        pv / 2,
        4
      ), pos = c(2, 4), col = 2)
      text(c(mlow, mup), 0, round(c(mlow, mup), 4),
        pos = 1,
        col = 4
      )
    }
    abline(h = 0)
    abline(v = 0, lty = 2, lwd = 2, col = "green3")
    lines(xa, dnorm(xa, 0, se), type = "l", lwd = 2, col = 4)
  }
