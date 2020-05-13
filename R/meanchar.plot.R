#' @title Operating Characteristic Curves of the Test for a Mean
#' @description Operating Characteristic Curves of the Test for a Population Mean
#' @usage meanchar.plot(mu0, mu1, sig, nv, alp = 0.05, prng, side = "two", mt, dig = 4)
#'
#' @param mu0 Population mean value under the null hypothesis
#' @param mu1 Vector of population mean values for which the power should be calculated
#' @param sig Population standard deviation
#' @param nv Sample size vector
#' @param alp Level of significance, Default: 0.05
#' @param prng Range of x-axis
#' @param side Type of the alternative hypothesis ("up", "low", "two"), Default: 'two'
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' n <- c(10, 30, 50, 100)
#' meanchar.plot(mu0 = 100, mu1 = 98, sig = 5, nv = n, side = "low")
#' meanchar.plot(mu0 = 100, mu1 = 98, sig = 5, nv = n, side = "two")
#' @export
meanchar.plot <- function(mu0, mu1, sig, nv, alp = 0.05, prng, side = "two",
                          mt, dig = 4) {
  nside <- grep(side, c("low", "up", "two"))
  nn <- length(nv)
  se0 <- sig / sqrt(min(nv))
  if (missing(prng)) {
    prng <- switch(nside, c(mu0 - 4 * se0, mu0), c(mu0, mu0 +
      4 * se0), c(mu0 - 4 * se0, mu0 + 4 * se0))
  }
  x1 <- prng[1] - (prng[2] - prng[1]) * 0.15
  if (missing(mt)) {
    mt <- bquote(bold("OC Curve") ~ ~ 1 - psi(mu) ~ ~ (mu[0] ~
    "=" ~ .(mu0) ~ "," ~ sigma ~ "=" ~
    .(sig)))
  }
  char1 <- function(n, mu) {
    1 - pnorm(-qnorm(1 - alp) + n^0.5 *
      (mu0 - mu) / sig)
  }
  char2 <- function(n, mu) {
    pnorm(qnorm(1 - alp) + n^0.5 * (mu0 -
      mu) / sig)
  }
  char3 <- function(n, mu) {
    -pnorm(-qnorm(1 - alp / 2) + n^0.5 * (mu0 - mu) / sig) +
      pnorm(qnorm(1 - alp / 2) + n^0.5 * (mu0 - mu) / sig)
  }
  if (nside == 1) {
    char <- char1(nv, mu1)
  }
  else if (nside == 2) {
    char <- char2(nv, mu1)
  }
  else if (nside == 3) {
    char <- char3(nv, mu1)
  }
  names(char) <- nv
  print(round(char, dig))
  xa <- seq(prng[1], prng[2], length.out = 100)
  win.graph(7, 5)
  if (nn > 5) {
    dcol <- rainbow(nn)
  }
  else {
    dcol <- c(2, 4, "green4", "purple", 6)
  }
  if (nside == 1) {
    plot(xa, char1(nv[1], xa), type = "n", ylim = c(
      0,
      1
    ), xlim = c(x1, prng[2]), main = mt, ylab = expression(1 -
      psi(mu)), xlab = expression(mu))
    for (i in 1:nn) {
      lines(xa, char1(nv[i], xa),
        lwd = 2,
        col = dcol[i]
      )
    }
    grid(col = 3)
    abline(h = 1 - alp, lty = 2, col = 2)
    text(mu0 - (prng[2] - prng[1]) * 0.05, 1 - alp, labels = bquote(alpha ==
      .(alp)), col = 2, pos = 3)
    if (!(missing(mu1))) {
      abline(v = mu1, lty = 2, col = 4)
      segments(prng[1], char1(nv, mu1), mu1, char1(
        nv,
        mu1
      ), lty = 2, col = 4)
      text(x1, char1(nv, mu1), labels = round(char1(
        nv,
        mu1
      ), 3), pos = 4, col = 2)
    }
    legend("top", paste0("n=", nv),
      col = dcol[1:nn],
      lwd = 2, bg = "white"
    )
  }
  else if (nside == 2) {
    plot(xa, char2(nv[1], xa), type = "n", ylim = c(
      0,
      1
    ), xlim = c(x1, prng[2]), main = mt, ylab = expression(1 -
      psi(mu)), xlab = expression(mu))
    for (i in 1:nn) {
      lines(xa, char2(nv[i], xa),
        lwd = 2,
        col = dcol[i]
      )
    }
    grid(col = 3)
    abline(h = 1 - alp, lty = 2, col = 2)
    text(mu0, 1 - alp,
      labels = bquote(alpha == .(alp)),
      col = 2, pos = 3
    )
    legend("right", paste0("n=", nv),
      col = dcol[1:nn],
      lwd = 2, bg = "white"
    )
    if (!(missing(mu1))) {
      abline(v = mu1, lty = 2, col = 4)
      segments(prng[1], char2(nv, mu1), mu1, char2(
        nv,
        mu1
      ), lty = 2, col = 4)
      text(x1, char2(nv, mu1), labels = round(char2(
        nv,
        mu1
      ), 3), pos = 4, col = 2)
    }
  }
  else if (nside == 3) {
    plot(xa, char3(nv[1], xa), type = "n", ylim = c(
      0,
      1
    ), xlim = c(x1, prng[2]), main = mt, ylab = expression(1 -
      psi(mu)), xlab = expression(mu))
    for (i in 1:nn) {
      lines(xa, char3(nv[i], xa),
        lwd = 2,
        col = dcol[i]
      )
    }
    grid(col = 3)
    abline(h = 1 - alp, lty = 2, col = 2)
    text(mu0, 1 - alp,
      labels = bquote(alpha == .(alp)),
      col = 2, pos = 3
    )
    legend("right", paste0("n=", nv),
      col = dcol[1:nn],
      lwd = 2, bg = "white"
    )
    if (!(missing(mu1))) {
      mu2 <- mu0 - (mu1 - mu0)
      abline(v = c(mu1, mu2), lty = 2, col = 4)
      segments(prng[1], char3(nv, mu1), mu1, char3(
        nv,
        mu1
      ), lty = 2, col = 4)
      text(x1, char3(nv, mu1), labels = round(char3(
        nv,
        mu1
      ), 3), pos = 4, col = 2)
    }
  }
}
