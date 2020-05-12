#' @title Simulation for the F-distribution
#' @description Simulation for the F-distribution
#' @usage fdist.sim2(sig1, sig2, n1, n2, N = 10000, ng = 300, seed = 9857, xp = 1:9, dig = 4)
#' @param nu1 Numerator degree of freedom, Default: 5
#' @param nu2 Denominator degree of freedom, Default: 5
#' @param N Number of random values, Default: 10000
#' @param ng Number of classes in histogram, Default: 250
#' @param seed Seed value for random number generator, Default: 9857
#' @param xp Vector of x-axis values (default=1:9), Default: 1:9
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' fdist.sim(nu1 = 8, nu2 = 5)
#' @export
fdist.sim2 <- function(sig1, sig2, n1, n2, N = 10000, ng = 300, seed = 9857,
                       xp = 1:9, dig = 4) {
  set.seed(seed)
  fs <- NULL
  vratio <- function(n1, n2, s1, s2) {
    var(rnorm(n1, sd = s1)) / s1^2 / (var(rnorm(n2, sd = s2)) / s2^2)
  }
  for (k in 1:N) fs <- c(fs, vratio(n1, n2, sig1, sig2))
  fd0 <- function(x) df(x, n1 - 1, n2 - 1)
  fd1 <- function(x) df(x, n1, n2)
  xmax <- max(xp, qf(0.99, n1 - 1, n2 - 1))
  xmod <- ifelse(n1 > 3, (n1 - 3) / (n1 - 1) * (n2 - 1) / (n2 +
    1), 0)
  ymax <- ifelse(n1 > 3, max(fd0(xmod), fd1(xmod)), 1)
  Ex <- mean(fs)
  Dx <- sd(fs)
  Ex0 <- ifelse(n2 > 3, (n2 - 1) / (n2 - 3), Inf)
  Dx0 <- ifelse(n2 > 5, sqrt(2 * (n2 - 1)^2 * (n1 + n2 - 4) / (n1 -
    1) / (n2 - 3)^2 / (n2 - 5)), ifelse(n2 > 3, Inf, NA))
  Ex1 <- ifelse(n2 > 2, n2 / (n2 - 2), Inf)
  Dx1 <- ifelse(n2 > 4, sqrt(2 * n2^2 * (n1 + n2 - 2) / n1 / (n2 -
    2)^2 / (n2 - 4)), ifelse(n2 > 2, Inf, NA))
  Theory <- pf(xp, n1 - 1, n2 - 1)
  Error <- pf(xp, n1, n2)
  Simula <- sapply(xp, function(x) sum(fs < x)) / N
  cdf <- rbind(Simula, Theory, Error)
  colnames(cdf) <- paste0("F(", xp, ")")
  print(round(cdf, dig))
  win.graph(7, 5)
  hist(fs,
    breaks = ng, prob = T, xlim = c(0, xmax), ylim = c(
      0,
      ymax
    ), col = 7, main = bquote("(" ~ S[1]^2 ~ "/" ~
    sigma[1]^2 ~ ")/(" ~ S[2]^2 ~ "/" ~ sigma[2]^2 ~
    ") ~" ~ F(.(n1 - 1), .(n2 - 1))), ylab = "f(x)",
    xlab = "x"
  )
  curve(fd0, 0, xmax, lwd = 2, col = 2, add = T)
  curve(fd1, 0, xmax, lwd = 2, col = 4, add = T)
  legend("right", c(
    "Para.  Exact   Error  Simul.",
    paste("E(X)", paste(format(c(Ex0, Ex1, Ex), digits = dig),
      collapse = "  "
    )), paste("D(X)", paste(format(c(
      Dx0,
      Dx1, Dx
    ), digits = dig), collapse = "  "))
  ),
  text.col = c(1, 4, 4)
  )
  leg <- list()
  leg[[1]] <- bquote(F(.(n1 - 1), .(n2 - 1)))
  leg[[2]] <- bquote(F(.(n1), .(n2)))
  legend("topright", sapply(leg, as.expression), lwd = c(
    2,
    2
  ), col = c(2, 4))
}
