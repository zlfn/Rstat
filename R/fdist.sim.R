#' @title Simulation of the F-distribution
#' @description Simulation of the F-distribution
#' @usage fdist.sim(nu1 = 5, nu2 = 5, N = 10000, ng = 250, seed = 9857, xp = 1:9, dig = 4)
#' @param sig1 Standard deviation of the first population
#' @param sig2 Standard deviation of the second population
#' @param n1 Sample size of the first population
#' @param n2 Sample size of the second population
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 300
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param xp Specific x-values for cumulative probability F(x), Default: 1:9
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' fdist.sim2(sig1 = 2, sig2 = 7, n1 = 8, n2 = 6)
#' @export

fdist.sim <- function(nu1 = 5, nu2 = 5, N = 10000, ng = 250, seed = 9857,
                      xp = 1:9, dig = 4) {
  set.seed(seed)
  dat1 <- rchisq(N, nu1)
  dat2 <- rchisq(N, nu2)
  fs1 <- (dat1 / nu1) / (dat2 / nu2)
  fd1 <- function(x) df(x, nu1, nu2)
  xmax <- max(xp, qf(0.99, nu1, nu2))
  Ex1 <- round(mean(fs1), dig)
  Dx1 <- round(sd(fs1), dig)
  Ex2 <- ifelse(nu2 > 2, round(nu2 / (nu2 - 2), dig), Inf)
  Dx2 <- ifelse(nu2 > 4, round(sqrt(2 * nu2^2 * (nu1 + nu2 -
    2) / nu1 / (nu2 - 2)^2 / (nu2 - 4)), dig), ifelse(nu2 > 2,
    Inf, NA
  ))
  dev.new(7, 5)
  hist(fs1,
    breaks = ng, prob = T, xlim = c(0, xmax), col = 7,
    main = bquote("(" ~ chi[.(nu1)]^2 ~ "/" ~
    .(nu1) ~ ") / (" ~ chi[.(nu2)]^2 ~ "/" ~
    .(nu2) ~ ")  ~  F(" ~ .(nu1) ~ "," ~
    .(nu2) ~ ")"), ylab = "f(x)", xlab = "x"
  )
  curve(fd1, 0, xmax, lwd = 2, col = 2, add = T)
  legend("topright", c(
    "Para.  Exact   Simul.",
    paste("E(X) ", Ex2, Ex1), paste(
      "D(X) ",
      Dx2, Dx1
    )
  ), text.col = c(1, 4, 4))
  Theory <- pf(xp, nu1, nu2)
  Simula <- sapply(xp, function(x) sum(fs1 < x)) / N
  cdf <- rbind(Theory, Simula)
  colnames(cdf) <- paste0("F(", xp, ")")
  print(round(cdf, dig))
}
