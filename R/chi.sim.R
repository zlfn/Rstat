#' @title Simulation for the chi-square Distribution
#' @description Simulation for the chi-square Distribution
#' @usage chi.sim(ns, mu = 0, sig = 1, N = 10000, ng = 100, seed = 9857, dig = 4, muknow = TRUE)
#'
#' @param ns Sample size
#' @param mu Expected value, Default: 0
#' @param sig Standard deviation, Default: 1
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param muknow Logical value for known expected value, Default: TRUE
#'
#' @return None.
#' @examples
#' chi.sim(ns = 10, mu = 100, sig = 10)
#' @export

chi.sim <- function(ns, mu = 0, sig = 1, N = 10000, ng = 100, seed = 9857,
                    dig = 4, muknow = TRUE) {
  set.seed(seed)
  cs <- NULL
  if (muknow) {
    for (k in 1:N) cs <- c(cs, sum((rnorm(ns, mu, sig) - mu)^2) / sig^2)
  }
  else {
    for (k in 1:N) {
      sam <- rnorm(ns, mu, sig)
      xb <- mean(sam)
      cs <- c(cs, sum((sam - xb)^2) / sig^2)
    }
  }
  nu0 <- ifelse(muknow, ns, ns - 1)
  nu1 <- ifelse(muknow, ns - 1, ns)
  svd0 <- function(x) dchisq(x, nu0)
  svd1 <- function(x) dchisq(x, nu1)
  Ec <- round(mean(cs), dig)
  Dc <- round(sd(cs), dig)
  Dc0 <- round(sqrt(2 * nu0), dig)
  Dc1 <- round(sqrt(2 * nu1), dig)
  cp <- seq(0, ceiling(max(cs)), by = 5)
  Theory <- pchisq(cp, nu0)
  Error <- pchisq(cp, nu1)
  Simula <- sapply(cp, function(x) sum(cs < x)) / N
  cdf <- rbind(Simula, Theory, Error)
  colnames(cdf) <- paste0("F(", cp, ")")
  print(round(cdf, dig))
  x1 <- 0
  x2 <- ceiling(max(cs))
  win.graph(7, 5)
  mt <- ifelse(muknow, "Distribution of Standardized Sum of Squares",
    "Dist. of Sum of Squares with Unknown Mean"
  )
  hist(cs,
    breaks = ng, prob = T, col = 7, xlim = c(x1, x2),
    ylab = "f(x)", xlab = "x", main = paste0(
      mt,
      " (n=", ns, ", N=", N, ")"
    )
  )
  curve(svd0, x1, x2, lwd = 2, col = 2, add = T)
  curve(svd1, x1, x2, lwd = 2, col = 4, add = T)
  legend("right", c(
    "Para.   Exact    Error    Simul.",
    paste(
      "E(X)      ", nu0, "      ", nu1, "     ",
      Ec
    ), paste(
      "D(X)", Dc0, " ", Dc1, " ",
      Dc
    )
  ), text.col = c(1, 4, 4))
  leg <- list()
  leg[[1]] <- bquote(chi^2 ~ (.(nu0)))
  leg[[2]] <- bquote(chi^2 ~ (.(nu1)))
  legend("topright", sapply(leg, as.expression), lwd = c(
    2,
    2
  ), col = c(2, 4))
}
