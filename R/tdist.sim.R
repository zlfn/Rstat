#' @title Simulation for the t-distribution
#' @description Simulation for the t-distribution
#' @usage tdist.sim(ns, mu = 0, sig = 1, N = 10000, ng = 100, seed = 9857, dig = 4, mt)
#' @param ns Sample size
#' @param mu Expected value, Default: 0
#' @param sig Standard deviation, Default: 1
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 100
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#' @param mt Plot title
#'
#' @return None.
#' @examples
#' tdist.sim(ns = 10, mu = 100, sig = 10)
#' @export

tdist.sim <-
  function(ns, mu = 0, sig = 1, N = 10000, ng = 100, seed = 9857,
           dig = 4, mt) {
    if (missing(mt)) {
      mt <- paste0(
        "Distribution of Standardized Sample Mean (n=",
        ns, ", N=", N, ")"
      )
    }
    set.seed(seed)
    xb <- ss <- NULL
    for (k in 1:N) {
      sam <- rnorm(ns, mu, sig)
      xb <- c(xb, mean(sam))
      ss <- c(ss, sd(sam))
    }
    zb <- (xb - mu) / ss * sqrt(ns)
    smd <- function(x) dt(x, ns - 1)
    Ez <- round(mean(zb), dig)
    Dz <- round(sd(zb), dig)
    Dt <- ifelse(ns > 3, round(sqrt((ns - 1) / (ns - 3)), dig),
      Inf
    )
    zp <- -3:3
    Theory <- pt(zp, ns - 1)
    Simula <- sapply(zp, function(x) sum(zb < x)) / N
    cdf <- rbind(Theory, Simula)
    colnames(cdf) <- paste0("F(", zp, ")")
    print(round(cdf, dig))
    x1 <- -5
    x2 <- 5
    dev.new(7, 5)
    hist(zb,
      breaks = ng, prob = T, col = 7, xlim = c(x1, x2),
      ylab = "f(t)", xlab = "t", main = mt
    )
    curve(dnorm, x1, x2, lwd = 2, col = 4, add = T)
    curve(smd, x1, x2, lwd = 2, col = 2, add = T)
    legend("topright", c(
      "Para.  Exact  Simul.",
      paste("E(T)      ", 0, "      ", Ez), paste(
        "D(T) ",
        Dt, " ", Dz
      )
    ), text.col = c(1, 4, 4))
    legend("topleft", c(
      paste0("t(", ns - 1, ")"),
      "N(0,1)"
    ), lwd = c(2, 2), col = c(2, 4))
  }
