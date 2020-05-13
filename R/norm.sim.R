#' @title Simulation for the Normal Distribution
#' @description Simulation for the Normal Distribution
#' @usage norm.sim(ns, mu = 0, sig = 1, N = 10000, ng = 50, seed = 9857, dig = 4)

#'
#' @param ns Sample size
#' @param mu Expected value, Default: 0
#' @param sig Standard deviation, Default: 1
#' @param N Number of iterations, Default: 10000
#' @param ng Number of classes in histogram, Default: 50
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#'
#' @return None.
#' @examples
#' norm.sim(ns = 10, mu = 100, sig = 10, N = 10000)
#' @export
norm.sim <-
  function(ns, mu = 0, sig = 1, N = 10000, ng = 50, seed = 9857,
           dig = 4) {
    set.seed(seed)
    xb <- NULL
    for (k in 1:N) xb <- c(xb, mean(rnorm(ns, mu, sig)))
    zb <- (xb - mu) / sig * sqrt(ns)
    popd <- function(x) dnorm(x, mu, sig)
    smd <- function(x) dnorm(x, mu, sig / sqrt(ns))
    Ex1 <- round(mean(xb), dig)
    Dx1 <- round(sd(xb), dig)
    Ex2 <- mu
    Dx2 <- round(sig / sqrt(ns), dig)
    Ez <- round(mean(zb), dig)
    Dz <- round(sd(zb), dig)
    xp <- seq(floor(mu - 3 * sig / sqrt(ns)), ceiling(mu + 3 * sig / sqrt(ns)),
      by = 0.5 * sig
    )
    Theory <- pnorm(xp, mu, sig / sqrt(ns))
    Simula <- sapply(xp, function(x) sum(xb < x)) / N
    cdf <- rbind(Theory, Simula)
    colnames(cdf) <- paste0("F(", xp, ")")
    print(round(cdf, dig))
    win.graph(7, 6)
    par(mfrow = c(2, 1))
    par(mar = c(3, 4, 4, 2))
    x1 <- mu - 3 * sig
    x2 <- mu + 3 * sig
    hist(xb,
      breaks = ng, prob = T, col = 7, xlim = c(x1, x2),
      ylab = "f(x)", xlab = "", main = bquote(bold("Distribution of ") ~
      bar(X)[.(ns)] ~ ~ bold(from) ~ ~ N(.(mu), .(sig)^2))
    )
    curve(popd, x1, x2, col = 4, add = T)
    curve(smd, x1, x2, col = 2, add = T)
    legend("topright", c(
      "Para.  Exact  Simul.",
      paste("E(X) ", Ex2, Ex1, sep = "  "), paste("D(X)",
        Dx2, Dx1,
        sep = "  "
      )
    ), text.col = c(
      1, 4,
      4
    ))
    hist(zb, breaks = 2 * ng, prob = T, col = "cyan", xlim = c(
      -4,
      4
    ), ylab = bquote(phi(z)), xlab = "", main = "Distribution of the Standardized Sample Mean")
    curve(dnorm, -4, 4, col = 2, add = T)
    legend("topright", c(
      "Para.  Exact  Simul.",
      paste("E(Z)    ", 0, "    ", Ez), paste(
        "D(Z)    ",
        1, "    ", Dz
      )
    ), text.col = c(1, 4, 4))
  }
