#' @title Diagnose the Central Limit Theorem
#' @description Diagnose the Central Limit Theorem
#' @usage clt.plot(dist, para, para2, ns = c(10, 30, 50), d = rep(0.5, 3), N = 10000, seed = 9857, sigknow = TRUE)
#'
#' @param dist Name of population distribution ("exp","gamma","weibull","beta","norm", "t","chisq","f","pois","binom")
#' @param para Parameter for the first population
#' @param para2 Parameter for the second population (if necessary)
#' @param ns Sample size, Default: c(10, 30, 50)
#' @param d Group width in histogram, Default: rep(0.5, 3)
#' @param N Number of iterations, Default: 10000
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param sigknow Logical value for known population variance, Default: TRUE
#'
#' @return None.
#' @examples
#' clt.plot("exp", para = 5, d = rep(0.4, 3))
#' clt.plot("bin", para = 0.1, ns = nv, d = c(1, 0.6, 0.5))
#' @export
clt.plot <- function(dist, para, para2, ns = c(10, 30, 50), d = rep(
                       0.5,
                       3
                     ), N = 10000, seed = 9857, sigknow = TRUE) {
  dlist <- c(
    "exp", "gamma", "weibull", "beta",
    "norm", "t", "chisq", "f", "pois",
    "binom"
  )
  dname <- c(
    "Exp", "Gam", "Wei", "Beta",
    "Norm", "T", "Chisq", "F", "Poi",
    "Binom"
  )
  if (missing(dist)) {
    cat(paste(dlist, collapse = ", "), "\n")
    stop("Input one of the distribution above....")
  }
  dnum <- grep(dist, dlist)
  dnum <- dnum[length(dnum)]
  mt <- ifelse(dnum %in% c(1, 6, 7, 9, 10), paste0(
    dname[dnum],
    "(", para, ")"
  ), paste0(
    dname[dnum], "(",
    para, ",", para2, ")"
  ))
  if (dnum == 10) {
    mt <- paste0(dname[dnum], "(n,", para, ")")
  }
  m <- length(ns)
  zs <- list()
  for (k in 1:m) {
    zs[[k]] <- genstat(
      dist, para, para2, ns[k],
      N, seed, sigknow
    )
  }
  testplot(zs, d, mt, n = ns)
}
