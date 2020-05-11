#' @title Manual for Ch9. Functions
#' @description Ch9. Distributions of Sample Statistics
#' @usage ch9.man(fn = 0)
#' @param fn Function number (0~6), Default: 0
#' @return None.
#' @examples
#' ch9.man()
#' ch9.man(5)
#' @export

ch9.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] norm.sim\t\tSimulation for the Normal Distribution\n")
    cat("[2] norm.spn\t\tMinimum Number of Samples from Normal Population\n")
    cat("[3] tdist.sim\t\tSimulation for the t-distribution\n")
    cat("[4] chi.sim\t\tSimulation for the chi-square Distribution\n")
    cat("[5] fdist.sim2\t\tSimulation for the F-distribution\n")
    cat("[6] clt.plot\t\tDiagnose the Central Limit Theorem\n")
  }
  if (1 %in% fn) {
    cat("[1] Simulation for the Normal Distribution\n")
    cat("norm.sim(ns, mu=0, sig=1, N=10000, ng=50, seed=9857, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("ns\t Sample size\n")
    cat("[Optional Input]--------------------------\n")
    cat("mu\t Expected value (default=0)\n")
    cat("sig\t Standard deviation (default=1)\n")
    cat("N\t Number of iterations (default=10000)\n")
    cat("ng\t Number of classes in histogram (default=50)\n")
    cat("seed\t Seed value for generating random numbers (default=9857)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (2 %in% fn) {
    cat("[2] Minimum Number of Samples from Normal Population\n")
    cat("norm.spn(kp, alp, lo=0.1, up=1, mt, dcol, log=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("kp\t Error limit in multiples of the standard deviation\n")
    cat("alp\t Level of significance vector\n")
    cat("[Optional Input]--------------------------\n")
    cat("lo\t Lower limit of the error limit (default=0.1)\n")
    cat("up\t Upper limit of the error limit (default=1)\n")
    cat("mt\t Plot title\n")
    cat("dcol\t Line color (default=rainbow())\n")
    cat("log\t Logical value for log-scaling y-axis (default=TRUE)\n")
  }
  if (3 %in% fn) {
    cat("[3] Simulation for the t-distribution\n")
    cat("tdist.sim(ns, mu=0, sig=1, N=10000, ng=100, seed=9857, dig=4, mt)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("ns\t Sample size\n")
    cat("[Optional Input]--------------------------\n")
    cat("mu\t Expected value (default=0)\n")
    cat("sig\t Standard deviation (default=1)\n")
    cat("N\t Number of iterations (default=10000)\n")
    cat("ng\t Number of classes in histogram (default=100)\n")
    cat("seed\t Seed value for generating random numbers (default=9857)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("mt\t Plot title\n")
  }
  if (4 %in% fn) {
    cat("[4] Simulation for the chi-square Distribution\n")
    cat("chi.sim(ns, mu=0, sig=1, N=10000, ng=100, seed=9857, dig=4, muknow=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("ns\t Sample size\n")
    cat("[Optional Input]--------------------------\n")
    cat("mu\t Expected value (default=0)\n")
    cat("sig\t Standard deviation (default=1)\n")
    cat("N\t Number of iterations (default=10000)\n")
    cat("ng\t Number of classes in histogram (default=100)\n")
    cat("seed\t Seed value for generating random numbers (default=9857)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("muknow\t Logical value for known expected value (default=TRUE)\n")
  }
  if (5 %in% fn) {
    cat("[5] Simulation for the F-distribution\n")
    cat("fdist.sim2(sig1, sig2, n1, n2, N=10000, ng=300, seed=9857, xp=1:9, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("sig1\t Standard deviation of the first population\n")
    cat("sig2\t Standard deviation of the second population\n")
    cat("n1\t Sample size of the first population\n")
    cat("n2\t Sample size of the second population\n")
    cat("[Optional Input]--------------------------\n")
    cat("N\t Number of iterations (default=10000)\n")
    cat("ng\t Number of classes in histogram (default=300)\n")
    cat("seed\t Seed value for generating random numbers (default=9857)\n")
    cat("xp\t Specific x-values for cumulative probability F(x)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (6 %in% fn) {
    cat("[6] Diagnose the Central Limit Theorem\n")
    cat("clt.plot(dist, para, para2, ns=c(10,30,50), d=rep(0.5, 3), N=10000, seed=9857, \n\t\tsigknow=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("dist\t Name of population distribution (one of the follows)\n")
    cat("\t (\"exp\",\"gamma\",\"weibull\",\"beta\",\"norm\",\n\t\t\"t\",\"chisq\",\"f\",\"pois\",\"binom\")\n")
    cat("para\t Parameter for the first population\n")
    cat("para2\t Parameter for the second population (if necessary)\n")
    cat("[Optional Input]--------------------------\n")
    cat("ns\t Sample size (default=c(10,30,50))\n")
    cat("d\t Group width in histogram (default=rep(0.5, 3))\n")
    cat("N\t Number of iterations (default=10000)\n")
    cat("seed\t Seed value for generating random numbers (default=9857)\n")
    cat("sigknow\t Logical value for known population variance (default=TRUE)\n")
  }
}
