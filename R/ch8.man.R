#' @title Manual for Ch.8
#' @description Ch8. Normal and Related Distributions
#' @usage ch8.man(fn = 0)
#' @param fn Function number (0~11), Default: 0
#' @return None.
#' @examples
#' ch8.man()
#' ch8.man(1)
#' @export
ch8.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] cont.spdf\t\tPlot the PDF of Continuous Random Variables (separate)\n")
    cat("[2] norm.trans\t\tCheck Probability Conservation in Standardizing the Normal Distribution\n")
    cat("[3] snorm.cdf\t\tPlot Standard Normal Cumulative Probability P(Z<z)\n")
    cat("[4] snorm.prob\t\tCentral Probability of the Standard Normal Distribution\n")
    cat("[5] snorm.quant \tQuantile Plot of the Standard Normal Distribution\n")
    cat("[6] chi.prob\t\tCumulative Probability of the Chi-square Distribution\n")
    cat("[7] chi.quant\t\tQuantile Plot of the Chi-square Distribution\n")
    cat("[8] tnorm.comp\t\tPlot Central Probability P(-k<X<k)\n")
    cat("[9] fdist.sim\t\tSimulation of the F-distribution\n")
    cat("[10] f.prob\t\tCumulative Probability of the F-distribution\n")
    cat("[11] f.quant\t\tQuantile Plot of the F-distribution\n")
  }
  if (1 %in% fn) {
    cat("[1] Plot the PDF of Continuous Random Variables (separate)\n")
    cat("cont.spdf(dist, lo, up, para, para2, ymax, xl, yl, dcol, np=100, xp)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("dist\t Distribution name ")
    cat("(\"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", \"t\", \"chisq\", \"f\")\n")
    cat("lo\t Lower limit of x-axis\n")
    cat("up\t Upper limit of x-axis\n")
    cat("para\t First parameter vector of the distribution\n")
    cat("para2\t Second parameter vector (except \"exp\", \"t\", \"chisq\")\n")
    cat("[Optional Input]--------------------------\n")
    cat("ymax\t Upper limit of y-axis\n")
    cat("xl\t Label vector of x-axis\n")
    cat("yl\t Label vector of y-axis\n")
    cat("dcol\t Graph color vector\n")
    cat("np\t Number of plot points(default=100)\n")
    cat("xp\t Location vector for vertical lines\n")
  }
  if (2 %in% fn) {
    cat("[2] Check Probability Conservation in Standardizing the Normal Distribution\n")
    cat("norm.trans(mu, sig, a, b, mt1, mt0, dig=4, span=3, np=100)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("mu\t Mean of the normal distribution\n")
    cat("sig\t Standard deviation of the normal distribution\n")
    cat("a\t Lower limit of X for calculating probability P(a<X<b)\n")
    cat("b\t Upper limit of X for calculating probability P(a<X<b)\n")
    cat("[Optional Input]--------------------------\n")
    cat("mt1\t Title of the normal distribution probability plot\n")
    cat("mt0\t Title of the standard normal distribution probability plot\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("span\t Range of x-axis (mu ± span × sig) (default=3)\n")
    cat("np\t Number of plotting points (default=100)\n")
  }
  if (3 %in% fn) {
    cat("[3] Plot Standard Normal Cumulative Probability P(Z<z)\n")
    cat("snorm.cdf(zp, lo=-4, up=4, mt, dig=4)\n")
    cat("[Optional Input]--------------------------\n")
    cat("zp\t Vector of z-axis values (default=-2:2)\n")
    cat("lo\t Lower limit of z-axis (default=-4)\n")
    cat("up\t Upper limit of z-axis (default=4)\n")
    cat("mt\t Graph title (default=\"Cumulative Probabilities of the Standard Normal Distribution\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (4 %in% fn) {
    cat("[4] Central Probability of the Standard Normal Distribution\n")
    cat("snorm.prob(zp, lo=-4, up=4, mt, dig=4)\n")
    cat("[Optional Input]--------------------------\n")
    cat("zp\t Vector of z-axis values (default=1:4)\n")
    cat("lo\t Lower limit of z-axis (default=-4)\n")
    cat("up\t Upper limit of z-axis (default=4)\n")
    cat("mt\t Graph title (default=\"Central Probability of the Standard Normal Distribution\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (5 %in% fn) {
    cat("[5] Quantile Plot of the Standard Normal Distribution\n")
    cat("snorm.quant(pv, pv2, mt, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("pv\t Vector of probability values\n")
    cat("pv2\t Vector of specific probability values\n")
    cat("[Optional Input]--------------------------\n")
    cat("mt\t Graph title (default=\"Quantiles of the Standard Normal Distribution\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (6 %in% fn) {
    cat("[6] Cumulative Probability of the Chi-square Distribution\n")
    cat("chi.prob(nu, xp, pup=0.995, mt, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("nu\t Degree of freedom of the chi-square distribution\n")
    cat("xp\t Vector of specific x-axis values\n")
    cat("[Optional Input]--------------------------\n")
    cat("pup\t Upper limit of probabilities for quantiles (default=0.995)\n")
    cat("mt\t Graph title\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (7 %in% fn) {
    cat("[7] Quantile Plot of the Chi-square Distribution\n")
    cat("chi.quant(nu, pv, pv2=pv, pup=0.999, mt, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("nu\t Degree of freedom of the chi-square distribution\n")
    cat("pv\t Vector of probabilities for quantiles\n")
    cat("[Optional Input]--------------------------\n")
    cat("pv2\t Vector of probabilities for specific quantiles (default=pv)\n")
    cat("pup\t Upper limit of probabilities for quantiles (default=0.999)\n")
    cat("mt\t Graph title\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (8 %in% fn) {
    cat("[8] Compare T-Dist. with the Standard Normal\n")
    cat("tnorm.comp(nu=c(10, 30), lo=-3.5, up=3.5, dig=4, dcol)\n")
    cat("[Optional Input]--------------------------\n")
    cat("nu\t Degree of freedom for the chi-sq. dist. (default=c(10,30))\n")
    cat("lo\t Lower limit of x-axis (default=-3.5)\n")
    cat("up\t Upper limit of x-axis(default=3.5)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("dcol\t Color of plot lines\n")
  }
  if (9 %in% fn) {
    cat("[9] Simulation of the F-distribution\n")
    cat("fdist.sim(nu1=5, nu2=5, N=10000, ng=250, seed=9857, xp=1:9, dig=4)\n")
    cat("[Optional Input]--------------------------\n")
    cat("nu1\t Numerator degree of freedom (default=5)\n")
    cat("nu2\t Denominator degree of freedom (default=5)\n")
    cat("N\t Number of random values (default=10000)\n")
    cat("ng\t Number of classes in histogram (default=250)\n")
    cat("seed\t Seed value for random number generator (default=9857)\n")
    cat("xp\t Vector of x-axis values (default=1:9)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (10 %in% fn) {
    cat("[10] Cumulative Probability of the F-distribution\n")
    cat("f.prob(nu1, nu2, xp, mt, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("nu1\t Numerator degree of freedom (default=5)\n")
    cat("nu2\t Denominator degree of freedom (default=5)\n")
    cat("xp\t Vector of x-axis values\n")
    cat("[Optional Input]--------------------------\n")
    cat("mt\t Graph title\n")
    cat("dig=4\t Number of digits below the decimal point (default=4)\n")
  }
  if (11 %in% fn) {
    cat("[11] Quantile Plot of the F-distribution\n")
    cat("f.quant(nu1, nu2, pv, pv2=pv, pup=0.995, mt, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("nu1\t Numerator degree of freedom (default=5)\n")
    cat("nu2\t Denominator degree of freedom (default=5)\n")
    cat("pv\t Vector of probabilities for quantiles\n")
    cat("[Optional Input]--------------------------\n")
    cat("pv2\t Vector of probabilities for specific quantiles (default=pv)\n")
    cat("pup\t Upper limit of probabilities for quantiles (default=0.995)\n")
    cat("mt\t Graph title\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
}
