#' @title Manual for Ch11. Functions
#' @description Ch11. Inference on Two Populations
#' @usage ch11.man(fn = 0)
#' @param fn Function number (0~8), Default: 0
#' @return None.
#' @examples
#' ch11.man()
#' ch11.man(1)
#' @export

ch11.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] mean2.ci\t\tConfidence Interval for the Difference of Two Population Means\n")
    cat("[2] mean2test.plot \tHypothesis Test for the Difference of Two Population Means\n")
    cat("[3] normtest.plot  \tPlot the PDF of Test Statistic with the Normal Distribution\n")
    cat("[4] ttest.plot\t\tPlot the PDF of Test Statistic with the T-distribution\n")
    cat("[5] prob2.ci\t\tConfidence Interval for the Difference of Population Ratios\n")
    cat("[6] prob2test.plot  \tTest for the Difference of Population Ratios (Large Sample)\n")
    cat("[7] ftest.plot\t\tPlot the PDF of the F-test Statistic\n")
    cat("[8] civar2.sim\t\tSimulate the Confidence Interval for a the ratio of Population Variances\n")
  }
  if (1 %in% fn) {
    cat("[1] Confidence Interval for the Difference of Two Population Means\n")
    cat("mean2.ci(xb1, xb2, s1, s2, n1, n2, pvar=\"equal\", alp=0.05, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("xb1\t Sample mean of population1 (or sample data)\n")
    cat("xb2\t Sample mean of population2 (or sample data)\n")
    cat("s1\t Standard deviation of population1 (optional for unknown variance)\n")
    cat("s2\t Standard deviation of population2 (optional for unknown variance)\n")
    cat("n1\t Sample size of population1 (unnecessary if data are given)\n")
    cat("n2\t Sample size of population2 (unnecessary if data are given)\n")
    cat("[Optional Input]--------------------------\n")
    cat("pvar\t Status of variance (one of \"known\", \"equal\", \"unequal\")\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n\n")
  }
  if (2 %in% fn) {
    cat("[2] Hypothesis Test for the Difference of Two Population Means\n")
    cat("mean2test.plot(xb1, xb2, s1, s2, n1, n2, d0=0, prng, side=\"two\", pvar=\"equal\", mt, dig=4, xlab)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("xb1\t Sample mean of population1 (or sample data)\n")
    cat("xb2\t Sample mean of population2 (or sample data)\n")
    cat("s1\t Standard deviation of population1 (optional for unknown variance)\n")
    cat("s2\t Standard deviation of population2 (optional for unknown variance)\n")
    cat("n1\t Sample size of population1 (unnecessary if data are given)\n")
    cat("n2\t Sample size of population2 (unnecessary if data are given)\n")
    cat("[Optional Input]--------------------------\n")
    cat("d0\t Difference of two population means under the null hypothesis (default=0)\n")
    cat("prng\t Range of x-axis (default = d0 ± 4 × se)\n")
    cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
    cat("pvar\t Status of variance (one of \"known\", \"equal\", \"unequal\")\n")
    cat("mt\t Graph title\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("xlab\t Label of x-axis\n")
  }
  if (3 %in% fn) {
    cat("[3] Plot the PDF of Test Statistic with the Normal Distribution\n")
    cat("normtest.plot(md, mu0=0, se=1, prng=c(-4,4), sided=\"two\", xlab=\"Sample Mean\")\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("md\t Difference of sample means or test statistic\n")
    cat("[Optional Input]--------------------------\n")
    cat("m0\t Difference of population means under the null hypothesis (default=0)\n")
    cat("se\t Standard error of the difference of sample means (default=1)\n")
    cat("prng\t Range of x-axis (default = c(-4,4))\n")
    cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
    cat("xlab\t Label of x-axis (default=\"Sample Mean\")\n")
  }
  if (4 %in% fn) {
    cat("[4] Plot the PDF of Test Statistic with the T-distribution\n")
    cat("ttest.plot(md, deg, prng=c(-4,4), sided=\"two\")\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("md\t T-test statistic for the difference of population means\n")
    cat("deg\t Degree of freedom\n")
    cat("[Optional Input]--------------------------\n")
    cat("prng\t Range of x-axis (default = c(-4,4))\n")
    cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
  }
  if (5 %in% fn) {
    cat("[5] Confidence Interval for the Difference of Population Ratios\n")
    cat("prob2.ci(n1, x1, n2, x2, alp=0.05, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("n1\t Sample size of population1\n")
    cat("x1\t Number of successes in samples from population1\n")
    cat("n2\t Sample size of population2\n")
    cat("x2\t Number of successes in samples from population2\n")
    cat("[Optional Input]--------------------------\n")
    cat("alp\t Level of significance (default = 0.05)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (6 %in% fn) {
    cat("[6] Test for the Difference of Population Ratios (Large Sample)\n")
    cat("prob2test.plot(n1, x1, n2, x2, prng, side=\"two\", mt, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("n1\t Sample size of population1\n")
    cat("x1\t Number of successes in samples from population1\n")
    cat("n2\t Sample size of population2\n")
    cat("x2\t Number of successes in samples from population2\n")
    cat("[Optional Input]--------------------------\n")
    cat("prng\t Range of x-axis (default = d0 ± 4*se)\n")
    cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
    cat("mt\t Graph title\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (7 %in% fn) {
    cat("[7] Plot the PDF of the F-test Statistic\n")
    cat("ftest.plot(fstat, deg, pmax=0.995, sided=\"two\")\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("fstat\t F-test statistic for the ratio of two population variances\n")
    cat("deg\t Vector of degree of freedoms\n")
    cat("[Optional Input]--------------------------\n")
    cat("pmax\t Maximum probability for quantiles in x-axis (default=0.995)\n")
    cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
  }
  if (8 %in% fn) {
    cat("[8] Simulate the Confidence Interval for a the ratio of Population Variances\n")
    cat("civar2.sim(n1, n2, sig1, sig2, alp=0.05, N=100, seed=9857, dig=4, plot=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("n1\t Sample size of population1\n")
    cat("n2\t Sample size of population2\n")
    cat("sig1\t Standard deviation of population1\n")
    cat("sig2\t Standard deviation of population2\n")
    cat("[Optional Input]--------------------------\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("N\t Number of iterations (default=100)\n")
    cat("seed\t Seed value for generating random numbers (default=9857)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("plot\t Logical value for plot (default=TRUE)\n")
  }
}
