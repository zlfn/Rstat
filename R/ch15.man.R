#' @title Manual for Ch15. Functions
#' @description Ch15. Nonparametric Methods
#' @usage ch15.man(fn = 0)
#' @param fn Function number (1,2,31,32,4,51,52,61,62,7,8), Default: 0
#' @return None.
#' @examples
#' ch15.man()
#' ch15.man(2)
#' ch15.man(51)
#' @export

cat15.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] norm.diag\t\tInvestigate the Normality of Data\n")
    cat("[2] signtest.plot  \tSign Test (Binomial Test)\n")
    cat("[3-1] runs.dist\t\tDistribution of Run Test Statistic\n")
    cat("[3-2] runstest.plot  \tRun Test with a Plot\n")
    cat("[4] corr.spear\t\tPearson Correlation Coefficient & Spearman Correlation Coefficient\n")
    cat("[5-1] ranksum.dist \tDistribution of Wilcoxon Rank Sum Test Statistic\n")
    cat("[5-2] ranksum.plot \tWilcoxon Rank Sum Test\n")
    cat("[6-1] signrank.dist \tDistribution of Wilcoxon Signed Rank Test Statistic\n")
    cat("[6-2] signrank.plot \tWilcoxon Signed Rank Test\n")
    cat("[7] kruswall.plot   \tKruskal Wallis Test\n")
    cat("[8] friedman.plot \tFriedman Test\n")
  }
  if (1 %in% fn) {
    cat("[1] Investigate the Normality of Data\n")
    cat("norm.diag(x, xrng, by=1, dig=4, dc=c(\"cyan\", 2, 4))\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector\n")
    cat("[Optional Input]--------------------------\n")
    cat("xrng\t Range of x-axis (default=mean ± 3 × stdev)\n")
    cat("by\t Histogram class interval (default=1)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("dc\t Color vector (default=c(\"cyan\", 2, 4))\n")
  }
  if (2 %in% fn) {
    cat("[2] Sign Test (Binomial Test)\n")
    cat("signtest.plot(x, mu0=0, side=\"two\", dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector\n")
    cat("[Optional Input]--------------------------\n")
    cat("mu0\t Mean value under the null hypothesis (default=0)\n")
    cat("side\t Type of alternative hypothesis (default=\"two\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (31 %in% fn) {
    cat("[3-1] Distribution of Runs Test Statistic\n")
    cat("require(randomizeBE)\n")
    cat("runs.dist(n1=2:20, n2=2:20, alp=0.05, tab=TRUE, side=\"two\", plot=FALSE)\n")
    cat("[Optional Input]--------------------------\n")
    cat("n1\t Number of data in group 1 (default=2:20)\n")
    cat("n2\t Number of data in group 2 (default=2:20)\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("tab\t Logical value for printing critical value table (default=TRUE)\n")
    cat("side\t Type of alternative hypothesis (default=\"two\")\n")
    cat("plot\t Logical value for plotting run distribution (default=FALSE)\n\n")
  }
  if (32 %in% fn) {
    cat("[3-2] Runs Test\n")
    cat("require(randomizeBE)\n")
    cat("runstest.plot(x, n1, n2, alp=0.05, side=\"two\", dig=4, plot=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector (or number of runs)\n")
    cat("[Optional Input]--------------------------\n")
    cat("n1\t Number of data in group 1 (required if raw data are not given)\n")
    cat("n2\t Number of data in group 2 (required if raw data are not given)\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("side\t Type alternative hypothesis (default=\"two\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("plot\t Logical value for plotting run test results (default=TRUE)\n")
  }
  if (4 %in% fn) {
    cat("[4] Pearson Correlation Coefficient & Spearman Correlation Coefficient\n")
    cat("corr.spear(x, y, r0=0, xl, yl, mt, step=1:2, alp=0.05, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Vector of x-data\n")
    cat("y\t Vector of y-data\n")
    cat("[Optional Input]--------------------------\n")
    cat("r0\t Correlation coefficient value under the null hypothesis\n")
    cat("xl\t Name of x-data\n")
    cat("yl\t Name of y-data\n")
    cat("mt\t Title of scatter plot\n")
    cat("step\t Steps of the analysis (default =1:2)\n")
    cat("\t 1\t Pearson correlation coefficient, correlation test, and the confidence interval\n")
    cat("\t 2\t Spearman correlation coefficient, correlation test, and the confidence interval\n")
    cat("\t 3\t Scatter plot\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (51 %in% fn) {
    cat("[5-1] Distribution of Wilcoxon Rank Sum Test Statistic\n")
    cat("ranksum.dist(n1, n2=3:10, tab=TRUE, plot=FALSE, dig=4)\n")
    cat("[Optional Input]--------------------------\n")
    cat("n1\t Number of data in group 1 (default=1:min(n2))\n")
    cat("n2\t Number of data in group 2 (default=3:10)\n")
    cat("tab\t Logical value for printing critical value table (default=TRUE)\n")
    cat("plot\t Logical value for plotting rank sum distribution (default=FALSE)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n\n")
  }
  if (52 %in% fn) {
    cat("[5-2] Wilcoxon Rank Sum Test\n")
    cat("ranksum.plot(x, y, side=\"two\", xlab=\"Rank Sum statistic\", dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector in group 1\n")
    cat("y\t Data vector in group 2\n")
    cat("[Optional Input]--------------------------\n")
    cat("side\t Type of alternative hypothesis (default=\"two\")\n")
    cat("xlab\t Label of x-axis (default=\"Rank Sum statistic\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (61 %in% fn) {
    cat("[6-1] Distribution of Wilcoxon Signed Rank Test Statistic\n")
    cat("signrank.dist(nv=5:50, av, tab=TRUE, plot=FALSE, dig=4)\n")
    cat("[Optional Input]--------------------------\n")
    cat("nv\t Number of signed data (default=5:50)\n")
    cat("av\t Probability vector (default=c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995))\n")
    cat("tab\t Logical value for printing quantile table (default=TRUE)\n")
    cat("plot\t Logical value for plotting test statistic distribution (default=FALSE)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n\n")
  }
  if (62 %in% fn) {
    cat("[6-2] Wilcoxon Signed Rank Test\n")
    cat("signrank.plot(x, y, mu0=0, side=\"two\", xlab=\"Signed Rank Sum\", dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector in group 1\n")
    cat("y\t Data vector in group 2\n")
    cat("[Optional Input]--------------------------\n")
    cat("mu0\t Mean difference under the null hypothesis (default=0)\n")
    cat("side\t Type of alternative hypothesis (default=\"two\")\n")
    cat("xlab\t Label of x-axis (default=\"Signed Rank Sum\")\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (7 %in% fn) {
    cat("[7] Kruskal Wallis Test\n")
    cat("kruswall.plot(x, y, dig=4, plot=FALSE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector\n")
    cat("y\t Vector of factor levels\n")
    cat("[Optional Input]--------------------------\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("plot\t Logical value for plotting test results (default=FALSE)\n")
  }
  if (8 %in% fn) {
    cat("[8] Friedman Test\n")
    cat("friedman.plot(x, a, b, dig=4, plot=FALSE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x\t Data vector\n")
    cat("a\t Vector of factor levels\n")
    cat("b\t Vector of block levels\n")
    cat("[Optional Input]--------------------------\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("plot\t Logical value for plotting test results (default=FALSE)\n")
  }
}
