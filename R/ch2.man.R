#' @title Chapter 2 Functions Manual
#' @description This function allows you to review the 7 functions in ch2.
#' @usage ch2.man(fn = 0)
#' @param fn Number 0 to 7 for function. Defaults to 0.
#' @return None.
#' @examples
#' ch2.man()
#' ch2.man(3:5)
#' @export

ch2.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] freq.table\t\t Making a Frequency Table\n")
    cat("[2] unstable.hist \t Making Histograms of Unstable Processes\n")
    cat("[3] strat.hist\t\t Making Stratified Histograms\n")
    cat("[4] corr.plot6\t\t Making Scatter Plots of Six Cases\n")
    cat("[5] scat.lm\t\t Making a Scatter Plot with a Regression Line\n")
    cat("[6] location.est \t Calculating Measures of Central Location\n")
    cat("[7] spread.est\t\t Calculating Measures of Dispersion\n")
  }
  if (1 %in% fn) {
    cat("[1] Making a Frequency Table\n")
    cat("freq.table(x, cuts, dig=4, mp=FALSE, ...)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x  \t Data vector\n")
    cat("[Optional Input]--------------------------\n")
    cat("cuts\t Vector of class limits (default=built-in values)\n")
    cat("dig \t Number of digits below decimal point (default=4)\n")
    cat("mp \t Plot histogram? (default=FALSE)\n")
    cat("...   \t Graphic parameter (default=built-in values)\n")
  }
  if (2 %in% fn) {
    cat("[2] Making Histograms of Unstable Processes\n")
    cat("unstable.hist(N=200, m1=10, s1=1, m2=6, s2=0.5, a=8, b=9, c=9, vc=rep(\"cyan\", 4))\n")
    cat("[Optional Input]--------------------------\n")
    cat("N  \t Number of data for each histogram (default=200)\n")
    cat("m1 \t Mean the main distribution (default=10)\n")
    cat("s1 \t Standard deviation of the main distribution (default=1)\n")
    cat("m2 \t Mean of the contaminated distribution (default=6)\n")
    cat("s2 \t Standard deviation of the contaminated distribution (default=0.5)\n")
    cat("a  \t Lower limit of the missing values (default=8)\n")
    cat("b  \t Upper limit of the missing values (default=9)\n")
    cat("c  \t Lower limit of the cliff type range (default=9)\n")
    cat("vc \t Color vector of histograms (default=\"cyan\")\n")
  }
  if (3 %in% fn) {
    cat("[3] Making Stratified Histograms\n")
    cat("strat.hist(ng=3, n=200, m=c(6,10,14), s=1, sp=c(4, 16), vc, prob=FALSE)\n")
    cat("[Optional Input]--------------------------\n")
    cat("ng \t Number of groups (default=3)\n")
    cat("n  \t Number of data for each histogram (default=200)\n")
    cat("m  \t Vector of mean for each group (default=c(6,10,14))\n")
    cat("s  \t Vector of standard deviation or each group (default=1)\n")
    cat("sp \t Specification limits (default=c(4, 16))\n")
    cat("vc \t Color vector(1+ng) of histograms (default=\"orange\"(total), \"cyan\"(sub))\n")
    cat("prob\t Logical value for selecting density instead of frequency (default=FALSE)\n")
  }
  if (4 %in% fn) {
    cat("[4] Making Scatter Plots of Six Cases\n")
    cat("corr.plot6(m1 = 60, s1=10, m2=60, s2=10, r=0.7, r2=0.8, n=50)\n")
    cat("[Optional Input]--------------------------\n")
    cat("m1 \t Mean of x (default= 60)\n")
    cat("s1  \t Standard deviation of x (default=10)\n")
    cat("m2 \t Mean of y (default=60)\n")
    cat("s2 \t Standard deviation of y (default=10)\n")
    cat("r   \t Correlation coefficient of x and y (default=0.7)\n")
    cat("r2  \t Correlation coefficient of the stratified sample (default=0.8)\n")
    cat("n   \t Number of data pairs (default=50)\n")
  }
  if (5 %in% fn) {
    cat("[5] Making a Scatter Plot with a Regression Line\n")
    cat("scat.lm(x, y, mt, xl, yl, w=c(7, 5), ...)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x  \t Data vector for x-axis\n")
    cat("y  \t Data vector for y-axis\n")
    cat("[Optional Input]--------------------------\n")
    cat("mt \t Title of the scatter plot\n")
    cat("xl \t Label of x-axis\n")
    cat("yl \t Label of y-axis\n")
    cat("w  \t Size of the graphic window (default=c(7, 5))\n")
    cat("...  \t Graphic parameters \n")
  }
  if (6 %in% fn) {
    cat("[6] Calculating Measures of Central Location\n")
    cat("location.est(x, tr=0.1, detail=FALSE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x  \t Data vector or matrix\n")
    cat("[Optional Input]--------------------------\n")
    cat("tr \t Trim proportion of the trimmed mean (default=0.1)\n")
    cat("detail\t Logical value for printing detailed output (default=FALSE)\n")
  }
  if (7 %in% fn) {
    cat("[7] Calculating Measures of Dispersion\n")
    cat("spread.est(x, detail=FALSE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x  \t Data vector or matrix\n")
    cat("[Optional Input]--------------------------\n")
    cat("detail\t Logical value for printing detailed output (default=FALSE)\n")
  }
}
