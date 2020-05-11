#' @title Manual for Ch12. Functions
#' @description Ch12. Analysis of Categorical Data
#' @usage ch12.man(fn = 0)
#' @param fn Function number, Default: 0
#' @return None.
#' @examples
#' ch12.man()
#' ch12.man(1)
#' @export
ch12.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] chitest.plot2  \tPlot the Result of Chi-square Test\n")
    cat("[2] mosaic2\t\tMosaic Plot\n")
  }
  if (1 %in% fn) {
    cat("[1] Plot the Result of Chi-square Test\n")
    cat("chitest.plot2(stat, df, alp=0.05, side=\"two\", pup=0.999, dig=4, ppt=20)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("stat\t Chi-square test statistic\n")
    cat("df\t Degree of freedom\n")
    cat("[Optional Input]--------------------------\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("side\t Type of the alternative hypothesis (default=\"two\")\n")
    cat("pup\t Maximum probability for the range of x-axis (default=0.999)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("ppt\t Number of plot points in the critical region (default=20)\n")
  }
  if (2 %in% fn) {
    cat("[2] Mosaic Plot\n")
    cat("require(vcd)\n")
    cat("mosaic2(tab, mt, dig=4, resid=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("tab\t Data table \n")
    cat("mt\t Graph title\n")
    cat("[Optional Input]--------------------------\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("resid\t Logical value for displaying Pearson residuals (default=TRUE)\n")
  }
}
