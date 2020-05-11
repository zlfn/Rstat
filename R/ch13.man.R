#' @title Manual for Ch13. Functions
#' @description Ch13. Analysis of Variance
#' @usage ch13.man(fn = 0)
#' @param fn Function number, Default: 0
#' @return None.
#' @examples
#' ch13.man()
#' ch13.man(1)
#' @export

ch13.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] anova1\t\tOne-way Analysis of Variance\n")
    cat("[2] anova2\t\tTwo-way Analysis of Variance\n")
  }
  if (1 %in% fn) {
    cat("[1] One-way Analysis of Variance\n")
    cat("anova1(y, f, xl=\"Factor\", yl=\"Response Variable\", step=0:7, alp=0.05, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("y\t Response variable data\n")
    cat("f\t Factors (same length as y)\n")
    cat("[Optional Input]--------------------------\n")
    cat("xl\t Label of x-axis (default=\"Factor\")\n")
    cat("yl\t Label of y-axis (default=\"Response Variable\")\n")
    cat("step\t Steps of the analysis of variance (default=0:7)\n")
    cat("\t 0\t Calculate the mean of response variable for each factor level\n")
    cat("\t 1\t Box plot of response data for prior investigation\n")
    cat("\t 2\t One-way analysis of variance\n")
    cat("\t 3\t Diagnostic plot of one-way analysis of variance\n")
    cat("\t 4\t Confidence interval of the response mean for each factor level\n")
    cat("\t 5\t Plot confidence interval of the response mean for each factor level\n")
    cat("\t 6\t Confidence intervals of the response mean differences\n")
    cat("\t 7\t Plot confidence intervals of the response mean differences\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
  if (2 %in% fn) {
    cat("[2] Two-way Analysis of Variance\n")
    cat("anova2(y, f1, f2, xl1, xl2, yl, step=0:7, alp=0.05,\n")
    cat("\t inter=TRUE, maxim=TRUE, nb=4, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("y\t Response variable data\n")
    cat("f1\t Levels of factor1 (same length as y)\n")
    cat("f2\t Levels of factor2 (same length as y)\n")
    cat("[Optional Input]--------------------------\n")
    cat("xl1\t Label of x-axis (default=name of f1)\n")
    cat("xl2\t Legend label (default=name of f2)\n")
    cat("yl\t Label of y-axis (default=name of y)\n")
    cat("step\t Steps of the analysis of variance (default=0:7)\n")
    cat("\t 0\t Calculate the mean of response variable for each factor level\n")
    cat("\t 1\t Interaction plot for prior investigation of data\n")
    cat("\t 2\t Two-way analysis of variance\n")
    cat("\t 3\t Diagnostic plot of two-way analysis of variance\n")
    cat("\t 4\t Confidence interval of the response mean for each factor level\n")
    cat("\t 5\t Plot confidence interval of the response mean for each factor level\n")
    cat("\t 6\t Confidence intervals of the response mean differences of best nb levels\n")
    cat("\t 7\t Plot confidence intervals of the response mean differences of best nb levels\n")
    cat("alp\t Level of significance (default=0.05)\n")
    cat("inter\t Logical value for including interaction (default=TRUE)\n")
    cat("\t\t (Set inter=FALSE for the case with no replication\n")
    cat("maxim\t Logical value for maximization problem (default=TRUE)\n")
    cat("\t\t (Set maxim=FALSE for minimization problem\n")
    cat("nb\t Number of best level to be compared (default=4)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
  }
}
