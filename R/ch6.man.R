#' @title Manual for Ch.6
#' @description Ch.6 Probability Distributions of Discrete Random Variables
#' @usage ch6.man(fn = 0)
#' @param fn Function Number. Default: 0
#' @return None.
#' @examples
#' ch6.man()
#' ch6.man(1:2)
#' @export

ch6.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] disc.mexp\t\tExpected Values of Discrete Random Variables\n")
    cat("[2] multinorm.plot \tGraphic Display of Multinomial Probability Distribution Function\n")
  }
  if (1 %in% fn) {
    cat("[1] Expected Values of Discrete Random Variables\n")
    cat("disc.mexp(xv, fx, fx2, fx3, mt, dig=3, del=0.2, prt=TRUE, plot=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("xv\t Vector of random variable values\n")
    cat("fx\t List of probability distributions (2~9)\n")
    cat("[Optional Input]--------------------------\n")
    cat("fx2\t Second list of probability distributions (same number of fx)\n")
    cat("fx3\t Third list of probability distributions (same number of fx)\n")
    cat("mt\t Vector of plot titles\n")
    cat("dig\t Number of digits below decimal point (default=3)\n")
    cat("del\t Distance between the probability distribution plots (default=0.2)\n")
    cat("prt\t Logical value for printing the expected values and variances (default=TRUE)\n")
    cat("plot\t Logical value for plotting the probability distributions (default=TRUE)\n")
  }
  if (2 %in% fn) {
    cat("[2] Graphic Display of Multinomial Probability Distribution Function\n")
    cat("multinorm.plot(ps, size)\n")
    cat("require(prob)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("ps\t Probability matrix with column for each group\n")
    cat("\t (relative group sizes may substitute for the probabilities)\n")
    cat("size\t Sample size\n")
  }
}
