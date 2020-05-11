#' @title Manual for Ch.7 Functions
#' @description Ch.7 Continuous Random Variables
#' @usage ch7.man(fn = 0)
#' @param fn Function number (0~2), Default: 0
#' @return None.
#' @examples
#' ch7.man()
#' ch7.man(1:2)
#' @export
ch7.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] cont.exp\t\tExpected Values of Continuous Random Variables\n")
    cat("[2] cont.mpdf\t\tPDF and CDF plots for Continuous Random Variables\n")
  }
  if (1 %in% fn) {
    cat("[1] Expected Values of Continuous Random Variables\n")
    cat("cont.exp(FUN, lo, up, mt, dig=3, xn=\"x\", prt=FALSE, plot=FALSE, pos=\"center\")\n")
    cat("[Mandatory Input]------------------------------\n")
    cat("FUN\t Continuous probability density function\n")
    cat("lo\t Lower limit of x-axis\n")
    cat("up\t Upper limit of x-axis\n")
    cat("[Optional Input]------------------------------\n")
    cat("mt\t Graph title\n")
    cat("dig\t Number of digits below the decimal point (default=3)\n")
    cat("xn\t Random variable name (default=\"x\")\n")
    cat("prt\t Logical value for printing expected values and variances (default=FALSE)\n")
    cat("plot\t Logical value for plotting probability density function (default=FALSE)\n")
    cat("pos\t Legend location (default=\"center\")\n")
  }
  if (2 %in% fn) {
    cat("[2] Probability Density Function and CDF for Continuous Random Variables\n")
    cat("cont.mpdf(dist, lo, up, para, para2, ymax, mt, dcol, np=100, \n")
    cat("\t pos1=\"topright\", pos2=\"bottomright\", xp1, xp2)\n")
    cat("[Mandatory Input]------------------------------\n")
    cat("dist\t Name of continuous probability distribution (one of the follows)\n")
    cat("\t (\"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", \"t\", \"chisq\", \"f\")\n")
    cat("lo\t Lower limit of x-axis\n")
    cat("up\t Upper limit of x-axis\n")
    cat("para\t First parameter vector of probability density function\n")
    cat("para2\t Second parameter vector of probability density function (if necessary)\n")
    cat("[Optional Input]------------------------------\n")
    cat("ymax\t Upper limit of y-axis\n")
    cat("mt\t Graph title\n")
    cat("dcol\t Graph color vector (default as follows)\n")
    cat("\t c(\"red\", \"blue\", \"orange2\", \"green4\", \"purple\", \"cyan2\")\n")
    cat("np\t Number of plot points (default=100)\n")
    cat("pos1\t Legend location of probability density function (default=\"topright\")\n")
    cat("pos2\t Legend location of cumulative distribution function (default=\"bottomright\")\n")
    cat("xp1\t Vector of specific x values for probability density function (ignore legend)\n")
    cat("xp2\t Vector of specific x values for cumulative distribution function (ignore legend)\n")
  }
}
