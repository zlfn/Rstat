#' @title Manual for Ch5
#' @description Ch5. Expected Values of Random Variables
#' @usage ch5.man(fn = 0)
#' @param fn Function Number. Default: 0
#' @return None.
#' @examples
#' ch5.man()
#' ch5.man(2)
#' @export
ch5.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[1] disc.exp\tExpected Value of a Discrete Random Variable\n")
    cat("[2] disc.jexp\tJoint Probabilities and Expected Values of Two Discrete Random Variables\n")
    cat("[3] cont.jexp\tJoint pdf and Expected Value of Two Continuous Random Variables\n")
    cat("[4] corr.plot\tCorrelation Coefficients and Scatter Plots of Discrete Random variables\n")
  }
  if (1 %in% fn) {
    cat("[1] Expected Value of a Discrete Random Variable\n")
    cat("disc.exp(xv, xf, mt, dig=3, prt=TRUE, plot=FALSE, pos=\"topright\")\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("xv\t Vector of random variable values\n")
    cat("xf\t Vector of PDF(or frequency distribution)\n")
    cat("[Optional Input]--------------------------\n")
    cat("mt\t graph title\n")
    cat("dig\t Number of digits below the decimal point (default=3)\n")
    cat("prt\t Logical value for printing detailed output (default=TRUE)\n")
    cat("plot\t Logical value for plotting the PDF (default=FALSE)\n")
    cat("pos\t Legend location (default=\"topright\")\n")
    cat("[Returned Object]--------------------------\n")
    cat("list(Ex=expected value, Dx=standard deviation, Vx=variance)\n")
  }
  if (2 %in% fn) {
    cat("[2] Joint Probabilities and Expected Values of Two Discrete Random Variables\n")
    cat("disc.jexp(tabXY, Xn=\"X\", Yn=\"Y\", prt=\"exp\", pprt=FALSE, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("tabXY\t Joint frequency (or probability) table\n")
    cat("[Optional Input]--------------------------\n")
    cat("Xn\t Name of X (default=\"X\")\n")
    cat("Yn\t Name of Y (default=\"Y\")\n")
    cat("prt\t Option for detailed output in c(\"\", \"exp\", \"cov\", \"cor\") (default=\"exp\")\n")
    cat("pprt\t Logical value for plotting the joint & marginal PDF (default=FALSE)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("[Returned Object]--------------------------\n")
    cat("list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))\n")
  }
  if (3 %in% fn) {
    cat("[3] Joint pdf and Expected Value of Two Continuous Random Variables\n")
    cat("cont.jexp(FUN, lo1=-Inf, up1=Inf, lo2=-Inf, up2=Inf, dig=4, prt=\"exp\")\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("FUN\t Continuous joint probability density function\n")
    cat("[Optional Input]--------------------------\n")
    cat("lo1\t Lower limit of X (default=-Inf)\n")
    cat("up1\t Upper limit of X (default=Inf)\n")
    cat("lo2\t Lower limit of Y (default=-Inf)\n")
    cat("up2\t Upper limit of Y (default=Inf)\n")
    cat("dig\t Number of digits below the decimal point (default=4)\n")
    cat("prt\t Option for detailed output in c(\"\", \"exp\", \"cov\", \"cor\") (default=\"exp\")\n")
    cat("[Returned Object]--------------------------\n")
    cat("list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))\n")
  }
  if (4 %in% fn) {
    cat("[4] Correlation Coefficients and Scatter Plots of Discrete Random variables\n")
    cat("corr.plot(X, Mt, item, dig=4, prt=TRUE, pprt=FALSE, plot=FALSE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("X\t Sample space vector of X\n")
    cat("item\t Names of random variables\n")
    cat("[Optional Input]--------------------------\n")
    cat("Mt\t Plot title\n")
    cat("dig\t Number of effective digits (default=5)\n")
    cat("prt\t Logical value for printing the result (default=TRUE)\n")
    cat("pprt\t Logical value for printing frequency tables (default=FALSE)\n")
    cat("plot\t Logical value for plotting the PDF and scatter plots (default=FALSE)\n")
  }
}
