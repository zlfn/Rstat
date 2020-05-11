#' @title Ch3. Probability
#' @description List of Ch3. Functions
#' @usage ch3.man(fn = 0)
#' @param fn Function number to be refered. Default: 0
#' @return None.
#' @examples
#' ch3.man()
#' @export

ch3.man <- function(fn = 0) {
  if (0 %in% fn) {
    cat("[P#] prob package functions replaced ----------------- \n")
    cat("[P1] rolldie2\t\tCreating Sample Space of Rolling Dice\n")
    cat("[P2] tosscoin2\t\tCreating Sample Space of Tossing Coins\n")
    cat("[P3] cards2  \t\tCreating Sample Space of Sampling Cards\n")
    cat("[P4] urnsample2\t\tCreating Sample Space of Urn Sampling\n")
    cat("[P5] union2 \t\tUnion of Events in Data frame Format\n")
    cat("[P6] intersect2\t\tIntersection of Events in Data frame Format\n")
    cat("[P7] setdiff2 \t\tSet Difference of Events in Data frame Format\n\n")
    cat("[#] Function for Chapter 3. Probability ----------------- \n")
    cat("[1] element \t\tDisplaying Elements of an Event\n")
    cat("[2] pprt    \t\tCalculating Probability of an Event\n")
    cat("[3] cprt    \t\tCalculating the Conditional Probability\n")
    cat("[4] indep.event\t\tIndependence of Two Discrete Random Variables\n")
    cat("[5] bayes.plot\t\tDisplaying the Prior and the Posterior Probabilities\n")
  }
  if (1 %in% fn) {
    cat("[1] Displaying Elements of an Event\n")
    cat("element(A, r=10)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("A  \t Data frame with elements of the event for each row\n")
    cat("[Optional Input]--------------------------\n")
    cat("r  \t Maximum number of elements for each Line (default=10)\n")
  }
  if (2 %in% fn) {
    cat("[2] Calculating the Probability of an Event by Counting the Elements\n")
    cat("pprt(x, n, prt=TRUE)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("x  \t Data frame with elements of the event for each row\n")
    cat("n  \t Number of elements in the sample space \n")
    cat("[Optional Input]--------------------------\n")
    cat("prt \t Logical value for printing detailed output (default=TRUE)\n")
  }
  if (3 %in% fn) {
    cat("[3] Calculating the Conditional Probability\n")
    cat("cprt(a, b)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("a  \t Data frame of conditioned event \n")
    cat("b  \t Data frame of conditioning event \n")
  }
  if (4 %in% fn) {
    cat("[4] Determining the Independence of Two Discrete Random Variables\n")
    cat("indep.event(X, Y, N, ep=1E-6)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("X  \t First random variable\n")
    cat("Y  \t Second random variable\n")
    cat("N  \t Number of elements in the sample space\n")
    cat("[Optional Input]--------------------------\n")
    cat("ep \t Precision limit for probability calculation (default=1E-6)\n")
  }
  if (5 %in% fn) {
    cat("[5] Displaying the Prior and the Posterior Probabilities\n")
    cat("bayes.plot(prior, post, group, cond, dcol, cex=1, dig=4)\n")
    cat("[Mandatory Input]--------------------------\n")
    cat("prior\t Prior probability distribution vector\n")
    cat("post\t Posterior probability distribution vector\n")
    cat("[Optional Input]--------------------------\n")
    cat("group\t Class name (default=A, B, C, ...)\n")
    cat("cond\t Conditional event name (default=F)\n")
    cat("dcol\t Bar chart colors (default=transparent rainbow colors)\n")
    cat("cex \t Text size of the probability (default=1)\n")
    cat("dig \t Number of digits below the decimal point (default=4)\n")
  }
}
