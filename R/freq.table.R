#' @title Frequency Table
#' @description To create a frequency table for a vector (or matrix) x
#' @usage freq.table(x, cuts, dig = 4, mp = FALSE, ...)
#' @param x Data vector (or matrix).
#' @param cuts Breaks (or number) of intervals.
#' @param dig Number of digits. Defaults to 4.
#' @param mp Logical variable for making histogram. Defaults to FALSE.
#' @param ... Other graphic parameters
#'
#' @return xtab Frequency Table
#' @examples
#' freq.table(rnorm(100))
#' @export

freq.table <- function(x, cuts, dig = 4, mp = FALSE, ...) {
  x <- as.vector(x)
  n <- length(x)
  if (missing(cuts)) {
    cuts <- hist(x, plot = F)$breaks
  }
  xh <- hist(x, breaks = cuts, plot = mp, ...)
  xcf <- cumsum(xh$counts)
  xrf <- xh$counts / n
  xrcf <- xcf / n
  ng <- length(cuts)
  xclass <- paste0(
    "(", xh$breaks[-ng], ", ", xh$breaks[-1],
    "]"
  )
  xtab <- cbind(xh$mids, xh$counts, xcf, round(xrf, dig), round(
    xrcf,
    dig
  ))
  rownames(xtab) <- xclass
  colnames(xtab) <- c(
    "Center", "Freq", "Cum-Fr",
    "Rel-Fr", "Rel-CFr"
  )
  return(xtab)
}
