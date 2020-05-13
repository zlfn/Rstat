#' @title Spread Estimation

#' @description To calculate spread estimates.


#' @usage spread.est(x, detail = FALSE)

#' @param x Vector of input data.
#' @param detail Print detailed output? Default: FALSE


#'
#'
#' @return None.
#' @examples
#' spread.est(mtcars$mpg, detail = T)
#' @export

spread.est <- function(x, detail = FALSE) {
  if (is.matrix(x)) {
    x <- as.vector(x)
  }
  n <- length(x)
  xvar <- var(x)
  xsd <- sd(x)
  xrng <- max(x) - min(x)
  xiqr <- IQR(x)
  xcv <- xsd / mean(x)
  if (detail == FALSE) {
    cat(
      "Variance=", xvar, "\t Stand. Dev.=",
      xsd, "\nRange=", xrng, "\t IQR=", xiqr,
      "\t CoV=", xcv, "\n"
    )
  }
  if (detail == TRUE) {
    cat(
      "Calculation in Detail -----------------------------------------------------",
      "\n(1) Variance =", paste0(
        sum(x^2), " - ",
        sum(x), "^2 / ", n
      ), "=", xvar, "\n(2) Stand. Dev. =",
      paste0("sqrt(", round(xvar, 7), ") ="),
      xsd, "\n(3) Range =", max(x), "-", min(x),
      "=", xrng, "\n(4) IQR =", quantile(
        x,
        0.75
      ), "-", quantile(x, 0.25), "=",
      xiqr, "\n(5) CoV =", xsd, "/", mean(x),
      "=", xcv, "\n"
    )
  }
}
