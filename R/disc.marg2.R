#' @title Marginal PDF of Two Discrete Random variables
#' @description Marginal PDF of Two Discrete Random variables
#' @usage disc.marg2(tabXY, Xn, Yn, prt = TRUE, plot = FALSE, dig = 5, dig2 = 4)
#' @param tabXY Joint frequency table of two random variables
#' @param Xn Name of the first random variable (default="X")
#' @param Yn Name of the second random variable (default="Y")
#' @param prt Print the marginal frequency and probability? Default: TRUE
#' @param plot Plot the marginal PDF? Default: FALSE
#' @param dig Number of digits below the decimal point in the console, Default: 5
#' @param dig2 Number of digits below the decimal point in the graph, Default: 4
#'
#' @return Marginal Probabilities
#' @examples
#' fxy <- with(mtcars, table(cyl, carb))
#' disc.marg2(fxy)
#' disc.marg2(fxy, "Cylinder", "Carbrator", prt = FALSE, plot = TRUE)
#' @export

disc.marg2 <- function(tabXY, Xn, Yn, prt = TRUE, plot = FALSE, dig = 5, dig2 = 4) {
  if (missing(Xn)) {
    Xn <- "X"
  }
  if (missing(Yn)) {
    Yn <- "Y"
  }
  N <- sum(tabXY)
  tabX <- apply(tabXY, 1, sum)
  tabY <- apply(tabXY, 2, sum)
  ptabX <- tabX / N
  ptabY <- tabY / N
  if (prt == TRUE) {
    cat("Marginal Frequency Distribution of", Xn, "\n")
    print(tabX)
    cat(
      "Marginal probability distribution of", Xn,
      "\n"
    )
    print(round(ptabX, dig))
    cat("Marginal Frequency Distribution of", Yn, "\n")
    print(tabY)
    cat(
      "Marginal probability distribution of", Yn,
      "\n"
    )
    print(round(ptabY, dig))
  }
  if (plot == TRUE) {
    xa <- as.numeric(names(tabX))
    nx <- length(xa)
    ya <- as.numeric(names(tabY))
    ny <- length(ya)
    dev.new(7, 6)
    par(mfrow = c(2, 1))
    par(mar = c(3, 4, 4, 2))
    plot(xa, ptabX,
      type = "h", main = paste(
        "Marginal Probability Distribution of",
        Xn
      ), ylim = c(0, max(ptabX) * 1.1), xlab = "",
      ylab = "f(x)", pch = 16, lwd = 5, col = 2
    )
    text(xa, ptabX, paste0(tabX, "/", N),
      pos = 3,
      col = 4, cex = 0.8
    )
    plot(ya, ptabY,
      type = "h", main = paste(
        "Marginal Probability Distribution of",
        Yn
      ), ylim = c(0, max(ptabY) * 1.1), xlab = "",
      ylab = "f(y)", pch = 16, lwd = 5, col = 2
    )
    text(ya, ptabY, paste0(tabY, "/", N),
      pos = 3,
      col = 4, cex = 0.8
    )
  }
  invisible(list(fx = ptabX, fy = ptabY))
}
