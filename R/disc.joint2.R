#' @title Joint PDF of Two Discrete Random variable
#' @description Joint Probability Distribution of Two Discrete Random variable
#' @usage disc.joint2(X, Y, prt = TRUE, plot = FALSE, dig = 4, dig2 = 3, ep = 0)
#' @param X Sample space vector of the first random variable
#' @param Y Sample space vector of the second random variable
#' @param prt Print the joint frequency and probability? Default: TRUE
#' @param plot Plot the joint PDF? Default: FALSE
#' @param dig Number of digits below the decimal point in the console, Default: 4
#' @param dig2 Number of digits below the decimal point in the graph, Default: 3
#' @param ep Minimum value for displaying the joint probability, Default: 0
#'
#' @return Joint PDF
#' @examples
#' S <- rolldie2(4)
#' X <- apply(S, 1, max)
#' Y <- apply(S, 1, min)
#' disc.joint2(X, Y)
#' library(scatterplot3d)
#' disc.joint2(X, Y, prt = FALSE, plot = TRUE)
#' @export

disc.joint2 <- function(X, Y, prt = TRUE, plot = FALSE, dig = 4, dig2 = 3,
                        ep = 0) {
  Xn <- deparse(substitute(X))
  Yn <- deparse(substitute(Y))
  N <- length(X)
  tabXY <- table(X, Y)
  mtabXY <- addmargins(tabXY)
  ptabXY <- tabXY / N
  mptabXY <- addmargins(ptabXY)
  if (prt == TRUE) {
    cat(
      paste0(Xn, " & ", Yn, " joint/marginal frequency distribution"),
      "\n"
    )
    print(mtabXY)
    cat(
      paste0(Xn, " & ", Yn, " joint/marginal probability distribution"),
      "\n"
    )
    print(round(mptabXY, dig))
  }
  if (plot == TRUE) {
    xa <- as.numeric(names(table(X)))
    nx <- length(xa)
    ya <- as.numeric(names(table(Y)))
    ny <- length(ya)
    xa <- rep(xa, ny)
    ya <- rep(ya, each = nx)
    fxy <- as.vector(as.matrix(ptabXY))
    dc <- rank(fxy)
    dp <- sort(unique(dc))
    nc <- length(dp)
    for (k in 1:nc) dc[dc == dp[k]] <- nc + 1 - k
    dcol <- heat.colors(nc)
    dev.new(7, 5)
    s3d <- scatterplot3d(xa, ya, fxy,
      type = "h", main = paste0(
        "Joint Probability Distribution of ",
        Xn, " & ", Yn
      ), xlab = Xn, ylab = Yn, zlab = "f(x, y)",
      pch = 16, lwd = 5, color = dcol[dc]
    )
    s3d.coords <- s3d$xyz.convert(xa, ya, fxy)
    text(s3d.coords$x[fxy > ep], s3d.coords$y[fxy > ep],
      labels = round(fxy[fxy > ep], dig2), pos = 3, offset = 0.3,
      col = 4, cex = 0.8
    )
  }
  invisible(list(freq = tabXY, prob = ptabXY))
}
