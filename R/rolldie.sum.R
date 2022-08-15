#' @title PDF of the Sum of n Dice

#' @description Probability Distribution of the Sum of n Dice


#' @usage rolldie.sum(n, cex = 1)

#' @param n Number of dice.
#' @param cex Default: 1
#'
#'
#'
#' @return None.
#' @examples
#' rolldie.sum(4)
#' @export
rolldie.sum <- function(n, cex = 1) {
  S <- rolldie2(n)
  N <- nrow(S)
  X <- apply(S, 1, sum)
  X.freq <- table(X)
  print(addmargins(X.freq))
  X.prob <- X.freq / length(X)
  print(round(addmargins(X.prob), 4))
  X.val <- as.numeric(names(X.freq))
  EX <- sum(X.val * X.prob)
  EX2 <- sum(X.val^2 * X.prob)
  VX <- EX2 - EX^2
  DX <- sqrt(VX)
  Xmin <- min(X.val)
  Xmax <- max(X.val)
  dev.new(7, 5)
  plot(X.prob, type = "h", col = "red", main = paste0(
    "Probability Distribution of the Sum of ",
    n, " Dice"
  ), lwd = 4, ylim = c(0, max(X.prob) +
    0.01))
  fitnorm <- function(x) dnorm(x, EX, DX)
  curve(fitnorm, Xmin, Xmax, add = T, col = 4)
  text(Xmin:Xmax, X.prob,
    labels = X.freq, pos = 3, col = 4,
    cex = cex
  )
  legend("topright", c(
    paste("S-S.Size =", N),
    paste("E(X) =", EX), paste("D(X) =", round(
      DX,
      4
    ))
  ), bg = "white")
}
