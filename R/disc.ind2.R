#' @title Independence of Two Discrete Random Variables
#' @description Determine Independence of Two Discrete Random Variables
#' @usage disc.ind2(X, Y, prt = TRUE)
#' @param X Sample space vector of X
#' @param Y Sample space vector of Y
#' @param prt Print detailed output? Default: TRUE
#'
#'
#' @return Joint PDF
#' @examples
#' S <- rolldie2(4)
#' sum3 <- function(x) sum(x >= 3)
#' X <- apply(S, 1, sum3)
#' even <- function(x) sum(x %% 2 == 0)
#' Y <- apply(S, 1, even)
#' disc.ind2(X, Y)
#' @export
disc.ind2 <- function(X, Y, prt = TRUE) {
  Xn <- deparse(substitute(X))
  Yn <- deparse(substitute(Y))
  N <- length(X)
  tabXY <- table(X, Y)
  mtabXY <- addmargins(tabXY)
  frX <- table(X)
  frY <- table(Y)
  frXY <- (frX %o% frY) / N
  mfrXY <- addmargins(as.table(frXY))
  diffXY <- mtabXY - mfrXY
  value <- c(as.numeric(names(frX)), NA)
  dfXY <- cbind(mtabXY, value, diffXY)
  if (prt == TRUE) {
    cat(paste0(
      "Joint PDF: f(x,y)×", N, " ⇒ [f(x,y)-f(x)f(y)]×",
      N
    ), "\n")
    print(dfXY)
  }
  err <- abs(max(tabXY - frXY))
  if (err == 0) {
    cat("f(x,y) = f(x)f(y) ⇒ Independent\n")
  }
  else {
    cat(
      "max|f(x,y)-f(x)f(y)| =", err, "/", N,
      "⇒ Not Independent\n"
    )
  }
  invisible(list(freq = mtabXY, prod = mfrXY))
}
