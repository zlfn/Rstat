#' @title Expected Values of Two Discrete Random Variables
#' @description Joint Probabilities and Expected Values of Two Discrete Random Variables
#' @usage disc.jexp(tabXY, Xn = "X", Yn = "Y", prt = "exp", pprt = FALSE, dig = 4)
#' @param tabXY Joint frequency (or probability) table
#' @param Xn Name of X, Default: 'X'
#' @param Yn Name of Y, Default: 'Y'
#' @param prt Option for detailed output in c("", "exp", "cov", "cor"), Default: 'exp'
#' @param pprt Plot the joint & marginal PDF? Default: FALSE
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))
#' @examples
#' S <- rolldie2(2)
#' X <- apply(S, 1, max)
#' Y <- apply(S, 1, min)
#' tXY <- table(X, Y)
#' disc.jexp(tXY, prt = "cor", pprt = TRUE)
#' @export


disc.jexp <- function(tabXY, Xn = "X", Yn = "Y", prt = "exp",
                      pprt = FALSE, dig = 4) {
  N <- sum(tabXY)
  mtabXY <- addmargins(tabXY)
  ptabXY <- tabXY / N
  mptabXY <- addmargins(ptabXY)
  Xv <- as.numeric(rownames(tabXY))
  Yv <- as.numeric(colnames(tabXY))
  nv <- dim(mtabXY)
  Xf <- as.vector(mtabXY[, nv[2]])[-nv[1]]
  Yf <- as.vector(mtabXY[nv[1], ])[-nv[2]]
  Sx <- (Xv %*% Xf)[1, 1]
  Ex <- Sx / N
  Sy <- (Yv %*% Yf)[1, 1]
  Ey <- Sy / N
  Sx2 <- (Xv^2 %*% Xf)[1, 1]
  Vx <- Sx2 / N - Ex^2
  Dx <- sqrt(Vx)
  Sy2 <- (Yv^2 %*% Yf)[1, 1]
  Vy <- Sy2 / N - Ey^2
  Dy <- sqrt(Vy)
  XY <- Xv %o% Yv
  Sxy <- (as.vector(XY) %*% as.vector(tabXY))[1, 1]
  Exy <- Sxy / N
  Vxy <- Exy - Ex * Ey
  Cxy <- Vxy / (Dx * Dy)
  if (pprt) {
    if (N > 1.1) {
      cat(paste0(
        "Joint & Marginal Frequency Distribution f(x,y)=n/",
        N
      ), "\n")
      print(mtabXY)
    }
    else {
      cat(
        paste0("Joint & Marginal Probability Distribution f(x,y)"),
        "\n"
      )
      print(ptabXY)
    }
  }
  if (prt %in% c("exp", "cov", "cor")) {
    cat(paste0(
      "E[X] = ", Sx, "/", N, " = ",
      round(Ex, dig)
    ), "\n")
    cat(paste0(
      "E[Y] = ", Sy, "/", N, " = ",
      round(Ey, dig)
    ), "\n")
    cat(paste0(
      "E[XY] = ", Sxy, "/", N, " = ",
      round(Exy, dig)
    ), "\n")
  }
  if (prt %in% c("cov", "cor")) {
    cat(
      paste0(
        "Var(X) = ", Sx2, "/", N, " - ",
        round(abs(Ex), dig), "² = ", round(Vx, dig)
      ),
      "\n"
    )
    cat(
      paste0(
        "Var(Y) = ", Sy2, "/", N, " - ",
        round(abs(Ey), dig), "² = ", round(Vy, dig)
      ),
      "\n"
    )
    cat(paste0(
      "Cov(X,Y) = ", round(Exy, dig), " - ",
      round(Ex, dig), " × ", round(Ey, dig), " = ",
      round(Vxy, dig)
    ), "\n")
  }
  if (prt == "cor") {
    cat(paste0(
      "Corr(X,Y) = ", round(Vxy, dig), " / √(",
      round(Vx, dig), "×", round(Vy, dig), ") = ",
      round(Cxy, dig)
    ), "\n")
  }
  out <- list(
    Ex = Ex, Dx = Dx, Ey = Ey, Dy = Dy, Exy = Exy,
    Vxy = Vxy, Cxy = Cxy
  )
  invisible(out)
}
