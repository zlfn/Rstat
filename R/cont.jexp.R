#' @title Expected Value of Two Continuous Random Variables
#' @description Joint pdf and Expected Value of Two Continuous Random Variables
#' @usage cont.jexp(FUN, lo1 = -Inf, up1 = Inf, lo2 = -Inf, up2 = Inf, dig = 4, prt = "exp")
#' @param FUN Continuous joint probability density function
#' @param lo1 Lower limit of X, Default: -Inf
#' @param up1 Upper limit of X, Default: Inf
#' @param lo2 Lower limit of Y, Default: -Inf
#' @param up2 Upper limit of Y, Default: Inf
#' @param dig Number of digits below the decimal point, Default: 4
#' @param prt Option for detailed output in c("", "exp", "cov", "cor"), Default: ”
#'
#'
#' @return list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))
#' @examples
#' pdf <- function(x, y) 0.5 * (x + 3 * y) * (x > 0 & x < 1) * (y > 0 & y < 1)
#' cont.jexp(pdf, prt = "cor")
#' @export
cont.jexp <- function(FUN, lo1 = -Inf, up1 = Inf, lo2 = -Inf, up2 = Inf,
                      dig = 4, prt = "exp") {
  ex1 <- function(x, y) x * FUN(x, y)
  ex2 <- function(x, y) x^2 * FUN(x, y)
  Ex <- Exp(ex1, lo1, up1, lo2, up2)[[1]]
  Ex2 <- Exp(ex2, lo1, up1, lo2, up2)[[1]]
  Vx <- Ex2 - Ex^2
  Dx <- sqrt(Vx)
  ey1 <- function(x, y) y * FUN(x, y)
  ey2 <- function(x, y) y^2 * FUN(x, y)
  Ey <- Exp(ey1, lo1, Inf, lo2, Inf)[[1]]
  Ey2 <- Exp(ey2, lo1, Inf, lo2, Inf)[[1]]
  Vy <- Ey2 - Ey^2
  Dy <- sqrt(Vy)
  exy <- function(x, y) x * y * FUN(x, y)
  Exy <- Exp(exy, lo1, Inf, lo2, Inf)[[1]]
  Vxy <- Exy - Ex * Ey
  Cxy <- Vxy / sqrt(Vx * Vy)
  if (prt %in% c("exp", "cov", "cor")) {
    cat(paste0("E(X) = ", round(Ex, dig)), "\n")
    cat(paste0("E(Y) = ", round(Ey, dig)), "\n")
    cat(paste0("E(XY) = ", round(Exy, dig)), "\n")
  }
  if (prt %in% c("cov", "cor")) {
    cat(
      paste0(
        "Var(X) = ", round(Ex2, dig), " - ",
        round(abs(Ex), dig), "² = ", round(Vx, dig)
      ),
      "\n"
    )
    cat(
      paste0(
        "Var(Y) = ", round(Ey2, dig), " - ",
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
