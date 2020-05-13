#' @title Independence of Random Variables
#' @description Determining Independence of Two Discrete Random Variables
#' @usage indep.event(X, Y, N, ep = 1e-06)
#' @param X First random variable.
#' @param Y Second random variable.
#' @param N Size of the sample space.
#' @param ep Precision limit. Default: 1e-06

#'
#' @return Probablities to be compared.


#' @examples
#' indep.event(A, B, nrow(S))
#' @export

indep.event <- function(X, Y, N, ep = 1e-06) {
  Xn <- deparse(substitute(X))
  Yn <- deparse(substitute(Y))
  px <- pprt2(X, Xn, N)
  py <- pprt2(Y, Yn, N)
  cat(
    paste0("P(", Xn, ") × P(", Yn, ") ="),
    px * py, "\n"
  )
  XY <- intersect2(X, Y)
  XYn <- paste0(Xn, Yn)
  pxy <- pprt2(XY, XYn, N)
  err <- abs(pxy - px * py)
  if (err < ep) {
    cat(paste0(
      "P(", XYn, ") = P(", Xn, ") × P(",
      Yn, ") ⇒ Independent"
    ), "\n")
    cprt2(X, Xn, Y, Yn)
    cat(" = ")
    pprt2(X, Xn, N)
    cprt2(Y, Yn, X, Xn)
    cat(" = ")
    pprt2(Y, Yn, N)
  }
  else {
    cat(
      paste0(
        "|P(", XYn, ")-P(", Xn, ")xP(",
        Yn, ")|=", round(err, 4), " --> Not independent"
      ),
      "\n"
    )
    cprt2(X, Xn, Y, Yn)
    cat("<>")
    pprt2(X, Xn, N)
    cprt2(Y, Yn, X, Xn)
    cat("<>")
    pprt2(Y, Yn, N)
  }
  invisible(list(pxpy = px * py, pxy = pxy))
}
