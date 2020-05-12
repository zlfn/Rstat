#' @title Joint CDF of Two Continuous Random Variables
#' @description Joint Cumulative Distribution Function of Two Continuous Random Variables
#' @usage cont.jcdf(FUN, xs, ys, lo1 = -Inf, lo2 = -Inf)
#' @param FUN Continuous joint PDF function
#' @param xs Specific values of X for displaying the cumulative probability
#' @param ys Specific values of Y for displaying the cumulative probability
#' @param lo1 Lower limit of X, Default: -Inf
#' @param lo2 Lower limit of Y, Default: -Inf
#' @return Cumulative probability
#' @examples
#' pdf <- function(x, y) (x + y) * (x > 0 & x < 1) * (y > 0 & y < 1)
#' cont.jcdf(pdf, Inf, Inf)
#' cont.jcdf(pdf, 0.5, 0.5)
#' @export
cont.jcdf <- function(FUN, xs, ys, lo1 = -Inf, lo2 = -Inf) {
  Fxy <- function(x, y) Pr(FUN, lo1, x, lo2, y)[[1]]
  VFxy <- Vectorize(Fxy, c("x", "y"))
  prob <- VFxy(xs, ys)
  names(prob) <- paste0("F(", xs, ",", ys, ")")
  return(prob)
}
