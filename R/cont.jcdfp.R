#' @title Joint CDF Plot of Two Continuous Random Variables
#' @description Joint CDF Plot of Two Continuous Random Variables
#' @usage cont.jcdfp(FUN, lo1, up1, lo2, up2, mt)
#' @param FUN Continuous joint PDF function
#' @param lo1 Lower limit of x-axis
#' @param up1 Upper limit of x-axis
#' @param lo2 Lower limit of y-axis
#' @param up2 Upper limit of y-axis
#' @param mt Title of the joint PDF plot
#'
#' @return None.
#' @examples
#' pdf = function(x, y) (x+y)*(x>0 & x<1)*(y>0 & y<1)
#' library(scatterplot3d)
#' cont.jcdfp(pdf, 0, 1, 0, 1)
#' @export
cont.jcdfp <- function (FUN, lo1, up1, lo2, up2, mt)
{
  if (missing(mt))
    mt = "Continuous Cumulative Probability Distribution Function(CDF)"
  Fxy = function(x, y) Pr(FUN, lo1, x, lo2, y)[[1]]
  VFxy = Vectorize(Fxy, c("x", "y"))
  xa = seq(lo1, up1, length = 20)
  ya = seq(lo2, up2, length = 20)
  xa = rep(xa, 20)
  ya = rep(ya, each = 20)
  win.graph(7, 5)
  s3d = scatterplot3d(xa, ya, VFxy(xa, ya), highlight.3d = TRUE,
                      main = "Joint Cumulative Distribution of X and Y",
                      xlab = "x", ylab = "y", zlab = "F(x, y)",
                      pch = 20)
}
