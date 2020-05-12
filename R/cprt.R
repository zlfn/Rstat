#' @title Conditional Probability
#' @description Calculating the Conditional Probability
#' @usage cprt(a, b)
#'
#' @param a Event to be conditioned.
#' @param b Event to be conditioning.
#'
#' @return None.
#' @examples
#' S <- rolldie2(2)
#' A <- subset(S, (X1 + X2) >= 4)
#' B <- subset(S, (X1 + X2) >= 8)
#' cprt(B, A)
#' @export

cprt <- function(a, b) {
  an <- deparse(substitute(a))
  bn <- deparse(substitute(b))
  ab <- intersect2(a, b)
  cat(
    paste0("P(", an, "|", bn, ")="), nrow(ab),
    "/", nrow(b), "=", nrow(ab) / nrow(b), "\n"
  )
}
