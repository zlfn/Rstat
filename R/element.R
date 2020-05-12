#' @title Elements of an Event
#' @description Displaying Elements of an Event
#' @usage element(A, r = 10)
#' @param A Event in data frame.
#' @param r Maximum number of elements in a line. Default: 10
#'
#'
#' @return Vector of elements of the event.
#' @examples
#' S <- rolldie2(2)
#' B <- subset(S2, (X1 + X2) >= 8)
#' element(B)
#' @export
element <- function(A, r = 10) {
  n <- nrow(A)
  d <- ncol(A)
  m <- ceiling(n / r)
  elem <- "("
  if (d >= 2) {
    for (k in 1:(d - 1)) elem <- paste0(elem, A[[k]], ",")
  }
  elem <- paste0(elem, A[[d]], ")")
  for (k in 1:m) {
    cat(
      elem[(r * (k - 1) + 1):min(r * k, n)],
      "\n"
    )
  }
  invisible(elem)
}
