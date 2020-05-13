#' @title Probability of an Event
#' @description Calculating the Probability of an Event
#' @usage pprt(x, n, prt = TRUE)
#' @param x Event in data frame.
#' @param n Size of the sample space.
#' @param prt Print output? Default: TRUE
#'
#'
#' @return The probability of an event.
#' @examples
#' S <- rolldie2(2)
#' B <- subset(S, (X1 + X2) >= 8)
#' pprt(B, nrow(S))
#' @export
pprt <-
  function(x, n, prt = TRUE) {
    en <- deparse(substitute(x))
    if (prt == TRUE) {
      cat(
        paste0("P(", en, ") ="), nrow(x), "/",
        n, "=", nrow(x) / n, "\n"
      )
    }
    invisible(nrow(x) / n)
  }
