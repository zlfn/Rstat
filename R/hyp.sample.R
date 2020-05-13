#' @title PDF of Successes from Finite Population

#' @description Probability Distribution of the Number of Successes from Finite Population

#' @usage hyp.sample(npop, ndef, nsamp, cex = 0.8, dig = 4)


#' @param npop Population size
#' @param ndef Number of success items in the population
#' @param nsamp Sample size
#' @param cex Text size, Default: 0.8
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return the PDF.
#' @examples
#' hyp.sample(50, 8, 10)
#' @export
hyp.sample <- function(npop, ndef, nsamp, cex = 0.8, dig = 4) {
  denom <- choose(npop, nsamp)
  freq <- choose(ndef, 0:nsamp) * choose(npop - ndef, nsamp -
    (0:nsamp))
  names(freq) <- 0:nsamp
  print(freq)
  cat("sum(freq) =", sum(freq), "\n")
  fx <- freq / denom
  print(round(fx, dig))
  cat("sum(f(x)) =", sum(fx), "\n")
  X.val <- 0:nsamp
  EX <- sum(X.val * fx)
  EX2 <- sum(X.val^2 * fx)
  VX <- EX2 - EX^2
  DX <- sqrt(VX)
  Xmin <- min(X.val) - 1
  Xmax <- max(X.val) + 1
  win.graph(7, 5)
  plot(X.val, fx,
    type = "h", col = "red", lwd = 4,
    xlim = c(Xmin, Xmax), ylim = c(0, max(fx) + 0.05), main = paste0(
      "Prob. Distn. of Successes in ",
      nsamp, " Samples out of (", ndef, "/",
      npop, ")"
    ), xlab = "Number of Successes",
    ylab = "f(x)"
  )
  text(X.val, fx,
    labels = round(fx, dig), pos = 3, cex = cex,
    col = 4
  )
  legend("topright", c(paste("E(X) =", EX), paste(
    "D(X) =",
    round(DX, 4)
  )), bg = "white")
  invisible(fx)
}
