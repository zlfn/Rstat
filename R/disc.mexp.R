#' @title Expected Values of Discrete Random Variables

#' @description Expected Values of Discrete Random Variables


#' @usage disc.mexp(xv, fx, fx2, fx3, mt, dig = 3, del = 0.2, prt = TRUE, plot = TRUE)
#' @param xv Vector of random variable values
#' @param fx List of probability distributions (2~9)
#' @param fx2 Second list of probability distributions (same number of fx)
#' @param fx3 Third list of probability distributions (same number of fx)
#' @param mt Vector of plot titles
#' @param dig Number of digits below decimal point, Default: 3
#' @param del Distance between the probability distribution plots, Default: 0.2
#' @param prt Print the expected values and variances? Default: TRUE
#' @param plot Plot the probability distributions? Default: TRUE
#'
#' @return list(Ex, Vx, Dx)
#' @examples
#' # Binomial distributions
#' n <- 10
#' p <- c(0.2, 0.5, 0.8)
#' x <- 0:n
#' fx1 <- fx2 <- list()
#' for (i in 1:3) fx1[[i]] <- dbinom(x, n, p[i])
#' mt1 <- paste0("B(10,", p, ")")
#' disc.mexp(x, fx1, mt = mt1)
#' # Binomial vs. Hypergeometric
#' N <- 50
#' S <- c(10, 25, 40)
#' n <- 10
#' x <- 0:n
#' for (i in 1:3) fx2[[i]] <- dhyper(x, S[i], N - S[i], n)
#' mt12 <- paste0("HG(10,50,", S, "):B(10,", p, ")")
#' disc.mexp(x, fx2, fx1, mt = mt12)
#' @export

disc.mexp <- function(xv, fx, fx2, fx3, mt, dig = 3, del = 0.2, prt = TRUE,
                      plot = TRUE) {
  ng <- length(fx)
  if (ng > 9) {
    stop("Number of probability distribution must be in 2~9!")
  }
  Add2 <- ifelse(missing(fx2), FALSE, TRUE)
  Add3 <- ifelse(missing(fx3), FALSE, TRUE)
  Xn <- toupper(deparse(substitute(xv)))
  Ex <- Exs <- Vx <- Dx <- list()
  for (k in 1:ng) {
    Ex[[k]] <- sum(xv * fx[[k]])
    Exs[[k]] <- sum(xv^2 * fx[[k]])
    Vx[[k]] <- Exs[[k]] - Ex[[k]]^2
    Dx[[k]] <- sqrt(Vx[[k]])
  }
  if (Add2) {
    prt <- FALSE
  }
  if (Add3) {
    prt <- FALSE
  }
  if (prt == TRUE) {
    for (k in 1:ng) {
      cat(paste0("E(", Xn, ") = ", round(
        Ex[[k]],
        dig
      )), "\t ")
      cat(paste0(
        "V(", Xn, ") = ", round(
          Exs[[k]],
          dig
        ), " - ", round(Ex[[k]], dig), "² = ",
        round(Vx[[k]], dig)
      ), "\t ")
      cat(paste0("D(", Xn, ") = √(", round(
        Vx[[k]],
        dig
      ), ") = ", round(Dx[[k]], dig)), "\n")
    }
  }
  if (plot == TRUE) {
    if (missing(mt)) {
      mt <- paste0(
        "Probability Distribution of ",
        "X", 1:ng
      )
    }
    nc <- switch(ng, 1, 2, 3, 2, 3, 3, 3, 3, 3)
    nr <- switch(ng, 1, 1, 1, 2, 2, 2, 3, 3, 3)
    win.graph(3 * nc, 3 * nr)
    par(mfrow = c(nr, nc))
    for (k in 1:ng) {
      plot(xv, fx[[k]],
        type = "h", main = mt[k],
        ylab = "f(x)", xlab = "x", lwd = 3,
        col = 2
      )
      if (Add2) {
        lines(x - del, fx2[[k]],
          type = "h", lwd = 3,
          col = 4
        )
      }
      if (Add3) {
        lines(x + del, fx3[[k]],
          type = "h", lwd = 3,
          col = "green3"
        )
      }
    }
  }
  invisible(list(Ex, Vx, Dx))
}
