#' @title Multinomial PDF Plot
#' @description Graphic Display of Multinomial Probability Distribution Function
#' @usage multinorm.plot(ps, size)
#'
#' @param ps Probability (or proportion) matrix with column for each group
#' @param size Sample size
#'
#'
#' @return Matrix of multinomial PDF
#' @examples
#' library(scatterplot3d)
#' ps <- matrix(c(1, 1, 8, 1, 5, 4, 4, 4, 2, 1, 1, 1), nrow = 4, ncol = 3, byrow = T)
#' multinorm.plot(ps, 5)
#' @export
multinorm.plot <- function(ps, size) {
  nc <- 3
  ng <- nrow(ps)
  xr <- urnsample2(1:nc, size = size, replace = TRUE, ordered = FALSE)
  nr <- nrow(xr)
  cat("Number of Possible Combinations =", nr, "\n")
  x <- list()
  for (i in 1:nc) {
    x[[i]] <- apply(xr, 1, function(x) {
      sum(x ==
        i)
    })
  }
  cat("Combinations of Random Variable Vectors -------------\n")
  for (i in 1:nc) print(x[[i]])
  xm <- x[[1]]
  for (i in 2:nc) xm <- cbind(xm, x[[i]])
  fx6 <- matrix(NA, nr, ng)
  for (j in 1:ng) {
    for (i in 1:nr) {
      fx6[i, j] <- dmultinom(xm[i, ], size = size, prob = ps[j, ])
    }
  }
  colnames(fx6) <- paste0("P", 1:ng)
  print(apply(fx6, 2, sum))
  probs <- rep("", ng)
  if (sum(ps[1, ]) > 1) {
    for (j in 1:ng) {
      probs[j] <- paste(paste0(
        ps[j, ], "/",
        sum(ps[j, ])
      ), collapse = ",")
    }
  }
  else {
    for (j in 1:ng) probs[j] <- paste(ps[j, ], collapse = ",")
  }
  mt6 <- paste0("Multinom(", probs, ")")
  wc <- c(1, 2, 3, 2, 3, 3, 4, 4, 3, 4)
  wr <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3)
  ww <- c(4, 6, 9, 7, 9, 9, 9, 9, 9, 9)
  wl <- c(3, 3, 3, 6, 6, 6, 6, 6, 9, 9)
  dev.new(ww[ng], wl[ng])
  par(mfrow = c(wr[ng], wc[ng]))
  for (k in 1:ng) {
    scatterplot3d(x[[1]], x[[2]], fx6[, k],
      type = "h",
      main = mt6[k], zlab = "f(x1,x2,x3)", xlab = "x1",
      ylab = "x2", pch = 16, lwd = 5, color = 2
    )
  }
  dum <- matrix(NA, nc, nr)
  for (i in 1:nc) dum[i, ] <- x[[i]]
  rownames(fx6) <- paste0(
    "p(", apply(dum, 2, paste, collapse = ","),
    ")"
  )
  invisible(fx6)
}
