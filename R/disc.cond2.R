#' @title Conditional PDF of Discrete Random Variables
#' @description Conditional Probability Distribution of Discrete Random Variables
#' @usage disc.cond2(tabXY, Xs, Ys, prt = TRUE, plot = FALSE, dig = 5, dig2 = 4)
#'
#' @param tabXY Joint frequency table of two random variables
#' @param Xs Conditioning value of X
#' @param Ys Conditioning value of Y
#' @param prt Print the marginal frequency and probability? Default: TRUE
#' @param plot Plot the marginal PDF? Default: FALSE
#' @param dig Number of digits below the decimal point in the console, Default: 5
#' @param dig2 Number of digits below the decimal point in the graph, Default: 4
#'
#'
#'
#' @return Conditional Frequency and PDF
#' @examples
#' fxy <- with(mtcars, table(cyl, carb))
#' disc.cond2(fxy, Ys = 1:4, plot = TRUE)
#' @export

disc.cond2 <- function(tabXY, Xs, Ys, prt = TRUE, plot = FALSE, dig = 5, dig2 = 4) {
  N <- sum(tabXY)
  tabX <- apply(tabXY, 1, sum)
  tabY <- apply(tabXY, 2, sum)
  xa <- as.numeric(names(tabX))
  nx <- length(xa)
  ya <- as.numeric(names(tabY))
  ny <- length(ya)
  nc <- ifelse(missing(Xs), length(Ys), length(Xs))
  cpdf <- vector("list", nc)
  cfreq <- vector("list", nc)
  if (missing(Xs)) {
    Cs <- Ys
    Cn <- "Y"
    Dn <- "X"
    da <- xa
    yla <- "f(x|y)"
    for (k in 1:nc) {
      Cv <- as.character(Cs[k])
      cfreq[[k]] <- tabXY[, Cv]
      cpdf[[k]] <- cfreq[[k]] / sum(cfreq[[k]])
    }
  }
  if (missing(Ys)) {
    Cs <- Xs
    Cn <- "X"
    Dn <- "Y"
    da <- ya
    yla <- "f(y|x)"
    for (k in 1:nc) {
      Cv <- as.character(Cs[k])
      cfreq[[k]] <- tabXY[Cv, ]
      cpdf[[k]] <- cfreq[[k]] / sum(cfreq[[k]])
    }
  }
  if (prt == TRUE) {
    for (k in 1:nc) {
      cat(
        paste0(
          "Conditional prob. dist. of ", Dn,
          " (", Cn, " = ", Cs[k], ")"
        ),
        "\n"
      )
      if (N < 2) {
        print(cpdf[[k]])
      }
      else {
        cat(
          paste(names(cpdf[[k]]), collapse = "\t "),
          "\n"
        )
        cat(paste(paste0(cfreq[[k]], "/", sum(cfreq[[k]])),
          collapse = "\t "
        ), "\n")
      }
    }
  }
  if (plot == TRUE) {
    dr <- switch(nc, 1, 2, 2, 2, 2, 2, 3, 3, 3)
    dc <- switch(nc, 1, 1, 2, 2, 3, 3, 3, 3, 3)
    ww <- switch(nc, 7, 7, 8, 8, 9, 9, 9, 9, 9)
    wh <- switch(nc, 3, 6, 6, 6, 6, 6, 9, 9, 9)
    win.graph(ww, wh)
    par(mfrow = c(dr, dc))
    par(mar = c(3, 4, 4, 2))
    for (k in 1:nc) {
      plot(da, cpdf[[k]],
        type = "h", main = paste0(
          "Cond. Prob. Dist. of ",
          Dn, " | ", Cn, "=", Cs[k]
        ), ylim = c(
          0,
          max(cpdf[[k]]) * 1.15
        ), xlab = "", ylab = yla,
        pch = 16, lwd = 5, col = 2
      )
      text(da, cpdf[[k]], paste0(
        cfreq[[k]], "/",
        sum(cfreq[[k]])
      ), pos = 3, col = 4, cex = 0.8)
    }
  }
  invisible(list(freq = cfreq, pdf = cpdf))
}
