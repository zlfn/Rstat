#' @title Distribution of Wilcoxon Rank Sum
#' @description Distribution of Wilcoxon Rank Sum Test Statistic
#' @usage ranksum.dist(n1, n2 = 3:10, tab = TRUE, plot = FALSE, dig = 4)
#' @param n1 Number of data in group 1 (default=1:min(n2))
#' @param n2 Number of data in group 2, Default: 3:10
#' @param tab Print critical value table? Default: TRUE
#' @param plot Plot rank sum distribution? Default: FALSE
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' ranksum.dist(n2 = 3:5)
#' ranksum.dist(n1 = c(2, 4, 6, 8), n2 = 10, tab = FALSE, plot = TRUE)
#' @export

ranksum.dist <-
  function(n1, n2 = 3:10, tab = TRUE, plot = FALSE, dig = 4) {
    if (tab) {
      for (k2 in n2) {
        nu <- floor(k2^2 / 2) + 1
        pv <- array(NA, dim = c(nu, k2))
        colnames(pv) <- paste0("n1=", 1:k2)
        rownames(pv) <- paste0("U=", 0:(nu - 1))
        for (k1 in 1:k2) {
          pv[, n1] <- pwilcox(
            0:(nu - 1), k1,
            k2
          )
        }
        cat(paste0("n2=", k2))
        print(round(pv, dig))
      }
    }
    if (plot) {
      win.graph(7, 5)
      if (missing(n1)) {
        n1 <- 1:min(n2)
      }
      nn <- length(n1) * length(n2)
      if (nn < 6) {
        dcol <- c(
          1, 2, "green4", 4, "purple",
          6
        )
      }
      else {
        dcol <- rainbow(nn)
      }
      lab <- rep("", nn)
      ll <- 0
      xa <- 0:(max(n1) * max(n2))
      plot(xa, dwilcox(xa, min(n1), min(n2)),
        type = "n",
        main = "Wilcoxon Rank Sum Distribution", lwd = 2,
        xlab = "Rank Sum Statistic(U)", ylab = "f(u)"
      )
      for (k1 in n1) {
        for (k2 in n2) {
          ll <- ll + 1
          lines(xa, dwilcox(xa, k1, k2),
            type = "s",
            lwd = 2, col = dcol[ll]
          )
          lab[ll] <- paste0(
            "(n1,n2)=(", k1, ",",
            k2, ")"
          )
        }
      }
      legend("topright", lab,
        lwd = 2, col = dcol[1:nn],
        text.col = dcol[1:nn]
      )
    }
  }
