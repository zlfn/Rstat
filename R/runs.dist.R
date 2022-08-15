#' @title Distribution of Runs

#' @description Distribution of Runs Test Statistic


#' @usage runs.dist(n1 = 2:20, n2 = 2:20, alp = 0.05, tab = TRUE, side = "two", plot = FALSE)
#' @param n1 Number of data in group 1, Default: 2:20
#' @param n2 Number of data in group 2, Default: 2:20
#' @param alp Level of significance, Default: 0.05
#' @param tab Print critical value table? Default: TRUE
#' @param side Type of alternative hypothesis, Default: 'two'
#' @param plot Plot run distribution? Default: FALSE
#'
#'
#' @return None.
#' @examples
#' require(randomizeBE)
#' runs.dist(n1 = 2:10, n2 = 2:10)
#' runs.dist(n1 = c(5, 20), n2 = c(5, 20), tab = FALSE, plot = TRUE)
#' @export
runs.dist <-
  function(n1 = 2:20, n2 = 2:20, alp = 0.05, tab = TRUE, side = "two",
           plot = FALSE) {
    nn1 <- length(n1)
    nn2 <- length(n2)
    lcv <- ucv <- array(NA, dim = c(nn1, nn2))
    rownames(lcv) <- rownames(ucv) <- n1
    colnames(lcv) <- colnames(ucv) <- n2
    alpha <- ifelse(grepl(side, "two- sided"), alp, 2 *
      alp)
    for (k1 in 1:nn1) {
      for (k2 in 1:nn2) {
        temp <- cvruns.exact(alpha, n1[k1], n2[k2])
        lcv[k1, k2] <- temp$lcr
        ucv[k1, k2] <- temp$ucr
      }
    }
    if (tab) {
      if (any(grepl(side, c("low", "less")))) {
        cat(
          "Lower (one-sided) Critical Value (Level of significance =",
          100 * alp, "%) ----------\n"
        )
        print(lcv)
      }
      else if (any(grepl(side, c("up", "greater")))) {
        cat(
          "Upper (one-sided) Critical Value (Level of significance =",
          100 * alp, "%) ----------\n"
        )
        print(ucv)
      }
      else {
        cat(
          "Lower (two-sided) Critical Value (Level of significance =",
          100 * alp, "%) ----------\n"
        )
        print(lcv)
        cat(
          "Upper (two-sided) Critical Value (Level of significance =",
          100 * alp, "%) ----------\n"
        )
        print(ucv)
      }
    }
    if (plot) {
      dev.new(3.5 * nn2, 3 * nn1)
      par(mfrow = c(nn1, nn2))
      for (k1 in n1) {
        for (k2 in n2) {
          rmax <- ifelse(k1 == k2, 2 * k1, 2 * min(k1, k2) +
            1)
          x <- 2:rmax
          plot(x, Vdruns.exact(x, k1, k2),
            type = "h",
            lwd = 4, col = 2, ylab = "f(x)", main = paste0(
              "Runs PDF (n1 = ",
              k1, ", n2 =", k2, ")"
            )
          )
        }
      }
    }
  }
