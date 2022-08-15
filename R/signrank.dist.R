#' @title Distribution of Wilcoxon Signed Rank
#' @description Distribution of Wilcoxon Signed Rank Test Statistic
#' @usage signrank.dist(nv = 5:50, av, tab = TRUE, plot = FALSE, dig = 4)
#' @param nv Number of signed data, Default: 5:50
#' @param av Probability vector (default=c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995))
#' @param tab Print quantile table? Default: TRUE
#' @param plot Plot test statistic distribution? Default: FALSE
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' signrank.dist(nv = 5:15)
#' signrank.dist(nv = c(5, 7, 10, 20), tab = FALSE, plot = TRUE)
#' @export

signrank.dist <- function(nv = 5:50, av, tab = TRUE, plot = FALSE, dig = 4) {
  if (missing(av)) {
    av <- c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995)
  }
  nn <- length(nv)
  na <- length(av)
  if (tab) {
    cv <- array(NA, dim = c(nn, na))
    colnames(cv) <- av
    rownames(cv) <- paste0("n=", nv)
    for (i in 1:na) cv[, i] <- qsignrank(av[i], nv)
    print(cv)
  }
  if (plot) {
    nc <- switch(nn, 1, 2, 3, 2, 3, 3, 3, 3, 3)
    nr <- switch(nn, 1, 1, 1, 2, 2, 2, 3, 3, 3)
    wc <- switch(nn, 7, 7, 9, 7, 9, 9, 9, 9, 9)
    wr <- switch(nn, 5, 4, 4, 6, 6, 6, 9, 9, 9)
    dev.new(wc, wr)
    par(mfrow = c(nr, nc))
    for (n in nv) {
      mu <- n * (n + 1) / 4
      vw <- n * (n + 1) * (2 * n + 1) / 24
      dw <- sqrt(vw)
      xa <- 0:(n * (n + 1) / 2)
      ymax <- max(dsignrank(xa, n = n), dnorm(mu, mu, dw))
      plot(xa, dsignrank(xa, n = n),
        type = "h",
        lwd = 2, col = 2, ylab = "f(w)", xlab = "w",
        ylim = c(0, ymax), main = paste0(
          "W-Signed Rank (n = ",
          n, ")"
        )
      )
      xa2 <- seq(0, mu * 2, length = 100)
      lines(xa2, dnorm(xa2, mu, dw), col = 4)
    }
  }
}
