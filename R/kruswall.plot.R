#' @title Kruskal Wallis Test
#' @description Kruskal Wallis Test with a Plot
#' @usage kruswall.plot(x, y, dig = 4, plot = FALSE)
#' @param x Data vector
#' @param y Vector of factor levels
#' @param dig Number of digits below the decimal point, Default: 4
#' @param plot Plot test results? Default: FALSE
#'
#'
#' @return None.
#' @examples
#' x <- c(4.6, 10.6, 8.0, 25.0, 7.1, 2.9, 10.1, 3.2, 3.8, 6.6, 6.8, 9.4, 26.5, 12.8, 8.3, 3.4, 3.9, 6.0, 8.6, 5.0)
#' y <- rep(1:4, each = 5)
#' kruswall.plot(x, y, plot = TRUE)
#' @export

kruswall.plot <- function(x, y, dig = 4, plot = FALSE) {
  nn <- length(x)
  kk <- length(unique(y))
  ni <- as.vector(table(y))
  ns <- c(0, cumsum(ni))
  rx <- rank(x)
  rs <- tapply(rx, y, sum)
  rtab <- matrix(0, kk, max(ni))
  for (k in 1:kk) {
    rtab[k, 1:ni[k]] <- rx[(ns[k] + 1):ns[k +
      1]]
  }
  rtab <- cbind(rtab, rs)
  rownames(rtab) <- paste0("Group", 1:kk)
  colnames(rtab) <- c(1:max(ni), "Sum")
  cat("Rank Sums for each Group -----------\n")
  print(rtab)
  H <- 12 / nn / (nn + 1) * sum(rs^2 / ni) - 3 * (nn + 1)
  pv <- pchisq(H, kk - 1, lower.tail = F)
  cat("Kruskal Wallis Test ----------\n")
  cat(paste0(
    "H = (12 / ", nn, " / ", nn + 1, ") × ",
    round(sum(rs^2 / ni), dig), " - 3 × ", nn + 1, " = ",
    round(H, dig)
  ), "\n")
  cat(paste0("df=", kk - 1, "\t p-value=", round(
    pv,
    dig
  )), "\n")
  if (plot) {
    chitest.plot2(stat = H, df = kk - 1, side = "up")
  }
}
