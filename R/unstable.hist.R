#' @title Unstable Histograms

#' @description To create 4 types of unstable histograms


#' @usage unstable.hist(N = 200, m1 = 10, s1 = 1, m2 = 6, s2 = 0.5, a = 8, b = 9, c = 9, vc = rep("cyan", 4))
#' @param N Number of random variates. Defaults to 200.
#' @param m1 Means of two normal distributions.
#' @param m2 Means of two normal distributions.
#' @param s1 Standard deviations of two normal distributions.
#' @param s2 Standard deviations of two normal distributions.
#' @param mp Logical variable for making histogram. Defaults to FALSE.
#' @param ... Other graphic parameters
#'
#'
#' @return None.
#' @examples
#' unstable.hist()
#' unstable.hist(m1 = 15, s1 = 2, m2 = 8, s2 = 1, a = 12, b = 13, c = 10)
#' @export

unstable.hist <-
  function(N = 200, m1 = 10, s1 = 1, m2 = 6, s2 = 0.5, a = 8,
           b = 9, c = 9, vc = rep("cyan", 4)) {
    p1 <- (1:(0.9 * N)) / (0.9 * N + 1)
    p2 <- (1:(0.5 * N)) / (0.5 * N + 1)
    p3 <- (1:(0.1 * N)) / (0.1 * N + 1)
    x1 <- qnorm(p1, mean = m1, sd = s1)
    x2 <- qnorm(p2, mean = m1, sd = s1)
    y1 <- qnorm(p2, mean = m2, sd = s1)
    y2 <- qnorm(p3, mean = m2, sd = s2)
    da <- c(x1, y2)
    db <- c(x2, y1)
    dc <- x1[(x1 > b) | (x1 < a)]
    dd <- x1[x1 >= c]
    win.graph(7, 6)
    par(mfrow = c(2, 2))
    hist(da,
      breaks = 15, main = "Type-A", xlab = "(a)",
      col = vc[1]
    )
    hist(db,
      breaks = 15, main = "Type-B", xlab = "(b)",
      col = vc[2]
    )
    hist(dc,
      breaks = 12, main = "Type-C", xlab = "(c)",
      col = vc[3]
    )
    hist(dd,
      breaks = 12, main = "Type-D", xlab = "(d)",
      col = vc[4]
    )
  }
