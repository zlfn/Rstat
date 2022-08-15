
#' @title Stratified Histograms

#' @description To create Stratified Histograms from a mixed data set


#' @usage strat.hist(ng = 3, n = 200, m = c(6, 10, 14), s = 1, sp = c(4, 16), vc, prob = FALSE)
#' @param ng Number of classes. Defaults to 3.
#' @param n Number of samples. Defaults to 200.
#' @param m Vector of means. Defaults to c(6,10,14)
#' @param s Standard deviations. Default: 1
#' @param sp Specification limits. Default: c(4, 16)
#' @param vc Colors of histograms.
#' @param prob Density scale? Default: FALSE
#'
#' @return None.
#' @examples
#' strat.hist()
#' strat.hist(ng = 4, m = c(6, 10, 14, 18), sp = c(4, 20))
#' @export

strat.hist <-
  function(ng = 3, n = 200, m = c(6, 10, 14), s = 1, sp = c(
             4,
             16
           ), vc, prob = FALSE) {
    if (ng >= 9) {
      stop("Number of class should be less than 9!")
    }
    if (length(s) == 1) {
      s <- rep(s, ng)
    }
    p <- (1:n) / (n + 1)
    x <- vector("list", ng)
    for (k in 1:ng) x[[k]] <- qnorm(p, mean = m[k], sd = s[k])
    xd <- x[[1]]
    for (k in 2:ng) xd <- c(xd, x[[k]])
    nb1 <- ceiling(sqrt(n * ng))
    nb2 <- ceiling(sqrt(n))
    nr <- ifelse(ng <= 5, 2, 3)
    nc <- ceiling((ng + 1) / nr)
    h <- ifelse(nr > 2, 9, 6)
    w <- ifelse(nc > 2, 9, 7)
    mt <- paste0("group-", LETTERS[1:ng])
    xl <- c(min(m[1] - 3 * s[1], sp[1] - 2 * s[1]), max(m[ng] +
      3 * s[ng], sp[2] + 2 * s[ng]))
    if (missing(vc)) {
      vc <- c("orange", rep("cyan", ng))
    }
    dev.new(w, h)
    par(mfrow = c(nr, nc))
    par(mar = c(3, 3, 4, 1))
    xh <- hist(xd,
      breaks = nb1, main = "All Data", probability = prob,
      ylab = "", xlab = "", xlim = xl, col = vc[1]
    )
    ym <- ifelse(prob, max(xh$density), max(xh$counts))
    brk <- xh$breaks
    segments(sp, 0, sp, ym / 2, lwd = 2, col = 2)
    text(sp, c(ym / 2, ym / 2), c("SL", "SU"),
      col = 2,
      pos = 3
    )
    for (k in 1:ng) {
      hist(x[[k]],
        breaks = brk, main = mt[k], probability = prob,
        ylab = "", xlab = "", ylim = c(0, ym),
        xlim = xl, col = vc[k + 1]
      )
      segments(sp, 0, sp, ym / 2, lwd = 2, col = 2)
      text(sp, c(ym, ym) / 2, c("SL", "SU"),
        col = 2,
        pos = 3
      )
    }
  }
