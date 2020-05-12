#' @title Multiple Scatter Plots
#' @description Multiple Scatter Plots from Lists of Bivariate Data
#' @usage corr.mplot(X, item, xl, yl, mt, step = 1:4, alp = 0.05, dig = 4)
#' @param X List vector of bivariate data with 2 columns
#' @param item String vector of list names
#' @param xl Label of x-axis (default="group1")
#' @param yl Label of y-axis (default="group2")
#' @param mt Plot title
#' @param step Steps for correlation analysis, Default: 1:4
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' xd <- list(cbind(iris[[1]], iris[[2]]), cbind(iris[[3]], iris[[4]]))
#' spec <- c("Sepal", "Petal")
#' corr.mplot(X = xd, item = spec, xl = "Length", yl = "Width")
#' @export
corr.mplot <- function(X, item, xl, yl, mt, step = 1:4, alp = 0.05, dig = 4) {
  nv <- length(X)
  nc <- ifelse(nv <= 5, 2, 3)
  nr <- ceiling(nv / nc)
  h <- ifelse(nr > 2, 9, ifelse(nr == 1, 4, 6))
  w <- ifelse(nc > 2, 9, 7)
  if (1 %in% step) {
    cat("[Step 1] Scatter plot for prior investigation of data -------------\n")
    if (missing(mt)) {
      mt <- paste0(
        "Scatter Plot (", letters[1:nv],
        ") ", item
      )
    }
    if (missing(xl)) {
      xl <- "group1"
    }
    if (missing(yl)) {
      yl <- "group2"
    }
    win.graph(w, h)
    par(mfrow = c(nr, nc))
    for (k in 1:nv) {
      plot(X[[k]][, 1], X[[k]][, 2],
        pch = 19, main = mt[k],
        xlab = xl, ylab = yl
      )
      abline(lm(X[[k]][, 2] ~ X[[k]][, 1]), lty = 2, col = 2)
    }
  }
  if (2 %in% step) {
    cat("[Step 2] Estimate correlation coefficients -----------------\n")
    var1 <- var2 <- cov12 <- cor12 <- rep(NA, nv)
    for (k in 1:nv) {
      var1[k] <- var(X[[k]][, 1])
      var2[k] <- var(X[[k]][, 2])
      cov12[k] <- cov(X[[k]][, 1], X[[k]][, 2])
      cor12[k] <- cor(X[[k]][, 1], X[[k]][, 2])
      cat(paste0(
        item[k], "\t Corr(X1,X2) = ", round(
          cov12[k],
          dig
        ), " / √(", round(var1[k], dig), " × ",
        round(var2[k], dig), ") = ", round(
          cor12[k],
          dig
        )
      ), "\n")
    }
  }
  ct <- list()
  for (k in 1:nv) {
    ct[[k]] <- cor.test(X[[k]][, 1], X[[k]][
      ,
      2
    ], conf.level = 1 - alp)
  }
  if (3 %in% step) {
    cat("[Step 3] Correlation tests ------------------------\n")
    for (k in 1:nv) {
      cat(item[k], "\t Corr =", round(
        ct[[k]]$est,
        dig
      ), " \t t-Stat =", round(
        ct[[k]]$stat,
        dig
      ), "\t P-v =", round(
        ct[[k]]$p.val,
        dig
      ), "\n")
    }
  }
  if (4 %in% step) {
    cat("[Step 4] Confidence intervals for correlation coefficients ------------------------\n")
    for (k in 1:nv) {
      cat(item[k], "\t ", paste0(
        100 * (1 - alp),
        "% CI = [", round(ct[[k]]$conf[1], 4),
        ", ", round(ct[[k]]$conf[2], 4), "]\n"
      ))
    }
  }
  if (5 %in% step) {
    cat("[Step 5] T-test plot ------------------------\n")
    win.graph(w, h)
    par(mfrow = c(nr, nc))
    for (k in 1:nv) {
      mr <- max(c(3, abs(ct[[k]]$stat * 1.2)))
      ttest.plot(ct[[k]]$stat, ct[[k]]$para, prng = c(
        -mr,
        mr
      ), dig = 2, mt = item[k], pvout = F)
    }
  }
}
