#' @title One-way analysis of variance
#' @description One-way analysis of variance
#' @usage anova1(y, f, xl = "Factor", yl = "Response Variable", step = 0:7, alp = 0.05, dig = 4)
#' @param y Response variable data
#' @param f Factors (same length as y)
#' @param xl Label of x-axis, Default: 'Factor'
#' @param yl Label of y-axis, Default: 'Response Variable'
#' @param step Steps of the analysis of variance, Default: 0:7
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' y <- c(79, 83, 88, 78, 75, 81, 89, 91, 84, 86, 82, 86, 91, 93, 90, 89, 76, 81, 82, 79)
#' f <- c(rep(150, 5), rep(200, 6), rep(250, 5), rep(300, 4))
#' anova1(y, f, xl = "Temp", yl = "Yield")
#' @export

anova1 <- function(y, f, xl = "Factor", yl = "Response Variable",
                   step = 0:7, alp = 0.05, dig = 4) {
  m1 <- length(unique(f))
  nn <- length(y)
  af <- as.factor(f)
  ym <- tapply(y, af, mean)
  if (0 %in% step) {
    cat(paste0(
      "[Step 0] Mean of ", yl, " for each level of ",
      xl
    ), "---------\n")
    print(round(ym, dig))
  }
  if (1 %in% step) {
    cat(paste0(
      "[Step 1] Box Plot of ", yl, " for each level of ",
      xl
    ), "---------\n")
    win.graph(7, 5)
    boxplot(y ~ af, col = 7, main = paste0(
      "Box Plot of ",
      yl, " for each level of ", xl
    ), xlab = paste(
      "Level of",
      xl
    ), ylab = yl)
    points(af, y, pch = 19, col = 2, cex = 1.2)
    lines(1:m1, ym,
      type = "b", lty = 2, pch = 17,
      col = 4, cex = 1.2
    )
  }
  an1 <- aov(y ~ af)
  ans1 <- summary(an1)[[1]]
  if (2 %in% step) {
    SSt <- sum(y^2) - sum(y)^2 / nn
    antab <- matrix(NA, 3, 5)
    colnames(antab) <- c(
      "Sum Sq.", "df", "Mean Sq.",
      "F0", "P-value"
    )
    rownames(antab) <- c(xl, "Error", "Total")
    antab[1:2, ] <- cbind(
      ans1$Sum, ans1$Df, ans1$Mean, ans1$F,
      ans1$Pr
    )
    antab[3, 1] <- SSt
    antab[3, 2] <- nn - 1
    dum <- round(antab, dig)
    dum[is.na(dum)] <- ""
    cat(paste0(
      "[Step 2] ANOVA Table of ", yl, " w.r.t. ",
      xl
    ), "---------\n")
    print(as.data.frame(dum))
  }
  if (3 %in% step) {
    cat(paste0(
      "[Step 3] ANOVA Diagnostic Plot of ",
      yl, " w.r.t. ", xl
    ), "---------\n")
    win.graph(7, 4)
    par(mfrow = c(1, 2))
    plot(an1, which = 1:2)
  }
  ni <- tapply(y, af, length)
  mse <- summary(an1)[[1]]$Mean[2]
  se <- sqrt(mse / ni)
  tol <- qt(1 - alp / 2, nn - m1) * se
  lcl <- ym - tol
  ucl <- ym + tol
  if (4 %in% step) {
    cat(paste0(
      "[Step 4] ", (1 - alp) * 100, "% CI for the Mean of ",
      yl, " w.r.t. ", xl
    ), "---------\n")
    cat("MSE =", round(mse, dig), "\n")
    print(round(rbind(ym, tol, lcl, ucl), dig))
  }
  if (5 %in% step) {
    cat(paste0(
      "[Step 5] ", (1 - alp) * 100, "% CI Plot for the Mean of ",
      yl, " w.r.t. ", xl
    ), "---------\n")
    width <- 7 + min(8, max(0, m1 - 6) * 0.5)
    win.graph(width, 5)
    fnum <- as.numeric(af)
    lev <- 1:m1
    x1 <- 0.5
    x2 <- m1 + 0.5
    ymin <- min(y, lcl)
    ymax <- max(y, ucl)
    y1 <- ymin - 0.1 * (ymax - ymin)
    y2 <- ymax + 0.1 * (ymax - ymin)
    plot(fnum, y,
      pch = 19, col = 3, cex = 1.2, xaxt = "n",
      main = paste0(
        (1 - alp) * 100, "% CI for the Mean of ",
        yl, " w.r.t. ", xl
      ), ylab = yl, xlab = xl,
      xlim = c(x1, x2), ylim = c(y1, y2)
    )
    grid(col = 3)
    axis(1, at = 1:m1, labels = levels(af))
    lines(lev, ym,
      type = "b", lty = 2, pch = 17, col = 2,
      cex = 1.2
    )
    lines(lev, lcl,
      type = "b", lty = 4, pch = 18,
      col = 4, cex = 1.2
    )
    lines(lev, ucl,
      type = "b", lty = 4, pch = 18,
      col = 4, cex = 1.2
    )
    arrows(lev, lcl, lev, ucl,
      lwd = 2, length = 0.1, code = 3,
      angle = 90
    )
    text(lev, ym,
      labels = round(ym, 3), cex = 0.9, col = 2,
      pos = 4
    )
    text(lev, lcl,
      labels = round(lcl, 3), cex = 0.9, col = 4,
      pos = 4
    )
    text(lev, ucl,
      labels = round(ucl, 3), cex = 0.9, col = 4,
      pos = 4
    )
  }
  if (6 %in% step | 7 %in% step) {
    ym2 <- ym[1] - ym[-1]
    for (k in 2:(m1 - 1)) ym2 <- c(ym2, ym[k] - ym[-(1:k)])
    ylv <- paste0("A", 1:m1)
    nylv <- paste0(ylv[1], "-", ylv[-1])
    for (k in 2:(m1 - 1)) {
      nylv <- c(nylv, paste0(
        ylv[k], "-",
        ylv[-(1:k)]
      ))
    }
    names(ym2) <- nylv
    env <- sqrt(1 / ni[1] + 1 / ni[-1])
    for (k in 2:(m1 - 1)) env <- c(env, sqrt(1 / ni[k] + 1 / ni[-(1:k)]))
    tol2 <- qt(1 - alp / 2, nn - m1) * sqrt(mse) * env
    names(tol2) <- names(ym2)
    lcl2 <- ym2 - tol2
    ucl2 <- ym2 + tol2
    cat(paste0(
      "[Step 6] ", (1 - alp) * 100, "% CI for the Mean Differences of ",
      yl, " w.r.t. ", xl
    ), "---------\n")
    print(round(rbind(ym2, tol2, lcl2, ucl2), dig))
  }
  if (7 %in% step) {
    cat(paste0(
      "[Step 7] ", (1 - alp) * 100, "% CI Plot for the Mean Differences of ",
      yl, " w.r.t. ", xl
    ), "---------\n")
    n2 <- length(ym2)
    y1 <- min(lcl2) - 0.1 * (max(ucl2) - min(lcl2))
    y2 <- max(ucl2) + 0.05 * (max(ucl2) - min(lcl2))
    width <- 7 + min(8, max(0, m1 - 4))
    win.graph(width, 5)
    plot(ym2,
      type = "n", xlim = c(0.5, n2 + 0.5),
      ylim = c(y1, y2), main = paste0(
        (1 - alp) * 100,
        "% CI for the Mean Differences of ", yl,
        " w.r.t. ", xl
      ), ylab = paste(
        "Mean Differences of",
        yl
      ), xlab = paste(
        "Level Combinations of",
        xl
      ), xaxt = "n"
    )
    grid(col = 3)
    abline(h = 0, lty = 2, col = grey(0.2))
    dcol <- rep("green2", n2)
    dcol[lcl2 > 0 | ucl2 < 0] <- "orange"
    arrows(1:n2, lcl2, 1:n2, ucl2,
      lwd = 2, length = 0.1,
      code = 3, angle = 90, col = dcol
    )
    lines(1:n2, lcl2,
      type = "b", pch = 18, lty = 2,
      col = 4
    )
    lines(1:n2, ucl2,
      type = "b", pch = 18, lty = 2,
      col = 4
    )
    text(1:n2, ym2,
      labels = round(ym2, 2), cex = 0.9, col = 2,
      pos = 4
    )
    text(1:n2, lcl2,
      labels = round(lcl2, 2), cex = 0.9,
      col = 1, pos = 4
    )
    text(1:n2, ucl2,
      labels = round(ucl2, 2), cex = 0.9,
      col = 1, pos = 4
    )
    lines(1:n2, ym2,
      type = "b", pch = 17, lty = 2,
      col = 2, cex = 1.2
    )
    axis(1, at = 1:n2, labels = names(ym2), las = ifelse(m1 >
      5, 2, 1), cex.axis = ifelse(m1 > 4, 0.8, 1))
  }
}
