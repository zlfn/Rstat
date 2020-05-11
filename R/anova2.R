#' @title Two-way analysis of variance
#' @description Two-way analysis of variance
#' @usage anova2(y, f1, f2, xl1, xl2, yl, step = 0:7, alp = 0.05, inter = TRUE, maxim = TRUE, nb = 4, dig = 4)
#' @param y Response variable data
#' @param f1 Levels of factor1 (same length as y)
#' @param f2 Levels of factor2 (same length as y)
#' @param xl1 Label of x-axis (default=name of f1)
#' @param xl2 Legend label (default=name of f2)
#' @param yl Label of y-axis (default=name of y)
#' @param step Steps of the analysis of variance, Default: 0:7
#' @param alp Level of significance, Default: 0.05
#' @param inter Logical value for including interaction, Default: TRUE
#' @param maxim Logical value for maximization problem, Default: TRUE
#' @param nb Number of best level to be compared, Default: 4
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' Yield <- c(76, 79, 81, 79, 83, 85, 79, 81, 84, 86, 89, 88, 87, 91, 91, 94, 88, 86, 79, 82, 85, 84, 77, 76)
#' f1 <- c(rep(100, 6), rep(150, 6), rep(200, 6), rep(250, 6))
#' f2 <- rep(c(1, 1, 2, 2, 3, 3), 4)
#' Temp <- as.factor(f1)
#' Press <- as.factor(paste0(f2, "Psig"))
#' anova2(Yield, Temp, Press)
#' #' @export
anova2 <- function(y, f1, f2, xl1, xl2, yl, step = 0:7, alp = 0.05, inter = TRUE,
                   maxim = TRUE, nb = 4, dig = 4) {
  if (missing(yl)) {
    yl <- deparse(substitute(y))
  }
  if (missing(xl1)) {
    xl1 <- deparse(substitute(f1))
  }
  if (missing(xl2)) {
    xl2 <- deparse(substitute(f2))
  }
  nn <- length(y)
  m1 <- length(unique(f1))
  m2 <- length(unique(f2))
  nr <- nn / (m1 * m2)
  if (m2 < 6) {
    dcol <- c(1, 2, 4, "green4", "purple")
  }
  else {
    dcol <- rainbow(m2)
  }
  if (is.factor(f1)) {
    af1 <- f1
  }
  else {
    af1 <- as.factor(f1)
  }
  if (is.factor(f2)) {
    af2 <- f2
  }
  else {
    af2 <- as.factor(f2)
  }
  ym1 <- tapply(y, af1, mean)
  ym2 <- tapply(y, af2, mean)
  ym <- tapply(y, list(af2, af1), mean)
  if (0 %in% step) {
    cat(paste(
      "[Step 0] Mean of", yl, "for each combination of",
      xl1, "&", xl2
    ), "---------\n")
    ymtab <- addmargins(ym, FUN = mean, quiet = TRUE)
    print(round(ymtab, dig))
  }
  if (1 %in% step) {
    cat(paste(
      "[Step 1] Interaction Plot of", yl, "for each combination of",
      xl1, "&", xl2
    ), "---------\n")
    win.graph(7, 5)
    interaction.plot(af1, af2, y,
      type = "b", col = dcol[1:m2],
      lwd = 2, leg.bg = "white", leg.bty = "o",
      main = paste(
        "Interaction Plot of", yl, "w.r.t.",
        xl1, "&", xl2
      ), xlab = xl1, ylab = paste0(
        yl,
        " mean"
      ), trace.label = xl2
    )
    grid(col = 3)
  }
  if (inter) {
    an2 <- aov(y ~ af1 * af2)
    nrtab <- 5
    rname <- c(
      xl1, xl2, paste(xl1, xl2, sep = "*"),
      "Error", "Total"
    )
  }
  else {
    an2 <- aov(y ~ af1 + af2)
    nrtab <- 4
    rname <- c(xl1, xl2, "Error", "Total")
  }
  ans2 <- summary(an2)[[1]]
  if (2 %in% step) {
    SSt <- sum(y^2) - sum(y)^2 / nn
    antab2 <- matrix(NA, nrtab, 5)
    colnames(antab2) <- c(
      "Sum of Sq.", "df",
      "Mean Sq.", "F0", "P-value"
    )
    rownames(antab2) <- rname
    antab2[1:(nrtab - 1), ] <- cbind(
      ans2$Sum, ans2$Df, ans2$Mean,
      ans2$F, ans2$Pr
    )
    antab2[nrtab, 1] <- SSt
    antab2[nrtab, 2] <- nn - 1
    dum <- round(antab2, dig)
    dum[is.na(dum)] <- ""
    cat(paste(
      "[Step 2] ANOVA Table of", yl, "w.r.t.",
      xl1, "&", xl2
    ), "---------\n")
    print(as.data.frame(dum))
  }
  if (3 %in% step) {
    cat(paste0(
      "[Step 3] ANOVA Diagnostic plot of ",
      yl, " w.r.t. ", xl1, " & ", xl2
    ), "---------\n")
    win.graph(7, 4)
    par(mfrow = c(1, 2))
    plot(an2, which = 1:2)
  }
  if (inter) {
    ye <- ym
    mse <- summary(an2)[[1]]$Mean[4]
    se <- sqrt(mse / nr)
    dfe <- m1 * m2 * (nr - 1)
    if (maxim) {
      yes <- rev(sort(ye))
    }
    else {
      yes <- sort(ye)
    }
    yopt <- yes[1]
  }
  else {
    ye <- outer(ym2, ym1, "+") - mean(y)
    mse <- summary(an2)[[1]]$Mean[3]
    se <- sqrt(mse * (1 / m1 + 1 / m2 - 1 / (m1 * m2)))
    dfe <- (m1 - 1) * (m2 - 1)
    if (maxim) {
      yes <- rev(sort(ye))
    }
    else {
      yes <- sort(ye)
    }
    yopt <- yes[1]
  }
  tol <- qt(1 - alp / 2, dfe) * se
  lcl <- ye - tol
  ucl <- ye + tol
  lmax <- yopt - tol
  umax <- yopt + tol
  if (4 %in% step) {
    cat(paste(
      "[Step 4] CI for the Best Mean of", yl,
      "w.r.t.", xl1, "&", xl2
    ), "---------\n")
    cat("MSE =", round(mse, dig), "\n")
    cat(paste0(
      "[", round(yopt, dig), " ± ",
      round(tol, dig), "] = [", round(lmax, dig),
      ", ", round(umax, dig), "]\n"
    ))
    cat(paste(
      "Means of", yl, "w.r.t.", xl1,
      "&", xl2
    ), "---------\n")
    print(round(ye, dig))
    cat(paste(
      "Lower limits for the Mean of", yl, "w.r.t.",
      xl1, "&", xl2
    ), "---------\n")
    print(round(lcl, dig))
    cat(paste(
      "Upper limits for the Mean of", yl, "w.r.t.",
      xl1, "&", xl2
    ), "---------\n")
    print(round(ucl, dig))
  }
  if (5 %in% step) {
    cat(paste0(
      "[Step 5] ", (1 - alp) * 100, "% CI for the Mean of ",
      yl, " w.r.t. ", xl1, " & ", xl2
    ), "---------\n")
    win.graph(7, 5)
    lev <- 1:m1
    x1 <- 0.5
    x2 <- m1 + 0.5
    ymin <- min(y, lcl)
    ymax <- max(y, ucl)
    y1 <- ymin - (ymax - ymin) * 0.1
    y2 <- ymax + (ymax - ymin) * 0.1
    plot(unique(af1), rep(-10000, m1),
      type = "n",
      main = paste0(
        (1 - alp) * 100, "% CI for the Mean of ",
        yl, " w.r.t. ", xl1, " & ", xl2
      ),
      ylab = yl, xlab = xl1, xlim = c(x1, x2), ylim = c(
        y1,
        y2
      )
    )
    grid(col = 3)
    del <- (lev[m1] - lev[1]) * 0.01
    dv <- del * (-(m2 - 1) / 2):((m2 - 1) / 2)
    for (k in 1:m2) {
      lines(lev + dv[k], ye[k, ],
        type = "b",
        lty = 2, pch = 17, col = dcol[k], cex = 1.2
      )
    }
    for (k in 1:m2) {
      arrows(lev + dv[k], lcl[k, ], lev + dv[k],
        ucl[k, ],
        col = dcol[k], lwd = 2, length = 0.05,
        code = 3, angle = 90
      )
    }
    legend("topright", as.character(unique(af2)),
      lwd = 2,
      text.col = dcol[1:m2], col = dcol[1:m2]
    )
  }
  if (6 %in% step | 7 %in% step) {
    mm1 <- mm2 <- rep(NA, nb)
    for (k in 1:nb) mm1[k] <- ceiling(which(ye == yes[k]) / m2)
    for (k in 1:nb) mm2[k] <- which(ye == yes[k]) %% m2
    mm2[mm2 == 0] <- m2
    yeb <- yes[1:nb]
    ye2 <- yeb[1] - yeb[-1]
    for (k in 2:(m1 - 1)) ye2 <- c(ye2, yeb[k] - yeb[-(1:k)])
    ylv <- paste0(mm1, mm2)
    nylv <- paste0(ylv[1], "-", ylv[-1])
    for (k in 2:(m1 - 1)) {
      nylv <- c(nylv, paste0(
        ylv[k], "-",
        ylv[-(1:k)]
      ))
    }
    names(ye2) <- nylv
    if (inter) {
      env <- sqrt(2 / nr)
    }
    else {
      env <- sqrt(2 * (1 / m1 + 1 / m2 - 1 / (m1 * m2)))
    }
    tol2 <- qt(1 - alp / 2, dfe) * sqrt(mse) * env
    lcl2 <- ye2 - tol2
    ucl2 <- ye2 + tol2
    cat(
      paste(
        "[Step 6] Mean Differences of", yl, "for best",
        nb, "Combinations of", xl1, "&", xl2
      ),
      "---------\n"
    )
    print(round(rbind(ye2, tol2, lcl2, ucl2), dig))
  }
  if (7 %in% step) {
    cat(paste0(
      "[Step 7] ", (1 - alp) * 100, "% CI for the Mean Differences of ",
      yl, " w.r.t. ", xl1, " & ", xl2
    ), "---------\n")
    n2 <- length(ye2)
    y1 <- min(lcl2) - 0.1 * (max(ucl2) - min(lcl2))
    y2 <- max(ucl2) + 0.05 * (max(ucl2) - min(lcl2))
    win.graph(7, 5)
    plot(ye2,
      type = "n", xlim = c(0.5, n2 + 0.5),
      ylim = c(y1, y2), main = paste0(
        (1 - alp) * 100,
        "% CI for the Mean Differences of ", yl,
        " w.r.t. ", xl1, " & ", xl2
      ), ylab = paste(
        "Mean Differences of",
        yl
      ), xlab = paste(
        "Best", nb, "Combinations of",
        xl1, "&", xl2
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
    text(1:n2, ye2,
      labels = round(ye2, 2), cex = 0.9, col = 2,
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
    lines(1:n2, ye2,
      type = "b", pch = 17, lty = 2,
      col = 2, cex = 1.2
    )
    mtext(names(ye2), side = 1, at = 1:n2, col = 1, line = 0.5)
  }
}
