#' @title Correlation & Simple Regression Analysis
#' @description Correlation Analysis & Simple Linear Regression Analysis
#' @usage corr.reg1(x, y, r0 = 0, xl, yl, mt, step = 1:4, x0, xrng, by, alp = 0.05, dig = 4)
#'
#' @param x Vector of independent variable (explanatory variable) data
#' @param y Vector of dependent variable (response variable) data
#' @param r0 Correlation coefficient value under the null hypothesis, Default: 0
#' @param xl Label of x-axis (default=x variable name)
#' @param yl Label of y-axis (default=y variable name)
#' @param mt Plot title
#' @param step Steps of simple regression analysis, Default: 1:9
#' @param x0 Specipic poit value for confidence and prediction intervals
#' @param xrng Range of x-axis
#' @param by Plotting interval of confidence and prediction bands
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' Scorr.plot6()
#' corr.plot6(r = 0.6, r2 = 0.9, n = 100)
#' @export

corr.reg1 <- function(x, y, r0 = 0, xl, yl, mt, step = 1:4, x0, xrng, by,
                      alp = 0.05, dig = 4) {
  if (missing(xl)) {
    xl <- deparse(substitute(x))
  }
  if (missing(yl)) {
    yl <- deparse(substitute(y))
  }
  if (missing(x0)) {
    x0 <- max(x)
  }
  nn <- length(x)
  lm1 <- lm(y ~ x)
  if (1 %in% step | 6 %in% step) {
    cat("[Step 1] Scatter plot for prior investigation ------------------------\n")
    if (missing(mt)) {
      mt <- paste0(
        "Scatter Plot of ", xl, " vs. ",
        yl
      )
    }
    dev.new(7, 5)
    y1 <- floor(min(y, lm1$fit))
    y2 <- ceiling(max(y, lm1$fit))
    plot(x, y,
      pch = 19, main = mt, xlab = xl, ylab = yl,
      ylim = c(y1, y2)
    )
    grid(col = 3)
  }
  Sxx <- sum(x^2) - sum(x)^2 / nn
  Syy <- sum(y^2) - sum(y)^2 / nn
  Sxy <- sum(x * y) - sum(x) * sum(y) / nn
  rxy <- Sxy / sqrt(Sxx * Syy)
  if (2 %in% step) {
    cat("[Step 2] Estimate correlation coefficients ------------------------\n")
    cat(paste0(
      "Sxx = ", round(sum(x^2), dig), " - (",
      round(sum(x), dig), ")²", "/ ", nn,
      " = ", round(Sxx, dig)
    ), "\n")
    cat(paste0(
      "Syy = ", round(sum(y^2), dig), " - (",
      round(sum(y), dig), ")²", "/ ", nn,
      " = ", round(Syy, dig)
    ), "\n")
    cat(
      paste0(
        "Sxy = ", round(sum(x * y), dig), " - (",
        round(sum(x), dig), " × ", round(sum(y), dig),
        ") / ", nn, " = ", round(Sxy, dig)
      ),
      "\n"
    )
    cat(paste0(
      "Cor(xy) = ", round(Sxy, dig), " / √(",
      round(Sxx, dig), " × ", round(Syy, dig), ") = ",
      round(rxy, dig)
    ), "\n")
  }
  ct <- cor.test(x, y, conf.level = 1 - alp)
  T0 <- rxy * sqrt((nn - 2) / (1 - rxy^2))
  pv0 <- 2 * pt(-abs(T0), nn - 2)
  if (3 %in% step) {
    cat("[Step 3] Correlation tests ------------------------\n")
    cat("Sample Correlation Coefficient =", round(
      ct$est,
      dig
    ), "\n")
    if (r0 == 0) {
      cat(paste0(
        "t-statistic = ", round(rxy, dig),
        " × √(", nn - 2, "/(1-", round(
          rxy,
          dig
        ), "²)) = ", round(ct$stat, dig),
        "\n"
      ))
      cat(paste0(
        "p-value = P(|T", nn - 2, "| > ",
        round(abs(T0), dig), ") = ", round(
          pv0,
          dig
        ), "\n"
      ))
    }
    else {
      Z0 <- sqrt(nn - 3) * 0.5 * (log((1 + rxy) / (1 - rxy)) -
        log((1 + r0) / (1 - r0)))
      pv0 <- 2 * pnorm(-abs(Z0))
      cat(paste0(
        "Z-statistic = √(", nn - 3, ") × 0.5 × (log((1+",
        round(rxy, dig), ")/(1-", round(rxy, dig),
        "))-log((1+", r0, ")/(1-", r0, "))) = ",
        round(Z0, dig), "\n"
      ))
      cat(paste0("p-value = P(|Z| > ", round(
        abs(Z0),
        dig
      ), ") = ", round(pv0, dig), "\n"))
    }
  }
  if (4 %in% step) {
    cat("[Step 4] Confidence intervals for correlation coefficients -------------------\n")
    cat(paste0(
      100 * (1 - alp), "% Confidence Interval = [",
      round(ct$conf[1], dig), ", ", round(
        ct$conf[2],
        dig
      ), "]\n"
    ))
  }
  if (5 %in% step) {
    if (r0 == 0) {
      cat("[Step 5] T-test plot ------------------------\n")
      dev.new(7, 5)
      mr <- max(c(4, abs(ct$stat * 1.2)))
      ttest.plot(ct$stat, ct$para, prng = c(-mr, mr), pvout = F)
    }
    else {
      cat("[Step 5] Z-test plot ------------------------\n")
      dev.new(7, 5)
      mr <- max(c(4, abs(Z0 * 1.2)))
      normtest.plot(Z0,
        prng = c(-mr, mr), xlab = "Z-statistic",
        pvout = F
      )
    }
  }
  if (6 %in% step) {
    cat("[Step 6] Display the simple regression line ------------------------\n")
    dev.new(7, 5)
    plot(x, y,
      pch = 19, main = mt, xlab = xl, ylab = yl,
      ylim = c(y1, y2)
    )
    grid(col = 3)
    abline(lm1, lty = 2, col = 2)
    pos <- ifelse(lm1$coef[[2]] > 0, "bottomright",
      "upright"
    )
    sign <- ifelse(lm1$coef[[2]] > 0, "+", "")
    legend(pos, c("Regression Equation", paste0(
      "Y = ",
      round(lm1$coef[[1]], dig), sign, round(
        lm1$coef[[2]],
        dig
      ), " * X"
    )), text.col = c(1, 4), bg = "white")
    segments(x, y, x, lm1$fit, lty = 2, col = 4)
    text(x, (y + lm1$fit) / 2,
      labels = "e", col = 4,
      pos = 4
    )
  }
  if (any(7:11 %in% step)) {
    b1 <- Sxy / Sxx
    xb <- mean(x)
    yb <- mean(y)
    b0 <- yb - b1 * xb
    sign <- ifelse(b1 > 0, "+", "")
  }
  if (7 %in% step) {
    cat("[Step 7] Estimate simple regression coefficients ------------------\n")
    cat(paste0(
      "Sxx = ", round(sum(x^2), dig), " - (",
      round(sum(x), dig), ")²", "/ ", nn,
      " = ", round(Sxx, dig)
    ), "\n")
    cat(
      paste0(
        "Sxy = ", round(sum(x * y), dig), " - (",
        round(sum(x), dig), " × ", round(sum(y), dig),
        ") / ", nn, " = ", round(Sxy, dig)
      ),
      "\n"
    )
    cat(paste0(
      "Slope = ", round(Sxy, dig), " / ",
      round(Sxx, dig), " = ", round(b1, dig)
    ), "\n")
    cat(paste0(
      "Intersection = ", round(yb, dig), " - ",
      round(b1, dig), " × ", round(xb, dig), " = ",
      round(b0, dig)
    ), "\n")
    cat(paste0("Regression Equation : y = ", round(
      b0,
      dig
    ), " ", sign, " ", round(
      abs(b1),
      dig
    ), " x"), "\n")
  }
  if (any(8:11 %in% step)) {
    SST <- Syy
    SSR <- Sxy^2 / Sxx
    SSE <- SST - SSR
    MSE <- SSE / (nn - 2)
    Rsq <- SSR / SST
    tval <- qt(1 - alp / 2, nn - 2)
    an1 <- anova(lm1)
    sm1 <- summary(lm1)
    conf <- 100 * (1 - alp)
  }
  if (8 %in% step) {
    cat("[Step 8] ANOVA table by calculating sum of squares -------------------\n")
    cat(paste0(
      "SST = ", round(sum(y^2), dig), " - (",
      round(sum(y), dig), ")²", "/ ", nn,
      " = ", round(SST, dig)
    ), "\n")
    cat(paste0(
      "SSR = (", round(Sxy, dig), ")²",
      "/ ", round(Sxx, dig), " = ", round(
        SSR,
        dig
      )
    ), "\n")
    cat(paste0(
      "SSE = ", round(SST, dig), " - ",
      round(SSR, dig), " = ", round(SSE, dig)
    ), "\n")
    pv1 <- an1$Pr[1]
    if (pv1 < 0.001) {
      star <- "***"
    }
    else if (pv1 < 0.01) {
      star <- "**"
    }
    else if (pv1 < 0.05) {
      star <- "*"
    }
    cat("--------------- ANOVA table  ---------------------\n")
    antab <- cbind(an1$Sum, an1$Df, an1$Mean, an1$F, an1$Pr)
    antab <- rbind(antab, c(
      sum(an1$Sum), sum(an1$Df), NA,
      NA, NA
    ))
    rownames(antab) <- c(
      "Regress", "Residual",
      "Total"
    )
    dum <- cbind(round(antab[, 1:4], dig), round(
      antab[, 5],
      dig * 2
    ))
    colnames(dum) <- c(
      "Sum of Sq.", "df", "Mean Sq.",
      "F-value", "p-value"
    )
    dum[is.na(dum)] <- ""
    print(as.data.frame(dum))
    cat(paste0("F-test critical value = ", round(qf(1 -
      alp, 1, nn - 2), dig)), "\n")
    cat(paste0(
      "R-square = ", round(SSR, dig), " / ",
      round(SST, dig), " = ", round(Rsq, dig)
    ), "\n")
  }
  if (9 %in% step) {
    se1 <- sqrt(MSE / Sxx)
    se0 <- sqrt(MSE * (1 / nn + xb^2 / Sxx))
    tol1 <- tval * se1
    tol0 <- tval * se0
    tstat1 <- b1 / se1
    tstat0 <- b0 / se0
    pv1 <- 2 * pt(-abs(tstat1), nn - 2)
    pv0 <- 2 * pt(-abs(tstat0), nn - 2)
    cat("[Step 9] Confidence intervals & significance tests for regression coefficients --------\n")
    cat(paste0(
      conf, "%CI(b1) = [", round(b1, dig),
      " ± ", round(tval, dig), " × ", round(
        se1,
        dig
      ), "] = [", round(b1, dig), " ± ",
      round(tol1, dig), "] = [", round(
        b1 - tol1,
        dig
      ), ", ", round(b1 + tol1, dig), "]\n"
    ))
    cat(paste0(
      conf, "%CI(b0) = [", round(b0, dig),
      " ± ", round(tval, dig), " × ", round(
        se0,
        dig
      ), "] = [", round(b0, dig), " ± ",
      round(tol0, dig), "] = [", round(
        b0 - tol0,
        dig
      ), ", ", round(b0 + tol0, dig), "]\n"
    ))
    cat("    Significance tests for regression coefficient ------------------------\n")
    cat(paste0(
      "T1 = ", round(b1, dig), " / ",
      round(se1, dig), " = ", round(tstat1, dig),
      "  \t P-val = P(|T", nn - 2, "| > ",
      round(abs(tstat1), dig), ") = ", round(
        pv1,
        dig
      )
    ), "\n")
    cat(paste0(
      "T0 = ", round(b0, dig), " / ",
      round(se0, dig), " = ", round(tstat0, dig),
      "\t P-val = P(|T", nn - 2, "| > ", round(
        abs(tstat0),
        dig
      ), ") = ", round(pv0, dig)
    ), "\n")
  }
  if (any(10:11 %in% step)) {
    Ex0 <- b0 + b1 * x0
    vcx0 <- MSE * (1 / nn + (x0 - xb)^2 / Sxx)
    vpx0 <- MSE * (1 + 1 / nn + (x0 - xb)^2 / Sxx)
    cse <- sqrt(vcx0)
    pse <- sqrt(vpx0)
    ctol <- tval * cse
    ptol <- tval * pse
  }
  if (10 %in% step) {
    cat("[Step 10-1] Confidence interval for E[Y|x0] -------------\n")
    cat(paste0(
      conf, "%CI E(Y|", x0, ") = [",
      round(Ex0, dig), " ± ", round(tval, dig),
      " × ", round(cse, dig), "] = [", round(
        Ex0,
        dig
      ), " ± ", round(ctol, dig), "] = [",
      round(Ex0 - ctol, dig), ", ", round(
        Ex0 + ctol,
        dig
      ), "]\n"
    ))
    cat("[Step 10-2] Prediction interval for Y|x0 -------------------\n")
    cat(paste0(
      conf, "%PI (Y|", x0, ") = [",
      round(Ex0, dig), " ± ", round(tval, dig),
      " × ", round(pse, dig), "] = [", round(
        Ex0,
        dig
      ), " ± ", round(ptol, dig), "] = [",
      round(Ex0 - ptol, dig), ", ", round(
        Ex0 + ptol,
        dig
      ), "]\n"
    ))
  }
  if (11 %in% step) {
    cat("[Step 11] Plot confidence bands and prediction bands ------------------------\n")
    x1 <- min(x0, x)
    x2 <- max(x0, x)
    if (missing(xrng)) {
      xrng <- c(x1 - 0.1 * (x2 - x1), x2 + 0.1 * (x2 - x1))
    }
    if (missing(by)) {
      by <- (x2 - x1) / 50
    }
    nd <- data.frame(x = seq(xrng[1], xrng[2], by = by))
    conf2 <- predict(lm1, interval = "confidence", newdata = nd)
    pred2 <- predict(lm1, interval = "prediction", newdata = nd)
    y1 <- min(pred2[, "lwr"])
    y2 <- max(pred2[, "upr"])
    ymin <- y1 - (y2 - y1) * 0.1
    ymax <- y2 + (y2 - y1) * 0.1
    dev.new(7, 6)
    plot(x, y, pch = 19, main = paste(
      "Confidence and Prediction Bands of",
      yl, "given", xl
    ), xlab = xl, ylab = yl, ylim = c(
      ymin,
      ymax
    ), xlim = xrng)
    abline(lm1, col = 4)
    abline(v = xb, lty = 2, col = 3)
    text(xb, ymin, labels = expression(bar(x)), pos = 4)
    matlines(nd$x, conf2[, c("lwr", "upr")],
      col = 2, type = "p", pch = "+"
    )
    matlines(nd$x, pred2[, c("lwr", "upr")],
      col = 4, type = "p", pch = 1
    )
    abline(v = x0, lty = 2, col = "orange")
    text(x0, ymin,
      labels = expression(x[0]), cex = 0.9,
      col = "green4", pos = 4
    )
    text(x0, Ex0 + c(-ptol, 0, ptol),
      labels = format(Ex0 +
        c(-ptol, 0, ptol), digit = dig), cex = 0.8, col = "green4",
      pos = c(1, 1, 3)
    )
  }
  if (12 %in% step) {
    cat("[Step 12] Diagnosis of the regression model ------------------------\n")
    dev.new(7, 5)
    par(mfrow = c(2, 2))
    plot(lm1)
  }
}
