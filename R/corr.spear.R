#' @title Pearson & Spearman Correlation Coefficient
#' @description Pearson Correlation Coefficient & Spearman Correlation Coefficient
#' @usage corr.spear(x, y, r0 = 0, xl, yl, mt, step = 1:2, alp = 0.05, dig = 4)
#'
#' @param x Vector of x-data
#' @param y Vector of y-data
#' @param r0 Correlation coefficient value under the null hypothesis, Default: 0
#' @param xl Name of x-data
#' @param yl Name of y-data
#' @param mt Title of scatter plot
#' @param step Steps of the analysis, Default: 1:2
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#' @return None.
#' @examples
#' x <- c(10, 7, 0, 1, 5, 2, 8, 6, 4, 9, 3, 0, 2, 4, 6, 8)
#' y <- c(75, 77, 91, 64, 79, 81, 90, 86, 82, 76, 89, 93, 80, 87, 83, 78)
#' corr.spear(x, y, r0 = 0, xl = "Play", yl = "Score", step = 1:3)
#' @export

corr.spear <- function(x, y, r0 = 0, xl, yl, mt, step = 1:2, alp = 0.05, dig = 4) {
  if (missing(xl)) {
    xl <- deparse(substitute(x))
  }
  if (missing(yl)) {
    yl <- deparse(substitute(y))
  }
  nn <- length(x)
  lm1 <- lm(y ~ x)
  Sxx <- sum(x^2) - sum(x)^2 / nn
  Syy <- sum(y^2) - sum(y)^2 / nn
  Sxy <- sum(x * y) - sum(x) * sum(y) / nn
  rxy <- Sxy / sqrt(Sxx * Syy)
  if (1 %in% step) {
    cat("[Step 1-1] Pearson Correlation Coefficient ------------------------\n")
    cat(paste0(
      "Sxx = ", round(sum(x^2), dig), " - ",
      round(sum(x), dig), "² / ", nn, " = ",
      round(Sxx, dig)
    ), "\n")
    cat(paste0(
      "Syy = ", round(sum(y^2), dig), " - ",
      round(sum(y), dig), "² / ", nn, " = ",
      round(Syy, dig)
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
      "Corr(x,y) = ", round(Sxy, dig), " / √(",
      round(Sxx, dig), " × ", round(Syy, dig), ") = ",
      round(rxy, dig)
    ), "\n")
    ct <- cor.test(x, y, conf.level = 1 - alp)
    T0 <- rxy * sqrt((nn - 2) / (1 - rxy^2))
    pv0 <- 2 * pt(-abs(T0), nn - 2)
    cat("[Step 1-2] Pearson Correlation Test ------------------------\n")
    if (r0 == 0) {
      cat(paste0(
        "T-stat = ", round(rxy, dig), " × √(",
        nn - 2, " / (1 - ", round(abs(rxy), dig),
        "²)) = ", round(ct$stat, dig), "\n"
      ))
      cat(paste0(
        "P-value = P(|T", nn - 2, "| > ",
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
        ")) - log((1+", r0, ")/(1-", r0,
        "))) = ", round(Z0, dig), "\n"
      ))
      cat(paste0("P-value = P(|Z| > ", round(
        abs(Z0),
        dig
      ), ") = ", round(pv0, dig), "\n"))
    }
    cat(paste0(
      100 * (1 - alp), "% Confidence Interval = [",
      round(ct$conf[1], dig), ", ", round(
        ct$conf[2],
        dig
      ), "]\n"
    ))
  }
  x2 <- rank(x)
  y2 <- rank(y)
  lm2 <- lm(y2 ~ x2)
  Sxx2 <- sum(x2^2) - sum(x2)^2 / nn
  Syy2 <- sum(y2^2) - sum(y2)^2 / nn
  Sxy2 <- sum(x2 * y2) - sum(x2) * sum(y2) / nn
  rxy2 <- Sxy2 / sqrt(Sxx2 * Syy2)
  if (2 %in% step) {
    cat("[Step 2-1] Spearman correlation coefficient ------------------------\n")
    cat(paste0(
      "Srx.x = ", round(sum(x2^2), dig), " - ",
      round(sum(x2), dig), "² / ", nn, " = ",
      round(Sxx2, dig)
    ), "\n")
    cat(paste0(
      "Sry.y = ", round(sum(y2^2), dig), " - ",
      round(sum(y2), dig), "² / ", nn, " = ",
      round(Syy2, dig)
    ), "\n")
    cat(paste0(
      "Srx.y = ", round(sum(x2 * y2), dig),
      " - (", round(sum(x2), dig), " × ",
      round(sum(y2), dig), ") / ", nn, " = ",
      round(Sxy2, dig)
    ), "\n")
    cat(
      paste0(
        "Corr(rx,ry) = ", round(Sxy2, dig),
        " / √(", round(Sxx2, dig), " × ",
        round(Syy2, dig), ") = ", round(rxy2, dig)
      ),
      "\n"
    )
    ct2 <- cor.test(x2, y2, conf.level = 1 - alp)
    T02 <- rxy2 * sqrt((nn - 2) / (1 - rxy2^2))
    pv02 <- 2 * pt(-abs(T02), nn - 2)
    cat("[Step 2-2] Spearman correlation test ------------------------\n")
    if (r0 == 0) {
      cat(paste0(
        "T-stat = ", round(rxy2, dig), " × √(",
        nn - 2, " / (1 - ", round(abs(rxy2), dig),
        "²)) = ", round(ct2$stat, dig), "\n"
      ))
      cat(paste0(
        "P-value = P(|T", nn - 2, "| > ",
        round(abs(T02), dig), ") = ", round(
          pv02,
          dig
        ), "\n"
      ))
    }
    else {
      Z02 <- sqrt(nn - 3) * 0.5 * (log((1 + rxy2) / (1 - rxy2)) -
        log((1 + r0) / (1 - r0)))
      pv02 <- 2 * pnorm(-abs(Z02))
      cat(paste0(
        "Z-statistic = √(", nn - 3, ") × 0.5 × (log((1+",
        round(rxy2, dig), ")/(1-", round(
          rxy2,
          dig
        ), ")) - log((1+", r0, ")/(1-",
        r0, "))) = ", round(Z02, dig), "\n"
      ))
      cat(paste0("p-value = P(|Z| > ", round(
        abs(Z02),
        dig
      ), ") = ", round(pv02, dig), "\n"))
    }
    cat(paste0(
      100 * (1 - alp), "% Confidence Interval = [",
      round(ct2$conf[1], dig), ", ", round(
        ct2$conf[2],
        dig
      ), "]\n"
    ))
  }
  if (3 %in% step) {
    cat("[Step 3] Scatter plot ------------------------\n")
    if (missing(mt)) {
      mt <- paste(
        "Scatter Plot of", yl, "vs.",
        xl
      )
    }
    win.graph(8, 4)
    par(mfrow = c(1, 2))
    y11 <- floor(min(y, lm1$fit))
    y12 <- ceiling(max(y, lm1$fit))
    plot(x, y,
      pch = 19, main = mt, xlab = xl, ylab = yl,
      ylim = c(y11, y12)
    )
    grid(col = 3)
    abline(lm1, col = 2)
    mt2 <- paste0(
      "Scatter Plot of r(", yl, ") vs. r(",
      xl, ")"
    )
    y21 <- floor(min(y2, lm2$fit))
    y22 <- ceiling(max(y2, lm2$fit))
    plot(x2, y2,
      pch = 19, main = mt2, xlab = paste0(
        "r(",
        xl, ")"
      ), ylab = paste0("r(", yl, ")"),
      ylim = c(y21, y22)
    )
    grid(col = 3)
    abline(lm2, col = 2)
  }
}
