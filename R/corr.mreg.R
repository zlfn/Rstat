#' @title Multiple Regression Analysis
#' @description Multiple Regression Analysis
#' @usage corr.mreg(xd, y, form, xd2, form2, step = 0:4, newd, pvx, xrng, nx, alp = 0.05, dig = 4)
#' @param xd Data frame of independent variables (explanatory variables)
#' @param y Vector of dependent variable (response variable) data
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param xd2 Data frame of independent variables in model2 (step=4, 5)
#' @param form2 Formula of regression model2 (ex: y ~ x1 * x2) (step=4, 5)
#' @param step Steps of multiple regression analysis, Default: 0:4
#' @param newd Data frame of independent variables for step 6
#' @param pvx Designated number of independent variables for step 6(step=7)
#' @param xrng Range of independent variables for step 7
#' @param nx Designated number of independent variables for step 7
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#'
#'
#'
#' @return None.
#' @examples
#' mpg = mtcars$mpg
#' xd = mtcars[4:6]
#' attach(xd)
#' form = mpg ~ hp + drat + wt
#' corr.mreg(xd, mpg, form, step=0:3)
#'
#' form2 = mpg ~ hp * drat + wt
#' xd2 = data.frame(hp, drat, wt, hpd= hp * drat)
#' corr.mreg(xd, mpg, form, xd2, form2, step=4:5)
#' @export
corr.mreg <- function (xd, y, form, xd2, form2, step = 0:4, newd, pvx, xrng,
                       nx, alp = 0.05, dig = 4)
{
  ke = length(xd)
  kk = ke + 1
  xl = names(xd)
  yl = deparse(substitute(y))
  model1 = deparse(substitute(form))
  if (!missing(form2))
    model2 = deparse(substitute(form2))
  nn = length(y)
  if (0 %in% step) {
    cat("[Step 0] Scatter matrix plot for prior investigation of data --------------------\n")
    win.graph(7, 5)
    xyd = as.data.frame(cbind(xd, y))
    names(xyd) = c(xl, yl)
    pairs(xyd, lower.panel = panel.cor, upper.panel = function(x,
                                                               y) {
      points(x, y)
      abline(lm(y ~ x), col = "red")
    })
  }
  conf = 100 * (1 - alp)
  lm1 = lm(form)
  an1 = anova(lm1)
  sm1 = summary(lm1)
  X = as.matrix(xd)
  const = rep(1, nn)
  X = cbind(const, X)
  XX = t(X) %*% X
  Xy = t(X) %*% y
  colnames(Xy) = yl
  XXI = solve(XX)
  bh = XXI %*% Xy
  if (1 %in% step) {
    cat("[Step 1] Estimate multiple regression coefficients ------------------------\n")
    cat("X'X matrix ----------\n")
    print(XX)
    cat("Equation : b = inv(X'X) (X'y) ----------\n")
    for (k in 1:kk) cat(paste("b", k - 1), "\t",
                        round(XXI[k, ], 6), "\t", Xy[k], "\t",
                        round(bh[k], dig), "\n")
  }
  CT = sum(y)^2/nn
  SST = sum(y^2) - CT
  SSR = t(bh) %*% Xy - CT
  SSE = SST - SSR
  dfe = nn - kk
  MSR = SSR/ke
  MSE = SSE/dfe
  F0 = MSR/MSE
  pv0 = 1 - pf(F0, ke, dfe)
  Rsq = SSR/SST
  aRsq = 1 - SSE/SST * (nn - 1)/(nn - kk)
  tval = qt(1 - alp/2, dfe)
  if (2 %in% step) {
    cat("[Step 2] ANOVA table by calculating sum of squares ----------------------\n")
    cat(paste0("SST = ", round(sum(y^2), dig), " - (",
               round(sum(y), dig), ")²", "/ ", nn,
               " = ", round(SST, dig)), "\n")
    cat(paste0("SSR = (", paste(round(bh, dig), collapse = " "),
               ").(", paste(round(Xy, dig), collapse = " "),
               ") = ", round(SSR, dig)), "\n")
    cat(paste0("SSE = ", round(SST, dig), " - ",
               round(SSR, dig), " = ", round(SSE, dig)), "\n")
    cat("------ Analysis of Variance Table------------------------\n")
    antab = cbind(an1$Sum[1:ke], an1$Df[1:ke], an1$Mean[1:ke],
                  an1$F[1:ke], an1$Pr[1:ke])
    antab = rbind(antab, c(SSR, ke, MSR, F0, pv0))
    antab = rbind(antab, c(an1$Sum[kk], an1$Df[kk], an1$Mean[kk],
                           an1$F[kk], an1$Pr[kk]))
    antab = rbind(antab, c(sum(an1$Sum), sum(an1$Df), NA,
                           NA, NA))
    rownames(antab) = c(xl, "Regression", "Residual",
                        "Total")
    dum = cbind(round(antab[, 1:4], dig), round(antab[, 5],
                                                dig * 2))
    colnames(dum) = c("Sum of Sq.", "df", "Mean Sq.",
                      "F-value", "p-value")
    dum[is.na(dum)] = ""
    print(as.data.frame(dum))
    cat(paste0("F-test critical value = ", round(qf(1 -
                                                      alp, ke, dfe), dig)), "\n")
    cat(paste0("R-square = ", round(SSR, dig), " / ",
               round(SST, dig), " = ", round(Rsq, dig)), "\n")
    cat(paste0("Adj R-sq = 1 - (", round(SSE, dig),
               " / ", nn - kk, ") / (", round(SST, dig),
               " / ", nn - 1, ") = ", round(aRsq, dig)),
        "\n")
  }
  if (3 %in% step) {
    dXXI = diag(XXI)
    se = sqrt(MSE[1, 1] * dXXI)
    tstat = bh/se
    tol = tval * se
    lcl = bh - tol
    ucl = bh + tol
    pv = 2 * pt(-abs(tstat), dfe)
    cat("[Step 3-1] Confidence intervals for regression coefficients --------------------\n")
    citab = cbind(bh, tol, lcl, ucl)
    colnames(citab) = c("Estimate", "Tolerance",
                        "Lower limit", "Upper limit")
    print(round(citab, dig))
    cat("[Step 3-2] Significance tests for regression coefficients ------------------------\n")
    tstab = cbind(bh, se, tstat, pv)
    dum = cbind(round(tstab[, 1:3], dig), round(tstab[, 4],
                                                2 * dig))
    colnames(dum) = c("Estimate", "Std Error",
                      "T-stat", "P-value")
    print(dum)
  }
  if (any(4:5 %in% step)) {
    ke2 = length(xd2)
    kk2 = ke2 + 1
    xl2 = names(xd2)
    lm2 = lm(form2)
    an2 = anova(lm2)
    sm2 = summary(lm2)
  }
  if (4 %in% step) {
    cat("[Step 4] Analysis of model 2 (redo step 0~3) ------------------------\n")
    cat("[Step 4-0] Scatter plot matrix for model 2 -------------------\n")
    xl2 = names(xd2)
    xd2y = as.data.frame(cbind(xd2, y))
    names(xd2y) = c(xl2, yl)
    win.graph(7, 5)
    pairs(xd2y, lower.panel = panel.cor, upper.panel = function(x,
                                                                y) {
      points(x, y)
      abline(lm(y ~ x), col = "red")
    })
    cat("[Step 4-1] Regression equation for model 2 ------------------------\n")
    X2 = as.matrix(xd2)
    const = rep(1, nn)
    X2 = cbind(const, X2)
    XX2 = t(X2) %*% X2
    cat("X2'X2 matrix ----------\n")
    print(XX2)
    X2y = t(X2) %*% y
    colnames(X2y) = yl
    XX2I = solve(XX2)
    bh2 = XX2I %*% X2y
    cat("Equation : b = inv(X2'X2) (X2'y) ------------\n")
    for (k in 1:kk2) cat(paste0("b", k - 1), "\t ",
                         round(XX2I[k, ], 6), "\t ", X2y[k], "\t ",
                         round(bh2[k], dig), "\n")
    SSR2 = t(bh2) %*% X2y - CT
    SSE2 = SST - SSR2
    dfe2 = nn - kk2
    MSR2 = SSR2/ke2
    MSE2 = SSE2/dfe2
    F0 = MSR2/MSE2
    pv0 = 1 - pf(F0, ke2, dfe2)
    Rsq2 = SSR2/SST
    aRsq2 = 1 - SSE2/SST * (nn - 1)/(nn - kk2)
    tval2 = qt(1 - alp/2, dfe2)
    cat("[Step 4-2] ANOVA table of model 2 ------------------------\n")
    cat(paste0("SST = ", round(sum(y^2), dig), "-(",
               round(sum(y), dig), ")² /", nn, " = ",
               round(SST, dig)), "\n")
    cat("SSR = (", round(bh2, dig), ").(", round(X2y,
                                                 dig), ") = ", round(SSR2, dig), "\n")
    cat(paste0("SSE = ", round(SST, dig), " - ",
               round(SSR2, dig), " = ", round(SSE2, dig)),
        "\n")
    cat("------- ANOVA table ------------------------\n")
    antab2 = cbind(an2$Sum[1:ke2], an2$Df[1:ke2], an2$Mean[1:ke2],
                   an2$F[1:ke2], an2$Pr[1:ke2])
    antab2 = rbind(antab2, c(SSR2, ke2, MSR2, F0, pv0))
    antab2 = rbind(antab2, c(an2$Sum[kk2], an2$Df[kk2], an2$Mean[kk2],
                             an2$F[kk2], an2$Pr[kk2]))
    antab2 = rbind(antab2, c(sum(an2$Sum), sum(an2$Df), NA,
                             NA, NA))
    colnames(antab2) = c("Sum of Sq.", "df",
                         "Mean Sq.", "F-value", "p-value")
    rownames(antab2) = c(xl2, "Regression", "Residual",
                         "Total")
    dum = round(antab2, dig)
    dum[is.na(dum)] = ""
    print(as.data.frame(dum))
    cat(paste0("F-test critical value = ", round(qf(1 -
                                                      alp, ke2, dfe2), dig)), "\n")
    cat(paste0("R-square = ", round(SSR2, dig), "/",
               round(SST, dig), " = ", round(Rsq2, dig)),
        "\n")
    cat(paste0("Adj R-sq = 1-(", round(SSE2, dig),
               "/", nn - kk2, ")/(", round(SST, dig),
               "/", nn - 1, ") = ", round(aRsq2, dig)),
        "\n")
    dXX2I = diag(XX2I)
    se2 = sqrt(MSE2[1, 1] * dXX2I)
    tstat2 = bh2/se2
    tol2 = tval2 * se2
    lcl2 = bh2 - tol2
    ucl2 = bh2 + tol2
    pv2 = 2 * pt(-abs(tstat2), dfe2)
    cat("[Step 4-3-1] Confidence intervals for regression coefficients --------\n")
    citab2 = cbind(bh2, tol2, lcl2, ucl2)
    colnames(citab2) = c("Estimate", "Tolerance",
                         "Lower limit", "Upper limit")
    print(round(citab2, dig))
    cat("[Step 4-3-2] Significance tests for regression coefficients-----------------\n")
    tstab2 = cbind(bh2, se2, tstat2, pv2)
    colnames(tstab2) = c("Estimate", "Std Error",
                         "T-stat", "P-value")
    print(round(tstab2, dig))
  }
  if (5 %in% step) {
    cat("[Step 5-1] Compare regression models (analysis of variance) -------------\n")
    an12 = anova(lm1, lm2)
    print(an12)
    cat("[Step 5-2] Diagnose regression model 1 -------------\n")
    win.graph(7, 6)
    par(mfrow = c(2, 2))
    plot(lm1)
    cat("[Step 5-3] Diagnose regression model 2  -------------\n")
    win.graph(7, 6)
    par(mfrow = c(2, 2))
    plot(lm2)
  }
  if (6 %in% step) {
    cat("[Step 6] Confidence intervals and prediction intervals at x=newd --------------\n")
    print(newd)
    cat(paste0(conf, "% Confidence intervals --------------\n"))
    print(round(predict(lm1, newd, interval = "confidence"),
                dig))
    cat(paste0(conf, "% Prediction intervals --------------\n"))
    print(round(predict(lm1, newd, interval = "prediction"),
                dig))
  }
  if (7 %in% step) {
    cat("[Step 7] Plot confidence bands and prediction bands --------------\n")
    xb = apply(xd, 2, mean)
    if (missing(xrng)) {
      xmin = min(xd[[pvx]])
      xmax = max(xd[[pvx]])
      xspan = xmax - xmin
      xrng = c(xmin, xmax) + xspan * c(-0.1, 0.1)
    }
    avx = setdiff(1:ke, pvx)
    ndat = as.data.frame(matrix(NA, nx, ke))
    ndat[[pvx]] = seq(xrng[1], xrng[2], length = nx)
    for (k in avx) ndat[[avx]] = rep(xb[k], nx)
    names(ndat) = xl
    conf1 = predict(lm1, interval = "confidence", newdata = ndat)
    pred1 = predict(lm1, interval = "prediction", newdata = ndat)
    y1 = min(pred1[, "lwr"])
    y2 = max(pred1[, "upr"])
    ymin = y1 - (y2 - y1) * 0.1
    ymax = y2 + (y2 - y1) * 0.1
    win.graph(7, 6)
    plot(xd[[pvx]], y, pch = 19, cex = 1.2, main = paste("Confidence and Prediction Bands of ",
                                                         yl, "given", xl[pvx]), xlab = xl[pvx], ylab = yl,
         xlim = xrng, ylim = c(ymin, ymax))
    lines(ndat[[pvx]], conf1[, 1], lty = 2, col = "purple")
    abline(v = xb[pvx], lty = 2, col = 3)
    text(xb[pvx], ymin, labels = expression(bar(x)), pos = 4)
    matlines(ndat[[pvx]], conf1[, c("lwr", "upr")],
             col = 2, lty = 1, type = "b", pch = "+")
    matlines(ndat[[pvx]], pred1[, c("lwr", "upr")],
             col = 4, lty = 2, type = "b", pch = 1)
    text(xrng[2], conf1[nx, ], labels = format(conf1[nx,
                                                     ], digit = 4), pos = c(1, 1, 3))
  }
}
