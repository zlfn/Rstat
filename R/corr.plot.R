#' @title Correlation Coefficients and Scatter Plots

#' @description Correlation Coefficients and Scatter Plots of Discrete Random variables


#' @usage corr.plot(X, Mt, item, dig = 4, prt = TRUE, pprt = FALSE, plot = FALSE)

#' @param X Sample space vector of X
#' @param Mt Plot title
#' @param item Names of random variables
#' @param dig Number of effective digits, Default: 5
#' @param prt Print the result? Default: TRUE
#' @param pprt Print frequency tables? Default: FALSE
#' @param plot Plot the PDF and scatter plots? Default: FALSE
#'
#'
#' @return None.
#' @examples
#' S = rolldie2(4)
#' item = c("Sum", "Max", "Min", "Range")
#' X = list()
#' X[[1]] = apply(S, 1, sum)
#' X[[2]] = apply(S, 1, max)
#' X[[3]] = apply(S, 1, min)
#' X[[4]] = X[[2]]-X[[3]]
#' Mt = paste("PDF of", item, "in 4 Dice")
#' corr.plot(X, Mt, item, pprt=T, plot=T)
#' @export
corr.plot <- function (X, Mt, item, dig = 4, prt = TRUE, pprt = FALSE, plot = FALSE)
{
  nv = length(X)
  Xf = list()
  for (k in 1:nv) Xf[[k]] = table(X[[k]])
  if (pprt) {
    for (k in 1:nv) {
      cat(paste0(item[k], "(X", k, ") frequency distribution"))
      print(Xf[[k]])
    }
  }
  Xv = list()
  for (k in 1:nv) Xv[[k]] = as.numeric(names(Xf[[k]]))
  Xp = list()
  N = length(X[[1]])
  for (k in 1:nv) Xp[[k]] = Xf[[k]]/N
  SX = EX = rep(NA, nv)
  for (k in 1:nv) {
    SX[k] = (Xv[[k]] %*% Xf[[k]])[1, 1]
    EX[k] = SX[k]/N
  }
  SX2 = EX2 = VX = DX = rep(NA, nv)
  for (k in 1:nv) {
    SX2[k] = (Xv[[k]]^2 %*% Xf[[k]])[1, 1]
    EX2[k] = SX2[k]/N
    VX[k] = EX2[k] - EX[k]^2
    DX[k] = sqrt(VX[k])
  }
  SXY = EXY = VXY = CXY = matrix(NA, nv, nv)
  colnames(VXY) = colnames(CXY) = rownames(VXY) = rownames(CXY) = paste0("X",
                                                                         1:nv)
  for (k in 1:nv) for (m in 1:nv) {
    XYf = table(X[[k]], X[[m]])
    SXY[k, m] = (as.vector(Xv[[k]] %o% Xv[[m]]) %*% as.vector(XYf))[1,
                                                                    1]
    XYp = XYf/N
    EXY[k, m] = SXY[k, m]/N
    VXY[k, m] = EXY[k, m] - EX[k] * EX[m]
    CXY[k, m] = VXY[k, m]/(VX[k] * VX[m])^0.5
  }
  if (prt) {
    cat("Expected Values and Variances ------------------------\n")
    for (k in 1:nv) cat(paste0("E(X", k, ") ="),
                        format(EX[k], digits = (dig + 1)), "\t")
    cat("\n")
    for (k in 1:nv) cat(paste0("Var(X", k, ") ="),
                        format(VX[k], digits = (dig + 1)), "\t")
    cat("\nVariance-Covariance Matrix ------------------------\n")
    print(VXY)
    cat("Correlation Coefficient Matrix ------------------------\n")
    print(CXY)
  }
  if (plot) {
    if (missing(Mt))
      Mt = paste(item, "probability distribution")
    nc = ifelse(nv <= 5, 2, 3)
    nr = ceiling(nv/nc)
    h = ifelse(nr > 2, 9, 6)
    w = ifelse(nc > 2, 9, 7)
    win.graph(w, h)
    par(mfrow = c(nr, nc))
    for (k in 1:nv) plot(Xp[[k]], type = "h", col = "red",
                         main = Mt[k], ylab = "f(x)", xlab = "",
                         lwd = 3)
    St = matrix("character", nv, nv)
    for (k in 1:nv) for (m in 1:nv) St[k, m] = paste(item[m],
                                                     ":", item[k])
    np = nv * (nv - 1)/2
    nc = ifelse(np <= 5, 2, 3)
    nr = ceiling(np/nc)
    h = ifelse(nr > 2, 9, 6)
    w = ifelse(nc > 2, 9, 7)
    win.graph(w, h)
    par(mfrow = c(nr, nc))
    for (k in 1:(nv - 1)) for (m in (k + 1):nv) {
      plot(X[[m]], X[[k]], pch = 19, col = 4, main = St[k,
                                                        m], xlab = item[m], ylab = item[k])
      abline(lm(X[[k]] ~ X[[m]]), col = 2)
    }
  }
}
