#' @title Quantile Plot of the Chi-square Distribution
#' @description Quantile Plot of the Chi-square Distribution
#' @usage chi.quant(nu, pv, pv2 = pv, pup = 0.999, mt, dig = 4)
#' @param nu Degree of freedom of the chi-square distribution
#' @param pv Vector of probabilities for quantiles
#' @param pv2 Vector of probabilities for specific quantiles, Default: pv
#' @param pup Upper limit of probabilities for quantiles, Default: 0.999
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#'
#' @return None.
#' @examples
#' nu <- 5
#' pv <- c(0.005, 0.01, 0.025, 0.05, 1:9 / 10, 0.95, 0.975, 0.99, 0.995)
#' pv2 <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' chi.quant(nu, pv, pv2)
#' @export

chi.quant <- function(nu, pv, pv2 = pv, pup = 0.999, mt, dig = 4) {
  if (missing(mt)) {
    mt <- bquote("Quantiles of the Chi-square Distribution" ~
    chi[p ~ "," ~ .(nu)]^2)
  }
  cv <- qchisq(pv, nu)
  names(cv) <- pv
  print(round(cv, dig))
  up <- qchisq(pup, nu)
  x <- seq(0, up, length = 100)
  pdf <- dchisq(x, nu)
  ymax <- max(pdf)
  npv <- length(pv2)
  y1 <- 0.1 * npv * ymax
  wc <- ifelse(npv > 5, 6, 5)
  dev.new(7, wc)
  plot(x, pdf, type = "n", ylim = c(-y1, ymax), xlim = c(-0.1 *
    up, up), ylab = "f(x)", xlab = "x", main = mt)
  abline(h = 0, col = "green4")
  lines(x, pdf, type = "l", lwd = 2, col = 2)
  cv2 <- qchisq(pv2, nu)
  abline(v = c(0, nu), col = 3)
  abline(v = cv2, lty = 2, col = 4)
  yp <- -0.1 * ymax * (1:npv)
  arrows(0, yp, cv2, yp, length = 0.1, code = 2, col = 4)
  text(0, yp, paste0("p=", pv2), pos = 2, col = 4, cex = 0.8)
  text(cv2, yp, round(cv2, dig), pos = 4, cex = 0.8)
}
