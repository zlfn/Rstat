#' @title Estimation of Location


#' @description To calculate location estimates.



#' @usage location.est(x, tr = 0.1, detail = FALSE)
#' @param x Vector of input data.
#' @param tr Treaming ratio. Default: 0.1
#' @param detail Print detailed output? Default: FALSE
#'
#'

#'
#' @return None.


#' @examples
#' location.est(mtcars$mpg, detail = T)
#' @export

location.est <- function(x, tr = 0.1, detail = FALSE) {
  n <- length(x)
  xmean <- mean(x)
  xmed <- median(x)
  tabx <- table(x)
  xmode <- as.numeric(names(tabx[tabx == max(tabx)]))
  gm_mean <- function(a) {
    prod(a)^(1 / length(a))
  }
  gmean <- gm_mean(x)
  hmean <- 1 / mean(1 / x)
  tmean <- mean(x, trim = tr)
  if (detail == FALSE) {
    cat(
      "Mean=", xmean, "\t Median=", xmed, "\t Mode=",
      xmode, "\n"
    )
    cat(
      "Geom. Mean=", gmean, "\t Harm. Mean=",
      hmean, paste0("\t Trim. Mean(", tr, ")="),
      tmean, "\n"
    )
  }
  if (detail == TRUE) {
    nt <- floor(n * tr)
    sumt <- sum(sort(x)[(nt + 1):(n - nt)])
    n2 <- n - 2 * nt
    cat(
      "Calculation in Detail -----------------------------------------------------",
      "\n(1) Mean =", paste0(sum(x), "/", n),
      "=", xmean, "\n(2) Median =", paste0(
        "x(",
        (n + 1) / 2, ") ="
      ), xmed, "\n(3) Mode =",
      xmode, paste0("(", max(tabx), "times)"),
      "\n(4) Geom. Mean =", paste0(
        format(prod(x),
          digits = 7, scientific = T
        ), "^(1/", n,
        ") ="
      ), gmean, "\n(5) Harm. Mean =",
      paste0(
        "1/", format(mean(1 / x), digits = 7),
        " ="
      ), hmean, paste0(
        "\n(6) Trim. Mean(",
        tr, ") = ", sumt, "/", n2
      ), "=",
      tmean, "\n"
    )
  }
}
