#' @title Mosaic Plot
#' @description Mosaic Plot
#' @usage mosaic2(tab, mt, dig = 4, resid = TRUE)

#'
#' @param tab Data table
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 4
#' @param resid Display Pearson residuals? Default: TRUE
#'
#' @return Object from chisq.test()
#' @examples
#' require(vcd)
#' x <- c(39, 18, 12, 31, 14, 35, 23, 18, 35, 13, 27, 16, 17, 24, 8, 9, 12, 8, 19, 22)
#' x <- matrix(x, nrow = 4, ncol = 5, byrow = TRUE)
#' Subject <- c("Kor", "Eng", "Math", "Etc")
#' Hope <- c("Sam", "Pub", "Exp", "Sal", "Etc")
#' t1 <- as.table(x)
#' dimnames(t1) <- list(Subject = Subject, Hope = Hope)
#' win.graph(7, 6)
#' mosaic2(tab = t1, mt = "Students' Favorite Subject and Hope")
#' @export

mosaic2 <-
  function(tab, mt, dig = 4, resid = TRUE) {
    mosaic(tab, shade = T, pop = F, main = mt, labeling_args = list(
      offset_varnames = c(top = 1),
      offset_labels = c(top = 0.3)
    ))
    ct <- chisq.test(tab)
    cat(
      "Test statistic =", round(ct$stat, dig), "\t df =",
      ct$para, "\t P-value =", round(ct$p.val, dig),
      "\n"
    )
    if (resid) {
      labeling_cells(text = round(ct$res, 1), clip = FALSE)(tab)
    }
    invisible(ct)
  }
