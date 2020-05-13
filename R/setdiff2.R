#' @title Set Difference of Events
#' @description Set Difference of Events in Data frame Format
#' @usage setdiff2(x, y)
#' @export

setdiff2 <- function(x, y) {
  if (is.vector(x)) {
    res <- setdiff(x, y)
  }
  if (is.data.frame(x)) {
    nc <- ncol(x)
    x2 <- apply(x, 1, paste, collapse = ":")
    y2 <- apply(y, 1, paste, collapse = ":")
    dum <- setdiff(x2, y2)
    if (length(dum) > 0) {
      dd <- strsplit(dum, split = ":")
      nr <- length(dd)
      res <- as.data.frame(matrix(unlist(dd), nr, nc, byrow = T))
      names(res) <- c(paste(rep("X", nc), 1:nc, sep = ""))
    }
    else {
      res <- NULL
    }
  }
  return(res)
}
