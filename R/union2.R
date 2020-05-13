#' @title Union of Events
#' @description Union of Events in Data frame Format
#' @usage union2(x, y)
#' @export

union2 <- function(x, y) {
  if (is.vector(x)) {
    res <- union(x, y)
  }
  if (is.data.frame(x)) {
    nc <- ncol(x)
    x2 <- apply(x, 1, paste, collapse = ":")
    y2 <- apply(y, 1, paste, collapse = ":")
    dum <- union(x2, y2)
    dd <- strsplit(dum, split = ":")
    nr <- length(dd)
    res <- as.data.frame(matrix(unlist(dd), nr, nc, byrow = T))
    names(res) <- c(paste(rep("X", nc), 1:nc, sep = ""))
  }
  return(res)
}
