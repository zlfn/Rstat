#' @title Intersection of Events
#' @description Intersection of Events in Data frame Format
#' @usage intersect2(x, y)
#' @export

intersect2 <- function(x, y) {
  if (is.vector(x)) {
    res <- intersect(x, y)
  }
  if (is.data.frame(x)) {
    nc <- ncol(x)
    x2 <- apply(x, 1, paste, collapse = ":")
    y2 <- apply(y, 1, paste, collapse = ":")
    dum <- intersect(x2, y2)
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
