#' @title Sample Space of Tossing Coins

#' @description Create Sample Space of Tossing Coins


#' @usage tosscoin2(times)

#' @param times Number of coins

#'
#' @return Sample space in data frame
#' @examples
#' tosscoin2(4)
#' @export

tosscoin2 <- function(times) {
  temp <- list()
  for (i in 1:times) {
    temp[[i]] <- c("H", "T")
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
  return(res)
}
