#' @title Sample Space of Rolling Dice
#' @description Create Sample Space of Rolling Dice
#' @usage rolldie2(times, nsides = 6)
#' @param times Number of dice
#' @param nsides Number of sides of a die, Default: 6
#'
#'
#' @return Sample space in data frame
#' @examples
#' rolldie2(2)
#' @export
rolldie2 <- function(times, nsides = 6) {
  temp <- list()
  for (i in 1:times) {
    temp[[i]] <- 1:nsides
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
  return(res)
}
