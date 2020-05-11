#' @title Sample Space of Sampling Cards
#' @description Create Sample Space of Sampling Cards
#' @usage cards2(jokers = FALSE)
#' @param jokers Include a joker? Default: FALSE
#' @return Sample space in data frame
#' @examples
#' cards2()
#' @export


cards2 <- function(jokers = FALSE) {
  x <- c(2:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(rank = x, suit = y)
  if (jokers) {
    levels(res$rank) <- c(levels(res$rank), "Joker")
    res <- rbind(res, data.frame(rank = c(
      "Joker",
      "Joker"
    ), suit = c(NA, NA)))
  }
  return(res)
}
