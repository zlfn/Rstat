#' @title Sample Space of Urn Sampling
#' @description Create Sample Space of Urn Sampling
#' @usage urnsample2(x, size, replace = FALSE, ordered = FALSE, probspace = FALSE, ...)
#' @param x Vector of objects in the urn
#' @param size Number of samples from the urn
#' @param replace Sampling with replacement? Default: FALSE
#' @param ordered Consider the order of samples? Default: FALSE
#' @param probspace Create probablity space? Default: FALSE
#' @param ... Other parameters
#'
#'
#' @return Sample space in data frame
#' @examples
#' urnsample2(letters[1:5], 3)
#' @export
urnsample2 <-
  function(x, size, replace = FALSE, ordered = FALSE, probspace = FALSE,
           ...) {
    n <- length(x)
    xf <- as.factor(x)
    if (replace) {
      if (ordered) {
        temp <- list()
        for (i in 1:size) {
          temp[[i]] <- x
        }
        res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
      }
      else {
        dum <- combnWithRepetition(n, size)
        res <- as.data.frame(t(matrix(xf[dum], size, ncol(dum))))
        if (probspace) {
          dd <- apply(res, 1, paste, collapse = ":")
          tab <- table(dd)
          res <- data.frame(matrix(unlist(strsplit(
            names(tab),
            ":"
          )), length(tab), size, byrow = TRUE))
          res$freq <- as.vector(tab)
        }
      }
    }
    else {
      if (ordered) {
        dum <- permn(x, size)
        res <- as.data.frame(matrix(
          as.factor(dum), nrow(dum),
          size
        ))
        return(res)
      }
      else {
        dum <- combn(x, size)
        res <- as.data.frame(t(dum))
        if (probspace) {
          dd <- apply(res, 1, paste, collapse = ":")
          tab <- table(dd)
          res <- data.frame(matrix(unlist(strsplit(
            names(tab),
            ":"
          )), length(tab), size, byrow = TRUE))
          res$freq <- as.vector(tab)
        }
      }
    }
    return(res)
  }
