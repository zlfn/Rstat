#' @title Prior and Posterior Probabilities
#' @description Displaying the Prior and the Posterior Probabilities
#' @usage bayes.plot(prior, post, group, cond, dcol, cex = 1, dig = 4)
#' @param prior Prior probability distribution vector.
#' @param post Posterior probability distribution vector.
#' @param group Class names, Default: A, B, C, ...
#' @param cond Conditional event name, Default: F
#' @param dcol Bar chart colors, Default: transparent rainbow colors
#' @param cex Text size of the probability, Default: 1
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' prior <- c(0.2, 0.4, 0.3, 0.1)
#' cond <- c(0.04, 0.02, 0.01, 0.05)
#' tot <- prior * cond
#' post <- tot / sum(tot)
#' bayes.plot(prior, post)
#' @export

bayes.plot <-
  function(prior, post, group, cond, dcol, cex = 1, dig = 4) {
    n <- length(prior)
    if (missing(dcol)) {
      dcol <- rainbow(n, alpha = 0.3)
    }
    if (missing(group)) {
      group <- LETTERS[1:n]
    }
    if (missing(cond)) {
      cond <- "F"
    }
    dev.new(7, 4)
    dum <- barplot(cbind(prior, post),
      col = dcol, main = "Prior Probability vs. Posterior Probability",
      horiz = T
    )
    centprior <- cumsum(prior) - prior / 2
    centpost <- cumsum(post) - post / 2
    text(centprior, dum[1], labels = paste0(
      "P(", group,
      ")\n", prior
    ), cex = cex)
    text(centpost, dum[2],
      labels = paste0(
        "P(", group,
        "|", cond, ")\n", format(post, digits = dig)
      ),
      cex = cex
    )
  }
