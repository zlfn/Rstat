#' @title Minimum Number of Samples

#' @description Minimum Number of Samples from Normal Population


#' @usage norm.spn(kp, alp, lo = 0.1, up = 1, mt, dcol, log = TRUE)

#' @param kp Error limit in multiples of the standard deviation
#' @param alp Level of significance vector
#' @param lo Lower limit of the error limit, Default: 0.1
#' @param up Upper limit of the error limit, Default: 1
#' @param mt Plot title
#' @param dcol Line color (default=rainbow())
#' @param log Logical value for log-scaling y-axis, Default: TRUE
#'


#'
#'
#' @return None.
#' @examples
#' alp <- c(0.01, 0.05, 0.1)
#' dcol <- c(2, 4, "green4")
#' norm.spn(kp = 0.4, alp, dcol = dcol)
#' @export
norm.spn <-
  function(kp, alp, lo = 0.1, up = 1, mt, dcol, log = TRUE) {
    spn <- function(k, alp) ceiling((qnorm(1 - alp / 2) / k)^2)
    nalp <- length(alp)
    nkp <- length(kp)
    if (min(nalp, nkp) == 1) {
      mspn <- spn(kp, alp)
      if (nalp > 1) {
        names(mspn) <- alp
      }
      if (nkp > 1) {
        names(mspn) <- kp
      }
    }
    else if (nkp > nalp) {
      mspn <- outer(alp, kp, "spn")
      colnames(mspn) <- kp
      rownames(mspn) <- alp
    }
    else {
      mspn <- outer(kp, alp, "spn")
      colnames(mspn) <- alp
      rownames(mspn) <- kp
    }
    print(mspn)
    if (missing(dcol)) {
      dcol <- rainbow(nalp)
    }
    if (missing(mt)) {
      mt <- paste0("Minimum Number of Samples for ", paste(kp,
        collapse = "/"
      ), "-sigma Error Limit")
    }
    win.graph(7, 6)
    kv <- seq(lo, up, length = 100)
    if (log) {
      plot(kv, spn(kv, alp[1]),
        type = "n", log = "y",
        ylab = "Number of sample", xlab = "k",
        ylim = c(1, spn(kv[1], min(alp))), main = mt
      )
    }
    else {
      plot(kv, spn(kv, alp[1]),
        type = "n", ylab = "Number of sample",
        xlab = "k", ylim = c(1, spn(kv[1], min(alp))),
        main = mt
      )
    }
    grid()
    for (i in 1:nalp) lines(kv, spn(kv, alp[i]), lwd = 2, col = dcol[i])
    leg <- list()
    for (i in 1:nalp) leg[[i]] <- bquote(alpha == .(alp[i]))
    legend("topright", sapply(leg, as.expression),
      lwd = 2,
      col = dcol
    )
    segments(kp, 1, kp, spn(kp, min(alp)), lty = 2, col = 6)
    segments(min(kv), spn(kp, alp), kp, spn(kp, alp),
      lty = 2,
      col = 6
    )
    text(min(kv), spn(kp, alp), labels = spn(kp, alp), col = 4)
  }
