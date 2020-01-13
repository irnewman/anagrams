
#' Compute longest increasing sub-sequence. Used in compute_moves function
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples


compute_lis <- function( x ){

  # function borrowed from link below:
  # https://www.rdocumentation.org/packages/LIStest/versions/1.0/topics/lis

  N <- length(x)
  gr <- c(1:N) * 0
  gr[1] <- 1

  for (i in 2:N) {
    gr[i] = 1

    for (j in 1:(i - 1)) {
      if (x[i] > x[j]) {
        gr[i] = max(gr[i], gr[j] + 1)
        }
    }
  }
  rr <- max(gr)
}
