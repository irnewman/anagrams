
#' compute_sbf
#'
#' @param word a word or anagram in string format
#' @param bg_freq a file of bigram frequencies
#'
#' @return
#' @export
#'
#' @examples
#'
compute_sbf <- function(word, bg_freq = bigram_freq) {

  # init values
  word_length <- nchar(word)
  number_of_bigrams <- word_length - 1
  total_sbf <- 0

  # split word into separate letters
  word_letters <- toupper(unlist(strsplit(word, "")))

  # each letter in a column
  bigrams <- data.frame(matrix(nrow = 0, ncol = word_length))
  bigrams <- rbind(bigrams, c(1:5))
  colnames(bigrams) <- c(paste0(rep("l", word_length), rep(1:word_length)))
  for (l in 1:word_length) {
    bigrams[bigrams == l] <- word_letters[l]
  }

  # create columns for each bigram
  for (i in 1:number_of_bigrams) {
    bigrams[, ncol(bigrams) + 1] <-
      paste0(bigrams[, i], bigrams[, i+1])
    names(bigrams)[word_length + i] <- paste0("bg", i)
  }

  # create bigram frequency columns
  for (j in 1:number_of_bigrams) {
    current_bigram <-
      bigram_freq[
        , paste0("pos", j)][match(bigrams[
          , paste0("bg", j)], bigram_freq$bigram)]
    bigrams[, ncol(bigrams) + 1] <- current_bigram

    total_sbf <- ifelse(!is.na(current_bigram),
                        total_sbf + current_bigram,
                        total_sbf + 0)

    names(bigrams)[word_length + number_of_bigrams + j] <-
      paste0("bg", j, "_freq")
  }

  # set NA values to 0
  bigrams[is.na(bigrams)] <- 0.0

  # calculate total word sbf
  bigrams$sbf <- total_sbf

  return(total_sbf)

}
