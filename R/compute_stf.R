

#' Compute summed trigram frequency of word or anagram.
#'
#' @param word A word or anagram in string format.
#' @param tg_freq A file of trigram frequencies.
#'
#' @return Numeric STF value based on trigram frequency source.

compute_stf <- function(word, tg_freq = ns_trigram_freq) {

  # initialize values
  word_length <- nchar(word)
  number_of_trigrams <- word_length - 2
  total_stf <- 0

  # split word into separate letters
  word_letters <- toupper(unlist(strsplit(word, "")))

  # each letter in a column
  trigrams <- data.frame(matrix(nrow = 0, ncol = word_length))
  trigrams <- rbind(trigrams, c(1:length(word_letters)))
  colnames(trigrams) <- c(paste0(rep("l", word_length), rep(1:word_length)))
  for (l in 1:word_length) {
    trigrams[trigrams == l] <- word_letters[l]
  }

  # create columns for each bigram
  for (i in 1:number_of_trigrams) {
    trigrams[, ncol(trigrams) + 1] <-
      paste0(trigrams[, i], trigrams[, i+1], trigrams[, i+2])
    names(trigrams)[word_length + i] <- paste0("tg", i)
  }

  # compute bigram frequency columns
  for (j in 1:number_of_trigrams) {
    current_trigram <-
      tg_freq[
        , paste0("pos", j)][match(trigrams[
          , paste0("tg", j)], tg_freq$trigram)]
    trigrams[, ncol(trigrams) + 1] <- current_trigram

    total_stf <- ifelse(!is.na(current_trigram),
                        total_stf + current_trigram,
                        total_stf + 0)

    names(trigrams)[word_length + number_of_trigrams + j] <-
      paste0("tg", j, "_freq")
  }

  # set NA values to 0
  trigrams[is.na(trigrams)] <- 0.0

  # calculate total word sbf
  trigrams$stf <- total_stf

  return(total_stf)
}

