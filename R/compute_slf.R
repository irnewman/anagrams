
#' Compute summed letter frequency of word or anagram.
#'
#' @param word A word or anagram in string format.
#' @param l_freq A file of letter frequencies.
#'
#' @return Numeric SLF value based on letter frequency source.

compute_slf <- function(word, l_freq = ns_letter_freq) {

  # initialize values
  word_length <- nchar(word)
  number_of_letters <- word_length
  total_slf <- 0

  # split word into separate letters
  word_letters <- toupper(unlist(strsplit(word, "")))

  # each letter in a column
  letts <- data.frame(matrix(nrow = 0, ncol = word_length))
  letts <- rbind(letts, c(1:length(word_letters)))
  colnames(letts) <- c(paste0(rep("l", word_length), rep(1:word_length)))
  for (l in 1:word_length) {
    letts[letts == l] <- word_letters[l]
  }

  # create columns for each bigram
  # for (i in 1:number_of_bigrams) {
  #   bigrams[, ncol(bigrams) + 1] <-
  #     paste0(bigrams[, i], bigrams[, i+1])
  #   names(bigrams)[word_length + i] <- paste0("bg", i)
  # }

  # compute letter frequency columns
  for (j in 1:number_of_letters) {
    current_letter<-
      l_freq[
        , paste0("pos", j)][match(letts[
          , paste0("l", j)], l_freq$letter)]
    letts[, ncol(letts) + 1] <- current_letter

    total_slf <- ifelse(!is.na(current_letter),
                        total_slf + current_letter,
                        total_slf + 0)

    names(letts)[word_length + j] <-
      paste0("l", j, "_freq")
  }

  # set NA values to 0
  letts[is.na(letts)] <- 0.0

  # calculate total word sbf
  letts$slf <- total_slf

  return(total_slf)
}



