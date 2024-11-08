
#' Check for any vowels in the word.
#'
#' @param word A word or anagram in string format.
#'
#' @return TRUE if any vowels in the string.

check_vowels <- function(word) {

  vowels <- c("A", "E", "I", "O", "U", "Y")

  # split word into separate letters
  word_letters <- toupper(unlist(strsplit(word, "")))
  #word_letters <- unlist(strsplit(word, ""))

  # find vowels
  word_vowels <- c()
  for (l in word_letters) {
    word_vowels <- c(word_vowels, l %in% vowels)
  }

  return(any(word_vowels))
}
