

#' Check for three of the same letter within a word (avoids bug with? find).
#'
#' @param word A word or anagram in string format.
#'
#' @return TRUE if two or more of same letter found.

check_double <- function(word) {

  # split word into separate letters
  word_letters <- unlist(strsplit(word, ""))

  # find duplicates
  word_duplicates <- c()
  for (l in word_letters) {
    word_duplicates <- c(word_duplicates,
                         lengths(regmatches(word, gregexpr(l, word))))
  }
  fail <- word_duplicates >= 2

  return(any(fail))
}
