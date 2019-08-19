#' Title
#'
#' @param word
#'
#' @return
#' @export

check_vowels <- function(word) {

  vowels <- c("A", "E", "I", "O", "U", "Y")
  word_letters <- unlist(strsplit(word, ""))

  word_vowels <- c()

  for (l in word_letters) {
    word_vowels <- c(word_vowels, l %in% vowels)
  }

  return(any(word_vowels))

}
