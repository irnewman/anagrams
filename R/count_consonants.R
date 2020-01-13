


count_consonants <- function(word) {

  vowels <- c("A", "E", "I", "O", "U", "Y")
  word_letters <- toupper(unlist(strsplit(word, "")))

  word_vowels <- c()

  for (l in word_letters) {
    word_vowels <- c(word_vowels, l %in% vowels)
  }

  # return number of TRUE
  return(length(which(!word_vowels)))

}
