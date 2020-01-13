
first_letter <- function(word) {

  vowels <- c("A", "E", "I", "O", "U", "Y")
  word_letters <- toupper(unlist(strsplit(word, "")))

  letter <- ifelse(word_letters[1] %in% vowels, "vowel", "consonant")

  return(letter)
}
