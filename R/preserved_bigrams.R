
preserved_bigrams <- function(solution, anagram) {

  # init values
  solution_length <- nchar(solution)
  number_of_bigrams <- solution_length - 1

  # split word into separate letters
  solution_letters <- toupper(unlist(strsplit(solution, "")))
  anagram_letters <- toupper(unlist(strsplit(anagram, "")))


  solution_bigrams <- c()
  anagram_bigrams <- c()

  for (i in 1:number_of_bigrams) {
    solution_bigrams <- c(solution_bigrams,
                          paste0(solution_letters[i], solution_letters[i+1]))
    anagram_bigrams <- c(anagram_bigrams,
                         paste0(anagram_letters[i], anagram_letters[i+1]))
  }


  preserved_bg <- c()

  for (j in solution_bigrams) {
    if (j %in% anagram_bigrams) {
      preserved_bg <- c(preserved_bg, j)
    }
  }


  if (length(preserved_bg) == 0) {
    preserved_bg <- NA
  }

  return(preserved_bg)


}
