



check_triple <- function(word) {

  word_letters <- unlist(strsplit(word, ""))

  word_duplicates <- c()

  for (l in word_letters) {
    word_duplicates <- c(word_duplicates,
                         lengths(regmatches(word, gregexpr(l, word))))
  }


  fail <- word_duplicates >= 3

  return(any(fail))

}
