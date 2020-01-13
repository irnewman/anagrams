


generate_nonwords <- function(word) {

  alt_words <- c()
  word_letters <- unlist(strsplit(word, ""))


  for (i in 1:length(word_letters)) {

    base_word <- word

    letter <- word_letters[i]
    other_letters <- LETTERS[LETTERS != letter]

    for (j in other_letters) {

      substr(base_word, i, i) <- j
      alt_words <- c(alt_words, base_word)

    }


  }


  # remove any with zero vowels or with 3 of one letter
  for (w in alt_words) {

    # remove words without vowels
    if (check_vowels(w) == FALSE) {
      alt_words <- alt_words[alt_words != w]
    }

    # remove words with 3+ of same letter
    if (check_triple(w) == TRUE) {  # if any letter is a triple
      alt_words <- alt_words[alt_words != w]
    }

  }


  return(alt_words)


}


