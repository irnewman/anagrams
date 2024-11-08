
# I WAS SETTING LEXICON TO BE THE FIRST COLUMN OF LEXICON FILE

calculate_letter_frequency <- function(lexicon)
{

  word_length <- nchar(lexicon[1])
  number_of_words <- length(lexicon)

  # FUNCTION = do below, return data frame
  letter_frequency <- data.frame(matrix(nrow = length(letters),
                                        ncol = word_length + 1))
  colnames(letter_frequency) <- c("letter",
                                  paste0(rep("pos", word_length),
                                         rep(1:word_length)))
  rownames(letter_frequency) <- LETTERS

  letter_frequency[is.na(letter_frequency)] <- 0


  for (word in lexicon) {

    # split word into separate letters
    word_letters <- toupper(unlist(strsplit(word, "")))

    # update frequency for each position in word
    for (i in 1:word_length) {
      current_letter <- word_letters[i]

      letter_frequency[current_letter, paste0("pos", i)] <-
        letter_frequency[current_letter, paste0("pos", i)] + 1

    }
  }


  slf <- letter_frequency
  slf <- (slf/number_of_words) * 1000  # multiply by 100 for easier values
  slf$letter <- rownames(slf)

  return(slf)
}
