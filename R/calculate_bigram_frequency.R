

calculate_bigram_frequency <- function(lexicon)
{

  word_length <- nchar(lexicon[1])
  number_of_bigrams <- word_length - 1
  number_of_words <- length(lexicon)

  letter_combinations <- expand.grid(LETTERS, LETTERS)
  letter_combinations$bigrams <- paste0(
    letter_combinations$Var2, letter_combinations$Var1
  )


  bigram_frequency <- data.frame(matrix(nrow = nrow(letter_combinations),
                                        ncol = number_of_bigrams + 1))
  colnames(bigram_frequency) <- c("bigram",
                                  paste0(rep("pos", number_of_bigrams),
                                         rep(1:number_of_bigrams)))
  rownames(bigram_frequency) <- letter_combinations$bigrams

  bigram_frequency[is.na(bigram_frequency)] <- 0


  for (word in lexicon) {

    # split word into separate letters (make function)
    word_letters <- toupper(unlist(strsplit(word, "")))



    # STOP: find each bigram in each position here
    # each letter in a column
    bg_letters <- data.frame(matrix(nrow = 0, ncol = word_length))
    bg_letters <- rbind(bg_letters, c(1:word_length))
    colnames(bg_letters) <- c(paste0(rep("l", word_length), rep(1:word_length)))
    # letters <- data.frame(matrix(nrow = 0, ncol = word_length))
    # letters <- rbind(letters, c(1:word_length))
    # colnames(letters) <- c(paste0(rep("l", word_length), rep(1:word_length)))
    for (l in 1:word_length) {
      bg_letters[bg_letters == l] <- word_letters[l]
    }

    bigrams <- data.frame(matrix(nrow = 0, ncol = number_of_bigrams))
    bigrams <- rbind(bigrams, c(1:number_of_bigrams))
    colnames(bigrams) <- c(paste0(rep("bg", number_of_bigrams),
                                  rep(1:number_of_bigrams)))
    # for (bg in 1:number_of_bigrams) {
    #   bigrams[bigrams == bg] <- word_letters[l]
    # }

    # create columns for each bigram
    for (bg in 1:number_of_bigrams) {
      bigrams[, bg] <-
        paste0(bg_letters[, bg], bg_letters[, bg+1])
      names(bigrams)[bg] <- paste0("bg", bg)
    }

    # update frequency for each position in word
    for (i in 1:number_of_bigrams) {
      current_bigram <- bigrams[[i]]

      bigram_frequency[current_bigram, paste0("pos", i)] <-
        bigram_frequency[current_bigram, paste0("pos", i)] + 1

    }
  }


  sbf <- bigram_frequency

  # remove rows where all values are 0
  sbf <- sbf[(rowSums(sbf) != 0), ]

  sbf <- (sbf/number_of_words) * 1000  # multiply by 1000 for easier values

  sbf$bigram <- rownames(sbf)


  return(sbf)
}
