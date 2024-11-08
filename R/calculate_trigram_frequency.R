

calculate_trigram_frequency <- function(lexicon)
{

  word_length <- nchar(lexicon[1])
  number_of_trigrams <- word_length - 2
  number_of_words <- length(lexicon)

  letter_combinations <- expand.grid(LETTERS, LETTERS, LETTERS)
  letter_combinations$trigrams <- paste0(
    letter_combinations$Var3, letter_combinations$Var2, letter_combinations$Var1
  )


  trigram_frequency <- data.frame(matrix(nrow = nrow(letter_combinations),
                                         ncol = number_of_trigrams + 1))
  colnames(trigram_frequency) <- c("trigram",
                                   paste0(rep("pos", number_of_trigrams),
                                          rep(1:number_of_trigrams)))
  rownames(trigram_frequency) <- letter_combinations$trigrams

  trigram_frequency[is.na(trigram_frequency)] <- 0


  for (word in lexicon) {

    # split word into separate letters (make function)
    word_letters <- toupper(unlist(strsplit(word, "")))


    # STOP: find each trigram in each position here
    # each letter in a column
    tg_letters <- data.frame(matrix(nrow = 0, ncol = word_length))
    tg_letters <- rbind(tg_letters, c(1:word_length))
    colnames(tg_letters) <- c(paste0(rep("l", word_length), rep(1:word_length)))
    for (l in 1:word_length) {
      tg_letters[tg_letters == l] <- word_letters[l]
    }

    trigrams <- data.frame(matrix(nrow = 0, ncol = number_of_trigrams))
    trigrams <- rbind(trigrams, c(1:number_of_trigrams))
    colnames(trigrams) <- c(paste0(rep("tg", number_of_trigrams),
                                  rep(1:number_of_trigrams)))
    # for (bg in 1:number_of_bigrams) {
    #   bigrams[bigrams == bg] <- word_letters[l]
    # }

    # create columns for each trigram
    for (tg in 1:number_of_trigrams) {
      trigrams[, tg] <-
        paste0(tg_letters[, tg], tg_letters[, tg+1], tg_letters[, tg+2])
      names(trigrams)[tg] <- paste0("tg", tg)
    }

    # update frequency for each position in word
    for (i in 1:number_of_trigrams) {
      current_trigram <- trigrams[[i]]

      trigram_frequency[current_trigram, paste0("pos", i)] <-
        trigram_frequency[current_trigram, paste0("pos", i)] + 1

    }
  }


  stf <- trigram_frequency

  # remove rows where all values are 0
  stf <- stf[(rowSums(stf) != 0), ]

  stf <- (stf/number_of_words) * 1000  # multiply by 1000 for easier values


  stf$trigram <- rownames(stf)

  return(stf)
}
