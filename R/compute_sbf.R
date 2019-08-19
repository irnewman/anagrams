




compute_sbf <- function(word, word_length, number_of_bigrams, bigram_freq) {

  # create table of letter permutations
  word_permu <- data.frame(gtools::permutations(
    n = word_length,
    r = word_length,
    v = 1:word_length))
  colnames(word_permu) <- c(paste0(rep("l", word_length), rep(1:word_length)))

  # split word into separate letters
  word_letters <- unlist(strsplit(word, ""))

  for (l in 1:word_length) {
    word_permu[word_permu == l] <- word_letters[l]
  }


  # create columns for each bigram
  for (i in 1:number_of_bigrams) {
    word_permu[, ncol(word_permu) + 1] <-
      paste0(word_permu[, i], word_permu[, i+1])
    names(word_permu)[word_length + i] <- paste0("bg", i)
  }


  # NOTE: for now, this will work as we cannot compute bigram frequencies for
    # words longer than 5, but this code will need to change later
  # compute bigram frequencies
  word_permu$bg1_freq <-
    bigram_freq$pos1[match(word_permu$bg1, bigram_freq$bigram)]
  word_permu$bg2_freq <-
    bigram_freq$pos2[match(word_permu$bg2, bigram_freq$bigram)]
  word_permu$bg3_freq <-
    bigram_freq$pos3[match(word_permu$bg3, bigram_freq$bigram)]
  word_permu$bg4_freq <-
    bigram_freq$pos4[match(word_permu$bg4, bigram_freq$bigram)]

  # set NA values to 0
  word_permu[is.na(word_permu)] <- 0.0

  # calculate total word sbf
  word_permu$sbf <-
    word_permu$bg1_freq +
    word_permu$bg2_freq +
    word_permu$bg3_freq +
    word_permu$bg4_freq

  # reduce to only unique combinations, relevant for duplicated letters
  word_permu <- word_permu %>%
    distinct()

  # sort by sbf
  sorted_word <- word_permu[order(-word_permu$sbf),]
  sorted_word$rank <- 1:length(sorted_word$sbf)
  sorted_word$word <- paste0(sorted_word$l1,
                             sorted_word$l2,
                             sorted_word$l3,
                             sorted_word$l4,
                             sorted_word$l5)

  word_sbf <- data.frame(sorted_word$word,
                         sorted_word$sbf,
                         sorted_word$rank)
  word_sbf$sorted_word.word <- as.character(word_sbf$sorted_word.word)
  colnames(word_sbf) <- c("order", "sbf", "rank")


  # compute moves
  for (i in 1:nrow(word_sbf)) {

    number_of_moves <- compute_moves(
      word,
      word_sbf$order[i])

    word_sbf$moves[i] <- number_of_moves
  }


  word_sbf$is_word <- apply(word_sbf, 1,
                            function(x) tolower(x[1]) %in% GradyAugmented)

  word_sbf$is_unique <- ifelse(sum(word_sbf$is_word, na.rm = TRUE) == 1,
                               1, sum(word_sbf$is_word, na.rm = TRUE))


  return(word_sbf)
}

