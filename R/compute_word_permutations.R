
compute_word_permutations <- function(word) {


  word_length <- nchar(word)

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



  # reduce to only unique combinations, relevant for duplicated letters
  word_permu <- word_permu %>%
    distinct()


  permutations <- word_permu %>%
    tidyr::unite("permutations", colnames(word_permu), sep = "")


  p <- permutations[, 1]


  return(p)


}
