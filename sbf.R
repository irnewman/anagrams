
# NOTE: there are some bigrams that did not exist in the lexicon
# those bigram values resulted in "NA" values in the program
# I have set those values to 0


# functions?
  # load lexicon? or add that to the package somehow
    # see load SBF files section
  # word permu: for computing all permutations of a set of letters
    # but not for sbf of single word



# load libraries
# ------------------------------
library(tidyverse)
library(data.table)
library(qdapDictionaries)
library(anagrams)


# load SBF files
# ------------------------------
bigram_freq <- read.table(paste0(here::here(), "\\bigram frequencies.txt"),
                          stringsAsFactors = FALSE)
lexicon <- read.table(paste0(here::here(), "\\lexicon.txt"),
                      stringsAsFactors = FALSE)
nonword_lexicon <- read.table(paste0(here::here(), "\\nonwords.txt"))

# clean up the bigram frequencies
bigram_freq$V1 <- as.character(bigram_freq$V1)
bigram_freq$V1[is.na(bigram_freq$V1)] <- "NA"


# note: change this to a more general naming based on word length
colnames(bigram_freq) <- c("bigram",
                           "pos1", "pos2", "pos3", "pos4")
colnames(lexicon) <- c("word")


# NOTE: our input files only have bigram frequences for letter positions in
# five letter words, so to do this for other word lengths, we need to find or
# calculate more bigram frequencies
word_length <- 5  # nchar(word) in the future
number_of_bigrams <- word_length - 1

# TESTING
lexicon <- read.table(paste0(here::here(), "\\test items.csv"),
                      stringsAsFactors = FALSE)
colnames(lexicon) <- c("word")
lexicon$word <- toupper(lexicon$word)

# create a list of words
word_list <- lexicon$word

# set the working directory
parent_dir <- (paste0(here::here(), "\\355"))


# loop through each word
for (word in word_list) {

  setwd(parent_dir)

  # compute word sbf, rank, moves, solution, solution uniqueness
  word_sbf <- compute_sbf(word, word_length, number_of_bigrams, bigram_freq)

  # output file
  filename <- paste0(word, ".csv")
  data.table::fwrite(word_sbf, file = filename)

  # create folder for current word
  sub_dir <- paste0(parent_dir, "\\", word)

  if(file.exists(sub_dir)) {
    setwd(file.path(sub_dir))
  } else {
    dir.create(file.path(sub_dir))
    setwd(file.path(sub_dir))
  }

  # # create list of potential alternatives
  # alternatives <- generate_nonwords(word)
  #
  # # compute sbf on the alternatives
  # for (alt in alternatives) {
  #
  #   # compute word sbf, rank, moves, solution, solution uniqueness
  #   alt_sbf <- compute_sbf(alt, word_length, number_of_bigrams, bigram_freq)
  #
  #   # output file
  #   alt_filename <- paste0(alt, ".csv")
  #   data.table::fwrite(alt_sbf, file = alt_filename)
  #
  # }
  #
  #
  # # summarize alternatives
  # summarize_options(sub_dir)



}


# summarize words
summarize_options(parent_dir)

# ------------------------------
