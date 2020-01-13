
# should be unnecessary to run for the user

load_lexicons <- function() {

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

  usethis::use_data(bigram_freq, lexicon, nonword_lexicon,
                    internal = TRUE, overwrite = TRUE)
}
