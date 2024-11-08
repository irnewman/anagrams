
# ======================================================================= #
# anagrams package: internal package files                             ####
#
# Ian R Newman - ian.newman@usask.ca
#
# https://github.com/irnewman/anagrams
# ======================================================================= #


# load files                ####
# ============================ #

# novick and sherman lexicon
novick_sherman <- read.table(paste0(here::here(), "\\ns_lexicon.txt"),
                             stringsAsFactors = FALSE)
ns_lexicon <- novick_sherman$V1


# compute frequencies       ####
# ============================ #

ns_letter_freq <- calculate_letter_frequency(ns_lexicon)
ns_bigram_freq <- calculate_bigram_frequency(ns_lexicon)
ns_trigram_freq <- calculate_trigram_frequency(ns_lexicon)



# saves files to package    ####
# ============================ #

usethis::use_data(
  ns_lexicon,
  ns_letter_freq,
  ns_bigram_freq,
  ns_trigram_freq,
  internal = FALSE,
  overwrite = TRUE)
