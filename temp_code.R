


# testing
solution <- "sniff"
anagram <- "ffins"





# load libraries
# ------------------------------
library(tidyverse)
#library(anagrams)



# load anagram lists
# ------------------------------
stim_lists <- list.files(pattern = "*.csv") %>%
  map_df(~read_csv(.))

stim_lists$move_check <- NA

# trying to get apply function working instead
for (i in 1:nrow(stim_lists)) {

  number_of_moves <- compute_moves(
    stim_lists$solution[i],
    stim_lists$anagram[i])

  stim_lists$move_check[i] <- number_of_moves

  if (number_of_moves != stim_lists$moves[i]) {
    print(i)
  }
}


solvable <- stim_lists %>%
  filter(solvability == 1)












# next: compute the number of moves needed based on the anagram order
  # not clear on how yet.

# 1. find the longest increasing subsequence (LIS)
# 2. length of vector minus LIS
  # 5 - LIS



