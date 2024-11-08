
# temp wrapper for generating output files like I had produced before
  # but: with more values calculated

# params:
# lexicon
# letter/bigram/trigram frequencies (can add to package so we have default ones)


# for each word in lexicon
  # compute all anagrams -> compute features of each anagram
  # return output as a data frame


# function: load files based on word (name of file) and select from them

# function to load file based on word (name of file) and compute alternatives
  # DO NOT do this in a loop through every word
  # find a way to do it incrementally and zip files at end or something




create_word_files <- function(lexicon = ns_lexicon,
                                 letter_freq = ns_letter_freq,
                                 bigram_freq = ns_bigram_freq,
                                 trigram_freq = ns_trigram_freq)
{

  # lexicon can be set to default as ns_lexicon
  # frequency files can be default ns_bigfram_freq

  # set the working directory
  parent_dir <- (paste0(here::here(), "\\words\\new"))
  if(file.exists(parent_dir)) {
    setwd(file.path(parent_dir))
  } else {
    dir.create(file.path(parent_dir))
    setwd(file.path(parent_dir))
  }

  counter <- 1

  lexicon <- ns_lexicon # REMOVE AFTER


  # each variable to be computed, based on the functions written so far
  computed_values <- c("string",
                       "slf", "sbf", "stf",
                       "rank", "distance_to_rank1",
                       "moves", "vowels", "consonants",
                       "is_word", "is_unique",
                       "intact_letters", "num_intact",
                       "preserved_bigrams", "num_preserved",
                       "first_letter", "same_first_letter")


  for (word in lexicon) {



    print(paste0(
      "Working on word ", toupper(word), " which is word number ", counter,
      " of ", length(lexicon)
    ))


    word_list <- data.frame(matrix(nrow = 0, ncol = length(computed_values)))
    colnames(word_list) <- computed_values


    permutations <- compute_word_permutations(word)



    # for each, compute all these things, save in a row, compile into a df
    for (p in permutations) {



      word_row <- data.frame(matrix(nrow = 1, ncol = length(computed_values)))
      colnames(word_row) <- computed_values

      #print(p)
      # COMPUTE ALL THE INDICES

      # these match the above computed_values list

      # current permutation of letters
      word_row$string <- toupper(p)

      # summed frequencies
      word_row$slf <- compute_slf(p)
      word_row$sbf <- compute_sbf(p)
      word_row$stf <- compute_stf(p)


      word_row$vowels <- count_vowels(p)
      word_row$consonants <- count_consonants(p)

      word_row$moves <- compute_moves(word, p)


      word_row$intact_letters <- list(intact_letters(word, p))
      word_row$num_intact <- ifelse(all(is.na(intact_letters(word, p))),
                                    0,
                                    length(intact_letters(word, p)))




          # THIS RETURNS THE INDICES, NOT NUMBER OF THEM
      # number of intact is length of intact_letters

      word_row$preserved_bigrams <- list(preserved_bigrams(word, p))
      word_row$num_preserved <- ifelse(all(is.na(preserved_bigrams(word, p))),
                                             0,
                                             length(preserved_bigrams(word, p)))
          # RETURNS THE BIGRAM, NOT NUMBER OF THEM
      # number of preserved is length of preserved_bigrams

      word_row$first_letter <- first_letter(p)
      word_row$same_first_letter <-
        unlist(strsplit(word, ""))[1] == unlist(strsplit(p, ""))[1]
        # is the first the same

      word_row$is_word <- tolower(p) %in% GradyAugmented



      # ALSO: just what the first letter of the solution is
      ## maybe function same_first_letter

      # RANK = compute all sbf, then sort the list
      # IS WORD = is it in gradyaugmented
      # IS unique = is it the only TRUE in IS WORD

      #print(slf)
      #print(sbf)
      #print(stf)

      word_list <- rbind(word_list, word_row)

    }

    # sorting only by SBF for now - could change in the future
    sorted_list <- word_list[order(-word_list$sbf), ]

    sorted_list$rank <- 1:nrow(sorted_list)

    # compute distance to rank 1 for convergence
    rank_1_sbf <- sorted_list$sbf[1]
    for (i in 1:nrow(sorted_list)) {
      sorted_list$distance_to_rank1[i] <-
        rank_1_sbf - sorted_list$sbf[i]
    }

    sorted_list$is_unique <- ifelse(
      sum(sorted_list$is_word, na.rm = TRUE) == 1,
      1,
      sum(sorted_list$is_word, na.rm = TRUE))

    # FUNCTION - takes a word, calls all the calculations, saves them in a row


    file_name <- paste0(toupper(word), ".csv")
    data.table::fwrite(sorted_list, file = file_name)


    counter <- counter + 1
  }




  summarize_options(parent_dir)

}
