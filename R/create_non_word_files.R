
# update this to run N times and save the finished to the list
  # to run in batches

create_non_word_files <- function()
{

  # set the working directory
  parent_dir <- (paste0(here::here(), "\\words\\new"))
  if(file.exists(parent_dir)) {
    setwd(file.path(parent_dir))
  } else {
    dir.create(file.path(parent_dir))
    setwd(file.path(parent_dir))
  }

  # use the _report.csv file to generate nonwords for now
  word_list <- data.table::fread(
    file = paste0(parent_dir, "\\_nonwords_to_make.csv")) %>%
    filter(made == 0)

  words <- word_list$word

  counter <- 1
  # each variable to be computed, based on the functions written so far
  computed_values <- c("string",
                       "slf", "sbf", "stf",
                       #"rank", "distance_to_rank1",
                       #"moves",
                       "vowels", "consonants",
                       "is_word", #"is_unique",
                       #"intact_letters", "num_intact",
                       #"preserved_bigrams", "num_preserved",
                       "first_letter"# , #"same_first_letter"
                       )

  for (word in words) {

    print(paste0(
      "Working on nonword folder for word ", toupper(word), " which is word number ", counter,
      " of ", length(words)
    ))

    # make a list of potential nonwords from the current word
    alt <- generate_nonwords(word)

    # create a sub directory and set working directory to that
    nonword_dir <- (paste0(parent_dir, "\\", word))
    if(file.exists(nonword_dir)) {
      setwd(file.path(nonword_dir))
    } else {
      dir.create(file.path(nonword_dir))
      setwd(file.path(nonword_dir))
    }


    # for each alternate nonword, compute each permutation and compute indices
    for (a in alt) {

      word_list <- data.frame(matrix(nrow = 0, ncol = length(computed_values)))
      colnames(word_list) <- computed_values

      permutations <- compute_word_permutations(a)

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

        #word_row$moves <- compute_moves(alt[1], p)


        #word_row$intact_letters <- list(intact_letters(alt[1], p))
        # word_row$num_intact <- ifelse(all(is.na(intact_letters(alt[1], p))),
        #                               0,
        #                               length(intact_letters(alt[1], p)))




        # THIS RETURNS THE INDICES, NOT NUMBER OF THEM
        # number of intact is length of intact_letters

        # word_row$preserved_bigrams <- list(preserved_bigrams(alt[1], p))
        # word_row$num_preserved <- ifelse(all(is.na(preserved_bigrams(alt[1], p))),
        #                                  0,
        #                                  length(preserved_bigrams(alt[1], p)))
        # RETURNS THE BIGRAM, NOT NUMBER OF THEM
        # number of preserved is length of preserved_bigrams

        word_row$first_letter <- first_letter(p)
        # word_row$same_first_letter <-
        #   unlist(strsplit(word, ""))[1] == unlist(strsplit(p, ""))[1]
        # # is the first the same

        word_row$is_word <- tolower(p) %in% qdapDictionaries::GradyAugmented



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

      #sorted_list$rank <- 1:nrow(sorted_list)

      # sorted_list$is_unique <- ifelse(
      #   sum(sorted_list$is_word, na.rm = TRUE) == 1,
      #   1,
      #   sum(sorted_list$is_word, na.rm = TRUE))

      file_name <- paste0(toupper(a), ".csv")
      data.table::fwrite(sorted_list, file = file_name)
    }

    counter <- counter + 1
    summarize_options(nonword_dir)
  }


}
