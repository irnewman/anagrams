
# 1. load all words, report rank, sbf, num_of_solutions
# a. maybe use longer list of words from qdap
# b. should be easy enough
# c. make this a function for use later, load a single csv, do things


summarize_options <- function(folder) {

  # load all csv in the folder
  setwd(folder)
  word_files <- list.files(pattern = "*.csv")

  # init report
  word_report <- as.data.frame(matrix(nrow = 0, ncol = 4))
  colnames(word_report) <- c("word", "sbf", "rank", "is_unique")

  # compute report from each file
  for (w in word_files) {

    current_file <- read_csv(w)
    name <- gsub(".csv", "", w)

    word_summary <- current_file %>%
      filter(order == name) %>%
      select(order, sbf, rank, is_unique) %>%
      rename(word = order)

    word_report <- bind_rows(word_report, word_summary)

  }

  # output file
  data.table::fwrite(word_report, file = "_report.csv")

}
