
# pass the dataframe that would be printed as a csv
# also pass the name of the file

compute_nonwords <- function(word_file, filename) {

  # need to keep a parent directory and create multiple subdirectories



  name <- gsub(".csv", "", filename)

  # create a subfolder = name
  current_dir <- getwd()
  dir.create(file.path(current_dir, "name"))

  # create a list of word options
    # for each letter of name, change one letter

  # after completing that, would do the same as in sbf script
    # change to function?



}
