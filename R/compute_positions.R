
#' Compute the order of solution letters within the anagram. used in
#' compute_moves function
#'
#' @param sol_letters Vector of letters in the solution.
#' @param ana_letters Vector of letters in the anagram.
#' @param duplicates Vector of characters that are duplicated in the solution.
#'
#' @return
#' @export

compute_positions <- function(sol_letters, ana_letters, duplicates) {

  # init anagram order vector and list
  ana_order <- c()
  possible <- list()

  # start with one order option (if no duplicates, this is the solution)
  for (i in 1:length(ana_letters)) {

    # find the positions(s) of current letter in the solution
    position <- grep(ana_letters[i], sol_letters, fixed = TRUE)

    # if the letter occurs more than once
    if (length(position) > 1) {
      for (j in 1:length(position)) {
        # add the first index that occurs that is not in ana_order yet
        if (!(position[j] %in% ana_order)) {
          ana_order <- cbind(ana_order, position[j])
          break
        }
      }
    } else {
      ana_order <- cbind(ana_order, position)
    }
  }

  # set one order as base
  possible[[1]] <- as.vector(ana_order)

  # compute other possible orders based on duplicated letters
  if (length(duplicates) > 0) {

    # find permutations of duplicates
    for (d in 1:length(duplicates)) {

      # indices of current duplicate in the solution
      sol_indices <- c(grep(duplicates[d], sol_letters))

      # indices of current duplicate in the anagram
      ana_indices <- match(sol_indices, as.vector(ana_order))

      this_letter <- gtools::permutations(
        n = length(ana_indices),
        r = length(ana_indices),
        v = ana_indices)

      rows <- length(possible)

      for (p in 1:length(possible)) {
        possible[[1 + length(possible)]] <-
          as.vector(
            replace(possible[[p]],
                    this_letter[1,],
                    possible[[p]][this_letter[2,]]))
      }
    }
  }

  # return list of possible orders
  return(possible)
}
