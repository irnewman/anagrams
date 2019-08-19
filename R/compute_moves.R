#' Find shortest number of moves to solution word.
#'
#' @param solution
#' @param anagram
#'
#' @return
#' @export

compute_moves <- function(solution, anagram) {

  # determine letters of solution and anagram
  sol_letters <- unlist(strsplit(solution, ""))
  ana_letters <- unlist(strsplit(anagram, ""))

  # init order of solution and anagram
  sol_order <- c(1:length(sol_letters))

  # find duplicate letters
  duplicates <- sol_letters[duplicated(sol_letters, solution)]

  # compute best anagram order
  ana_orders <- compute_positions(sol_letters, ana_letters, duplicates)

  # find shortest move length
  moves <- c()
  for (order in 1:length(ana_orders)) {
    moves <- c(moves,
                   length(ana_orders[[order]]) -
                     compute_lis(ana_orders[[order]]))
  }

  return(min(moves))
}

