
intact_letters <- function(solution, anagram) {

  intact <- c()

  # determine letters of solution and anagram
  sol_letters <- toupper(unlist(strsplit(solution, "")))
  ana_letters <- toupper(unlist(strsplit(anagram, "")))

  # probably better way to do this
  for (i in 1:length(sol_letters)) {
    if(sol_letters[i] == ana_letters[i]) {
      intact <- c(intact, i)
    }
  }

  if (length(intact) == 0) {
    intact <- NA
  }

  return(intact)
}
