get_front_matter <- function(file) {
  lines <- readLines(file)

  indeces <- which(lines == "---")

  if (length(indeces) < 2 || diff(indeces[1:2]) == 1) {
    return(NA_character_)
  }

  indeces <- c(indeces[1] + 1, indeces[2] - 1)

  paste0(lines[indeces], collapse = "\n")
}

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

slugify <- function(x) {
  tolower(gsub("([A-Z])", "-\\1", x, perl = TRUE))
}
