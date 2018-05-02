as_yaml <- function(x, ...) {
  UseMethod("as_yaml")
}

as_yaml.page <- function(x, ...) {
  yams <- yaml::as.yaml(x, indent = 2)
  class(yams) <- c("yaml", class(yams))
  yams
}

print.yaml <- function(x, ...) {
  cat(x)
  invisible(x)
}
