has_default <- function(code) {
  grepl("||", code, fixed = TRUE)
}

get_default <- function(code) {
  trimws(sub("^.+\\|\\|", "", code))
}

strip_default <- function(code) {
  sub("\\|\\|.+$", "", code)
}

has_format <- function(code) {
  grepl(">", code, fixed = TRUE)
}

get_format <- function(code) {
  trimws(sub("^.+>", "", code))
}

strip_format <- function(code) {
  sub(">.+$", "", code)
}

defaults_transformer <- function(code, env) {
  default <- ""

  if (has_default(code)) {
    default <- get_default(code)
    code <- strip_default(code)
  }

  tryCatch(
    if (has_format(code)) {
      format <- get_format(code)
      code <- strip_format(code)
      sprintf(format, evaluate(code, env))
    } else {
      evaluate(code, env)
    },
    error = function(e) default
  )
}

glue_hyderogen <- function(..., .open = "{:", .close = ":}",
                           .transformer = defaults_transformer) {
  glue_data(..., .open = .open, .close = .close, .transformer = .transformer)
}
