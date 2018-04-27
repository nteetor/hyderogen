is_document <- function(x) {
  inherits(x, "document")
}

is_page <- function(x) {
  inherits(x, "page")
}

is_item <- function(x) {
  inherits(x, "item")
}

make_documents <- function(blocks) {
  map(blocks, as_document)
}

as_document <- function(block) {
  tags <- names(block)
  attributes(block) <- NULL
  names(block) <- tags

  classes <- "document"

  if (utils::hasName(block, "family")) {
    classes <- c("item", classes)
  } else {
    classes <- c("page", classes)
  }

  structure(
    list(
      name = block$name,
      layout = "page",
      roxygen = map(block, unlist)
    ),
    class = classes
  )
}
