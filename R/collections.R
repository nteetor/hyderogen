is_collection <- function(x) {
  inherits(x, "collection")
}

make_collections <- function(docs) {
  if (!is.list(docs) && !all(map_lgl(docs, is_document))) {
    stop("expecting list of documents")
  }

  coll_names <- unlist(map(docs, c("roxygen", "family")))

  coll_docs <- map(coll_names, as_collection, docs)

  coll_docs
}

as_collection <- function(name, docs) {
  structure(
    list(
      name = name,
      items = keep(docs, ~ identical(.$roxygen$family, name))
    ),
    class = "collection"
  )
}
