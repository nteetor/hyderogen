parse_files <- function(dir = NULL, files = NULL) {
  if (!is.null(dir) && !dir_exists(dir)) {
    stop("`dir` is specified, but path does exist")
  }

  files <- unique(c(dir_ls(dir), files))

  blocks <- flatten(lapply(files, parse_file))
  blocks <- map(blocks, set_block_name)
  blocks <- clean_blocks(blocks)

  docs <- make_documents(blocks)

  colls <- make_collections(docs)

  structure(
    list(
      collections = colls,
      pages = NULL
    ),
    class = "jekyll"
  )
}

parse_file <- function(file) {
  roxygen2::parse_file(file)
}

set_block_name <- function(block) {
  nm <- get_block_name(block)
  block$rdname <- NULL
  block$name <- nm
  block
}

get_block_name <- function(block) {
  if (!is.null(block$rdname)) {
    block$rdname
  } else if (!is.null(block$name)) {
    block$name
  } else if (!is.null(attr(block, "object"))) {
    attr(block, "object")$name
  } else {
    NULL
  }
}

# drop blocks without names and join blocks with the same name
clean_blocks <- function(blocks) {
  all_names <- flatten_chr(map(blocks, "name"))

  if (!any(duplicated(all_names))) {
    return(blocks)
  }

  map(unique(all_names), join_blocks, blocks)
}

join_blocks <- function(name, blocks) {
  filtered <- keep(blocks, ~ identical(.$name, name))

  all_tags <- unique(unlist(map(filtered, names)))

  this <- map(
    all_tags,
    function(nm) compact(map(filtered, nm))
  )

  names(this) <- all_tags

  this
}
