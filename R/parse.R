make_project <- function(dir) {
  if (!dir_exists(dir)) {
    stop(
      "`make_project()`, `dir` does not exist",
      call. = FALSE
    )
  }

  r_dir <- path(dir, "R")

  if (!dir_exists(r_dir)) {
    stop(
      "`make_project()`, no R/ directory found",
      call. = FALSE
    )
  }

  blocks <- flatten(map(dir_ls(r_dir), parse_file))
  blocks <- map(blocks, set_block_name)
  blocks <- clean_blocks(blocks)

  pkg <- desc::desc(dir)
  docs <- make_documents(blocks)
  colls <- make_collections(docs)

  matter <- list(
    output = TRUE,
    permalink = glue("/docs/{ pkg$get('Version') }/:collection/:path")
  )
  colls <- map(colls, list_modify, !!!matter)

  structure(
    list(
      package = list(
        name = pkg$get("Package"),
        version = pkg$get("Version"),
        title = pkg$get("Title"),
        description = gsub("[\\n\\s]+", " ", pkg$get("Description"), perl = TRUE)
      ),
      username = system("git config user.name", intern = TRUE),
      collections = colls,
      templates = names(.templates),
      includes = names(.includes),
      layouts = names(.layouts)
    ),
    class = c("jekyll", "list")
  )
}

parse_file <- function(file) {
  roxygen2::parse_file(file)
}

set_block_name <- function(block) {
  nm <- if (!is.null(block$rdname)) {
    block$rdname
  } else if (!is.null(block$name)) {
    block$name
  } else if (!is.null(attr(block, "object"))) {
    attr(block, "object")$name
  } else {
    NULL
  }

  block$rdname <- NULL
  block$name <- nm

  block
}

# drop blocks without names and join blocks with the same name
clean_blocks <- function(blocks) {
  all_names <- flatten_chr(map(blocks, "name"))

  if (!any(duplicated(all_names))) {
    return(blocks)
  }

  joined <- map(unique(all_names), join_blocks, blocks)

  map(joined, ~ update_list(., name = unique(unlist(.$name))))
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
