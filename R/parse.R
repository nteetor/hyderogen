#' @importFrom purrr map keep flatten_chr %@@%

parse_package <- function(path = ".") {
  base_path <- normalizePath(path)

  # create blocks
  raw_blocks <- roxygen2::parse_package(base_path)
  options <- roxygen2:::load_options(base_path)
  blocks <- map(raw_blocks, roxygen2:::process_templates, base_path, options)

  # prep blocks
  blocks <- normalize_parameters(blocks)
  blocks <- normalize_sections(blocks)
  blocks <- normalize_rdnames(blocks)
  blocks <- normalize_names(blocks)
  blocks <- sort_names(blocks)

  blocks <- drop_empty(blocks)

  blocks
}

normalize_parameters <- function(blocks) {
  map(blocks, function(block) {
    if (!("param" %in% names(block))) {
      return(block)
    }

    is_param <- which(names(block) == "param")

    params <- block[is_param]
    names(params) <- NULL
    block[is_param[1]] <- list(params)
    names(block)[is_param[1]] <- "parameters"

    if (length(is_param) > 1) {
      block[is_param[-1]] <- NULL
    }

    block
  })
}

normalize_sections <- function(blocks) {
  map(blocks, function(block) {
    if (!any(names(block) == "section")) {
      return(block)
    }

    is_section <- which(names(block) == "section")

    sections <- map(block[is_section], function(section) {
      pieces <- as.list(strsplit(section, ":\n\n", fixed = TRUE)[[1]])
      names(pieces) <- c("title", "body")
      pieces
    })
    names(sections) <- NULL

    block[is_section[1]] <- list(sections)
    names(block)[is_section[1]] <- "sections"

    if (length(is_section) > 1) {
      block[is_section[-1]] <- NULL
    }

    block
  })
}

normalize_rdnames <- function(blocks) {
  rdname_map <- unique(flatten_chr(map(blocks, "rdname")))
  names(rdname_map) <- rdname_map
  rdname_map <- as.list(rdname_map)

  for (block in blocks) {
    block_call <- block %@% "call"

    if (is.language(block_call)) {
      block_name <- as.character(block_call[[2]])

      if (block_name %in% names(rdname_map)) {
        rdname_map[[block_name]] <- block
      }
    }
  }

  blocks <- map(blocks, function(block) {
    block_rdname <- block[["rdname"]]

    if (!is.null(block[["name"]]) || is.null(block_rdname)) {
      return(block)
    }

    if (block_rdname %in% names(rdname_map)) {
      block_replaced <- rdname_map[[block_rdname]]

      attr(block_replaced, "filename") <- block %@% "filename"
      attr(block_replaced, "location") <- block %@% "location"
      attr(block_replaced, "call") <- block %@% "call"
      attr(block_replaced, "object") <- block %@% "object"

      block_replaced
    }
  })

  blocks
}

normalize_names <- function(blocks) {
  map(blocks, function(block) {
    if (is.null(block[["name"]])) {
      if (!is.null(block %@% "object")) {
        block[["name"]] <- (block %@% "object")[["alias"]]
      }
    }

    block
  })
}

sort_names <- function(blocks) {
  map(blocks, function(block) {
    # reorder block pieces
    block_attrs <- attributes(block)
    block_names <- names(block)
    block <- block[c("name", block_names[block_names != "name"])]

    # shuffle names in attributes, set attributes
    block_attrs[["names"]] <- names(block)
    attributes(block) <- block_attrs

    block
  })
}

drop_empty <- function(blocks) {
  keep(blocks, function(block) {
    !is.null(block[["name"]]) || !is.null(block %@% "object")
  })
}
