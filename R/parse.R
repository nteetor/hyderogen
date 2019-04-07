#' @importFrom purrr map map_if map2 map_chr keep flatten_chr accumulate %@@%
#' @importFrom dplyr group_by summarise mutate filter lag
#' @importFrom sourcetools tokenize_string

parse_package <- function(path = ".") {
  base_path <- normalizePath(path)
  pkg_env <- pkgload::load_all(
    path,
    compile = FALSE,
    helpers = FALSE,
    attach_testthat = FALSE,
    quiet = TRUE
  )$env

  # create blocks
  raw_blocks <- roxygen2::parse_package(base_path, env = NULL)
  options <- roxygen2:::load_options(base_path)
  blocks <- map(raw_blocks, roxygen2:::process_templates, base_path, options)

  # prep blocks
  blocks <- normalize_parameters(blocks)
  blocks <- normalize_sections(blocks)
  blocks <- normalize_rdnames(blocks)
  blocks <- normalize_names(blocks)

  blocks <- drop_empty(blocks)
  blocks <- normalize_links(blocks)
  blocks <- parse_examples(blocks, pkg_env)
  blocks <- sort_names(blocks)

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
      } else if (!is.null(block %@% "call")) {
        if (length(block %@% "call") == 1) {
          block[["name"]] <- as.character(block %@% "call")
        } else {
          block[["name"]] <- as.character((block %@% "call")[[2]])
        }
      }
    }

    block[["rdname"]] <- block[["name"]]
    block
  })
}

normalize_links <- function(blocks) {
  families <- map(blocks, "family")
  names(families) <- map_chr(blocks, "rdname")
  families <- keep(families, ~ length(.) && nchar(.))
  families <- map_if(families, ~ . != "", ~ paste0(., "/"))

  map(blocks, function(block) {
    block[["parameters"]] <- map(
      block[["parameters"]],
      function(param) {
        param[["description"]] <- replace_links(param[["description"]], families)
        param
      }
    )

    block[["sections"]] <- map(
      block[["sections"]],
      function(section) {
        section[["body"]] <- replace_links(section[["body"]], families)
        section
      }
    )

    block[["examples"]] <- replace_links(block[["examples"]], families)

    block
  })
}

replace_links <- function(x, families) {
  if (!length(x) || !grepl("[^[]\\[[^]]+\\]", x)) {
    return(x)
  }

  x <- stringr::str_replace_all(
    x,
    "([^\\[])\\[([a-zA-Z][a-zA-Z0-9]*)\\]",
    '\\1[\\2]({{ families[["\\2"]] %||% "" }}{{ tolower("\\2") }}.html)'
  )

  x <- stringr::str_replace_all(
    x,
    "([^\\[])\\[([a-zA-Z][a-zA-Z0-9]*)\\(\\)\\]",
    '\\1[\\2()]({{ families[["\\2"]] %||% "" }}{{ tolower("\\2") }}.html)'
  )

  glue(x, .open = "{{", .close = "}}")
}

clean_rd_comments <- function(x) {
  gsub("(^|\\n)\\\\%", "\\1#.", x)
}

parse_examples <- function(blocks, env) {
  map(blocks, function(block) {
    if (!is.null(block[["examples"]])) {
      tokens <- tokenize_string(clean_rd_comments(block[["examples"]]))

      if (!all(tokens$type == "whitespace")) {
        block_env <- new.env(parent = env)

        tokens_by_row <- summarise(
          group_by(tokens, row),
          value = glue_collapse(value),
          type = list(unique(type))
        )

        tokens_simple_types <- mutate(
          tokens_by_row,
          type = purrr::map2_chr(type, value, ~ {
            if (length(.x) > 1)
              "code"
            else if (.x == "bracket")
              "code"
            else if (grepl("^[#]{2,}", .y))
              "title"
            else if (grepl("^[#][.]", .y))
              "literal"
            else
              .x
          })
        )

        tokens_indexed_types <- mutate(
          tokens_simple_types,
          index = accumulate(lag(type, 1, "") != type, `+`)
        )

        tokens_collapse_types <- summarise(
          group_by(tokens_indexed_types, index),
          type = type[1],
          value = glue_collapse(value)
        )

        tokens_by_section <- mutate(
          filter(tokens_collapse_types, type != "whitespace"),
          section = accumulate(type == "title", `+`),
          index = NULL
        )

        tokens_clean_values <- mutate(
          tokens_by_section,
          value = sub("\n+$", "", value),
          value = gsub("(^[#]+[.]?\\s|\\n[#][.]?)", "", value),
          value = gsub("\\\\%", "%", value)
        )

        tokens_sectioned <- summarise(
          group_by(tokens_clean_values, section),
          title = value[type == "title"],
          body = list(
            map2(value[type != "title"], type[type != "title"], function(value, type) {
              list(
                type = if (type %in% c("code", "literal")) type else "text",
                content = if (type == "literal")
                  glue(value, .open = "{{", .close = "}}")
                else
                  value,
                output = if (type == "code") {
                  as.character(eval(parse(text = value), envir = block_env))
                }
              )
            })
          )
        )

        block[["examples"]] <- purrr::pmap(
          tokens_sectioned,
          function(section, title, body) {
            list(
              title = title,
              body = body
            )
          }
        )

      }
    }

    block
  })
}

sort_names <- function(blocks) {
  map(blocks, function(block) {
    if (!("name" %in% names(block))) {
      return(block)
    }

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
  keep(blocks, function(block) !is.null(block[["name"]]))
}
