blocks_get <- function(path) {
  e <- rlang::env()

  roxygen2::parse_package(path, e)
}

blocks_process <- function(blocks) {
  blocks_tags <- purrr::map(blocks, `[[`, "tags")

  names(blocks_tags) <- purrr::map(blocks, c("object", "topic"))

  blocks_tags
}

blocks_format <- function(blocks) {
  purrr::map(blocks, function(block) {
    format_tags <- tags_format(block)

    gather_tags <- tags_gather(format_tags)

    unpack_tags <- tags_unpack(gather_tags)

    unpack_tags
  })
}

tags_format <- function(block) {
  purrr::map(block, tag_format_)
}

tags_gather <- function(block) {
  list_gather(block, "key")
}

tags_unpack <- function(block) {
  list_unpack(purrr::map(block, list_unpack))
}

blocks_expand <- function(blocks) {
  params_exp <- blocks_expand_params(blocks)

  params_exp
}

blocks_expand_params <- function(blocks) {
  purrr::map(blocks, function(block) {
    if (is.null(block$inheritparams)) {
      return(block)
    }

    srcs <- purrr::simplify(block$inheritparams)

    inherited <- unlist(
      purrr::map(blocks[srcs], "params"),
      recursive = FALSE,
      use.names = FALSE
    )

    block$params <- c(inherited, block$params)

    block
  })
}

blocks_nest <- function(blocks) {
  purrr::map(blocks, ~ list(roxygen = .x))
}

pages_yml <- function(pages) {
  purrr::map(pages, ymlthis::as_yml)
}

build_pages <- function(path) {
  blocks <- blocks_get(path)

  simple_blocks <- blocks_process(blocks)

  formatted_blocks <- blocks_format(simple_blocks)

  expanded_blocks <- blocks_expand(formatted_blocks)

  nested_blocks <- blocks_nest(expanded_blocks)


}
