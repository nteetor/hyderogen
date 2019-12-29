site_collections <- function(blocks) {
  all_tags <- purrr::flatten(purrr::map(blocks, "tags"))

  family_tags <- purrr::keep(all_tags, ~ .x$tag == "family")

  families <- sort(unique(purrr::map_chr(family_tags, "val")))
  names(families) <- stringr::str_replace_all(families, " ", "-")

  purrr::map(stringr::str_to_title(families), ~ {
    list(
      output = TRUE,
      title = .x
    )
  })
}
