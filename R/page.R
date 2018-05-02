as_page <- function(x, ...) {
  UseMethod("as_page")
}

as_page.roxy_block <- function(x, ...) {
  params <- compact(imap(x, ~ if (.y == "param") .x))
  params <- if (length(params)) set_names(params, NULL)

  sections <- set_names(
    compact(imap(x, ~ {
      if (.y == "section") strsplit(.x, "\n\n", fixed = TRUE)[[1]]
    })),
    NULL
  )

  examples <- if (!is.null(x$examples)) {
    gsub("^\n+", "", strsplit(x$examples, "\n{3,}", perl = TRUE)[[1]])
  }

  blk_call <- attr(x, "call", exact = TRUE)
  blk_name <- x$name %||% as.character(blk_call[[2]])
  blk_rdname <- x$rdname
  blk_family <- x$family

  # figure out the relative path to write out the page
  if (identical(blk_name, "reexports")) {
    page_path <- NULL
  } else if (blk_call == "_PACKAGE") {
    page_path <- path("package", ext = "md")
  } else if (!is.language(blk_call)) {
    page_path <- path(if (is.null(blk_name)) blk_name else blk_rdname, ext = "md")
  } else if (is.language(blk_call)) {
    f_name <- if (length(blk_call) == 1) blk_call else blk_call[[2]]
    if (!is.null(blk_family)) {
      page_path <- path("collections", paste0("_", blk_family), f_name, ext = "md")
    } else {
      page_path <- path("collections", "_misc", f_name, ext = "md")
    }
  } else {
    stop("could not create filename")
  }

  # slugify the base file name
  if (!is.null(page_path)) {
    page_path <- path(
      path_dir(page_path),
      slugify(path_file(page_path))
    )
  }

  blk_call <- attr(x, "call", exact = TRUE)
  if (!is.null(blk_call)) {
    blk_call <- paste(deparse(blk_call), collapse = "\n")
  }

  structure(
    list(
      layout = "page",
      slug = if (!is.null(page_path)) path_ext_remove(path_file(page_path)),
      roxygen = list(
        rdname = x$rdname,
        name = blk_name,
        doctype = x$docType,
        title = x$title,
        description = x$description,
        parameters = params,
        sections = if (length(sections)) sections,
        examples = examples,
        aliases = x$aliases,
        family = x$family,
        export = !is.null(x$export),
        filename = path_file(attr(x, "filename", TRUE)),
        source = blk_call
      )
    ),
    class = "page",
    path = page_path
  )
}

tag_examples <- function(x) {
  # all the error checking withtout touching the whitespace, need to preserve
  # newlines to split examples for front matter
  roxygen2::tag_examples(x)
  x
}

tag_defaults <- function() {
  list(
    aliases = tag_value,
    author = tag_markdown,
    backref = tag_value,
    concept = tag_markdown,
    describeIn = tag_name_description,
    description = tag_markdown,
    details = tag_markdown,
    docType = tag_name,
    encoding = tag_value,
    evalRd = tag_code,
    example = tag_value,
    examples = tag_examples,
    family = tag_value,
    field = tag_name_description,
    format = tag_markdown,
    inherit = tag_inherit,
    inheritParams = tag_value,
    inheritDotParams = tag_two_part("source", "args", required = FALSE),
    inheritSection = tag_name_description,
    keywords = tag_value,
    method = tag_words(2, 2),
    name = tag_value,
    md = tag_toggle,
    noMd = tag_toggle,
    noRd = tag_toggle,
    note = tag_markdown,
    param = tag_name_description,
    rdname = tag_value,
    rawRd = tag_value,
    references = tag_markdown,
    return = tag_markdown,
    section = tag_markdown,
    seealso = tag_markdown,
    slot = tag_name_description,
    source = tag_markdown,
    template = tag_value,
    templateVar = tag_name_description,
    title = tag_markdown_restricted,
    usage = tag_value,
    # some other tags ----
    evalNamespace = tag_code,
    export = tag_words_line,
    exportClass = tag_words(1),
    exportMethod = tag_words(1),
    exportPattern = tag_words(1),
    import = tag_words(1),
    importClassesFrom = tag_words(2),
    importFrom = tag_words(2),
    importMethodsFrom = tag_words(2),
    rawNamespace = tag_code,
    S3method = tag_words(2, 2),
    useDynLib = tag_words(1)
  )
}
