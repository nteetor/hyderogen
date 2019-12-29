tag_format_ <- function(x) {
  if (is.null(x$tag)) {
    return(NULL)
  }

  formatter <- switch(
    x$tag,
    aliases = tag_aliases,
    author = tag_author,
    backref = tag_backref,
    description = tag_description,
    details = tag_details,
    docType = tag_doc_type,
    examples = tag_examples,
    export = tag_export,
    family = tag_family,
    `.formals` = tag_formals,
    format = tag_format,
    import = tag_import,
    importFrom = tag_import_from,
    include = tag_include,
    inheritParams = tag_inherit_params,
    keywords = tag_keywords,
    name = tag_name,
    param = tag_param,
    `.reexport` = tag_reexport,
    rdname = tag_rdname,
    seealso = tag_seealso,
    section = tag_section,
    title = tag_title,
    usage = tag_usage
  )

  if (is.null(formatter)) {
    print(x)
    return(NULL)
  }

  formatter(x)
}

tag_aliases <- function(tag) {
  list(
    key = "aliases",
    value = stringr::str_subset(
      stringr::str_split(tag$val, " ", simplify = FALSE)[[1]],
      "^NULL$",
      negate = TRUE
    )
  )
}

tag_author <- function(tag) {
  list(
    key = "author",
    value = tag$val
  )
}

tag_backref <- function(tag) {
  NULL
}

tag_description <- function(tag) {
  list(
    key = "description",
    value = tag$val
  )
}

tag_details <- function(tag) {
  list(
    key = "details",
    title = "Details",
    body = tag$val
  )
}

tag_doc_type <- function(tag) {
  list(
    key = "doctype",
    value = tag$val
  )
}

tag_examples <- function(tag) {
  list(
    key = "examples",
    value = tag$val
  )
}

tag_export <- function(tag) {
  list(
    key = "exported",
    value = TRUE
  )
}

tag_family <- function(tag) {
  list(
    key = "family",
    value = tag$val
  )
}

tag_formals <- function(tag) {
  list(
    key = "formals",
    value = tag$val
  )
}

tag_format <- function(tag) {
  NULL
}

tag_import <- function(tag) {
  list(
    key = "import",
    package = tag$val
  )
}

tag_import_from <- function(tag) {
  list(
    key = "importFrom",
    package = tag$val[1],
    objects = tag$val[-1]
  )
}

tag_include <- function(tag) {
  NULL
}

tag_inherit_params <- function(tag) {
  list(
    key = "inheritparams",
    value = tag$val
  )
}

tag_keywords <- function(tag) {
  list(
    key = "keywords",
    value = tag$val
  )
}

tag_name <- function(tag) {
  list(
    key = "name",
    value = tag$val
  )
}

tag_param <- function(tag) {
  list(
    key = "params",
    name = tag$val$name,
    description = tag$val$description
  )
}

tag_rdname <- function(tag) {
  list(
    key = "rdname",
    value = tag$val
  )
}

tag_reexport <- function(tag) {
  list(
    key = "reexport",
    package = tag$val$pkg,
    object = tag$val$fun
  )
}

tag_seealso <- function(tag) {
  list(
    key = "seealso",
    value = tag$val
  )
}

tag_section <- function(tag) {
  list(
    key = "sections",
    title = stringr::str_extract(tag$val, "^[^:]+"),
    body = stringr::str_remove(tag$val, "^[^:]+[:]\n\n")
  )
}

tag_title <- function(tag) {
  list(
    key = "title",
    value = tag$val
  )
}

tag_usage <- function(tag) {
  list(
    key = "usage",
    value = tag$val
  )
}
