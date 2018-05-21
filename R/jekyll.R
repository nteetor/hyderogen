#' Build a jekyll site
#'
#' The `jekyll` function builds a jekyll site from your
#' package's roxygen.
#'
#' @param pkg A character string specifying a file path to an R package,
#'   defaults to `"."`, the current directory.
#'
#' @param dir A folder path, relative to `pkg`, where the jekyll site will be
#'   built, defaults to `"docs"`.
#'
#' @details
#'
#' Collections are currently overwritten. However, if a family's name is
#' changed then the old, corresponding collection will not be deleted. This is
#' to prevent custom collections from accidentally being removed. This behaviour
#' is subject to change.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' jekyll("~/git/mypkg")
#'
#' }
#'
jekyll <- function(pkg = ".", dir = "docs") {
  base_dir <- path(pkg, dir)
  r_dir <- path(pkg, "R")

  blocks <- suppressMessages(
    roxygen2::parse_package(pkg, registry = tag_defaults())
  )
  pages <- map(blocks, as_page)

  families <- unique(flatten_chr(map(pages, c("roxygen", "family"))))

  firsts <- map_int(families, function(f) detect_index(pages, ~ .$roxygen$family == f))

  pages[firsts] <- map(pages[firsts], ~ {
    .$redirect_from <- glue("/docs/{ desc::desc_get('Version', file = pkg) }/{ .$roxygen$family }/")
    .
  })

  collections <- as.yaml(list(collections = set_names(
    map(families, ~ list(
      output = TRUE,
      permalink = glue("/docs/{ desc::desc_get('Version', file = pkg) }/:collection/:path")
    )),
    families
  )))

  children <- set_names(
    map(families, ~ unique(map_chr(
      keep(pages, function(p) identical(p$roxygen$family, .)),
      page_name
    ))),
    families
  )

  defaults <- as.yaml(list(defaults = map(families, function(f) list(
    scope = list(path = "", type = f),
    values = list(
      default_page = glue("/docs/{ desc::desc_get('Version', file = pkg) }/{ f }/{ slugify(children[[f]][[1]]) }"),
      sections = map(children[[f]], ~ list(
        name = .,
        slug = slugify(.)
      ))
    )
  ))))

  layouts <- map(get_layouts(), layout)

  if (dir_exists(base_dir)) {
    file_delete(dir_ls(base_dir))
  }
  dir_create(base_dir)

  with_dir(base_dir, {
    message("Writing pages")
    walk(pages, write_out)

    message("Writing includes")
    walk(get_includes(), ~ write_out(include(.)))

    message("Writing templates")
    walk(get_templates(), ~ write_out(template(.)))

    message("Writing layouts")
    walk(get_layouts(), ~ write_out(layout(.)))

    message("Writing scss")
    dir_create("assets/css", recursive = TRUE)
    dir_create("_sass")
    cat0(
      "---",
      "---",
      "",
      map_chr(get_sass(), ~ glue("@import '{ path_ext_remove(path_file(.)) }';")),
      sep = "\n",
      file = path("assets", "css", "main", ext = "scss")
    )
    walk(get_sass(), ~ file_copy(., path("_sass", path_file(.))))
  })

  invisible()
}
