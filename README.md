# hyderogen

![stability-wip](https://img.shields.io/badge/stability-work_in_progress-blue.svg)

Turn your roxygen documentation into a jekyll site.

## How it works

- [x] one page per package function or object or named section
- [x] example sections support a simple templating system for readable docs
  offline and online
- [x] generates folder structure under `docs/` and builds site at `docs/_site/`
- [x] simple sidebar layout, styled with Bootstrap

```R
# remotes::install_github("nteetor/hyderogen")

library(hyderogen)

# WARNING ----
# In its current state this function will delete any existing files
# in your docs/ folder!!
jekyll("<path to pkg>", build = TRUE)
```

## Examples section templating

```R
#' @examples
#'
#' ### This is converted into a title
#'
#' # This is converted into a paragraph.
#' # This is in the same paragraph.
#'
#' div("hello, world")
#'
#' % <div>another div</div><!--this is dropped in verbatim-->
#'
#' # A final pararaph.
```

Code blocks, the `div("hello, world")`, are evaluated such that the code and
output are both shown.

## Why?

It's a toolset useful to me and I like jekyll.
