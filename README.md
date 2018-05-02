# hyderogen

![stability-wip](https://img.shields.io/badge/stability-work_in_progress-blue.svg)


Turn your existing roxygen documentation into a jekyll site.

## How it works

- [x] `@family`s are converted into collections
- [ ] bundle together docs with the same `@name`/`@rdname`
- [x] docs without a `@family` are added to the `misc` collection
- [x] generates default jekyll folder structure
- [x] applies bootstrap-based stylings
- [x] the majority of the site lives at `<url>/<pkg name>/docs/<pkg version>/`

```R
# remotes::install_github("nteetor/hyderogen")

library(hyderogen)
jekyll("<path to pkg>")

browseURL("http://127.0.0.1:4000/<pkg name>/docs/<pkg version>/")
```

## Why?

It's a toolset useful to me and I happen to like jekyll.
