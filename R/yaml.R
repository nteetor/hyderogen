as_yaml <- function(x, ...) {
  UseMethod("as_yaml")
}

as_yaml.roxy_block <- function(x, ...) {
  yaml::as.yaml(x)
}

