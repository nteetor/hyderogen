as_yaml <- function(x, ...) {
  UseMethod("as_yaml")
}

as_yaml.roxy_block <- function(x, ...) {
  yaml::as.yaml(x)
}

as_yaml.list <- function(x, ..., indent = 2) {
  yaml::as.yaml(x, indent = indent)
}
