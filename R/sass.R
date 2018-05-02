get_sass <- function() {
  s_dir <- path_package("hyderogen", "inst", "sass")
  dir_ls(s_dir, type = "file")
}

