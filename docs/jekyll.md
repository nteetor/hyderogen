---
name: jekyll
title: Jekyll folder structure
description: |-
  Create a jekyll site structure from a package's roxygen. Optionally, a
  site may be built, too. Building your site requires
  [jekyll](https://jekyllrb.com/docs/installation/) to be installed and
  available on your system.
parameters:
- name: path
  description: |-
    A file path specifying the folder of a package, defaults to
    `"."`.
- name: dir
  description: |-
    A file path, relative to `path`, specifying the destination of the
    jekyll folders and files, defaults to `"docs/"`.
- name: build
  description: |-
    One of `TRUE` or `FALSE` specifying if the jekyll site is built
    after creating the folder structure, defaults to `FALSE`.
- name: config
  description: |-
    A file path specifying a config file to overwrite build
    parameters, defaults to `NULL`. See
    [here](https://jekyllrb.com/docs/configuration/default/) for information
    about jekyll config paramters.
export: ''
layout: doc
---
