metayer
====

metayer provides miscellaneous tools useful for package development.  In particular, it currently attempts to improve
the experience of writing vignettes in jupyter lab and across publishing contexts.  It anticipates the need to retrofit
scripts by folding `cli` notifications into `logger` log files.  It offers a simple, hierarchical storage container.
And, lastly, it introduces some additional utility functions for inspecting environments.

vignettes
----

jupyter lab provides a convenient way to author vignette / notebook style documents.  It can be particularly useful in a
remote context.  However, the integration with R can be awkward.  
[rmarkdown::convert_ipynb](https://pkgs.rstudio.com/rmarkdown/reference/convert_ipynb.html) offers a simple converter
with "rough edges" but it misses an opportunity to configure the YAML section of the converted document.  This means the
user is expected to manage metadata for what is now, in this pipeline context, an intermediate Rmd file.

Trying to get visually similar output while authoring in jupyter and rendering vignettes in HTML or building articles in
pkgdown can also be frustrating.

In jupyter:

![jupyter tibble](docs/images/jupyter_tibble.png)

After `rmarkdown::convert_ipynb` and `rmarkdown::render`:

![jupyter tibble](docs/images/rmarkdown_tibble.png)

After `pkgdown::build_article`:


