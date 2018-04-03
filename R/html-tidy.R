#' @importFrom htmltidy tidy_html
html_tidy <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)

  html_paths <- dir_ls(pkg$dst_path, recursive = TRUE, glob = "*.html")
}

tidy_file <- function(src) {
  doc <- xml2::read_html(src)
  <- tidy_file(doc)

}
