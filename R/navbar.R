# @return An function that generates the navbar given the depth beneath
#   the docs root directory
data_navbar <- function(pkg = ".", depth = 0L) {
  pkg <- as_pkgdown(pkg)

  default <- default_navbar(pkg)

  navbar <- list(
    title = pkg$meta$navbar$title %||% default$title,
    type = pkg$meta$navbar$type %||% default$type,
    items = pkg$meta$navbar$items %||% default$items
  )

  navbar$items <- render_navbar_links(navbar$items, depth = depth)

  print_yaml(navbar)
}

render_navbar_links <- function(x, depth = 0L) {
  stopifnot(is.integer(depth), depth >= 0L)

  tweak <- function(x) {
    if (!is.null(x$menu)) {
      x$menu <- lapply(x$menu, tweak)
      x
    } else if (!is.null(x$href) && !grepl("://", x$href, fixed = TRUE)) {
      x$href <- paste0(up_path(depth), x$href)
      x
    } else {
      x
    }
  }

  if (depth != 0L) {
    x <- lapply(x, tweak)
  }

  navbar_links_html(x)
}

navbar_links_html <- function(x) {

}

navbar_dropdown_item <- function(x) {

}

navbar_icon_item <- function(x) {

}

navbar_text_item <- function(x) {
  paste0("<li class='nav-item'>",
        "<a class='nav-link' href='", x$href, "'>",
        x$text, "</a>",
        "</li>")
}

# Default navbar ----------------------------------------------------------

default_navbar <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)

  items <- list()

  items$home <- list(
    icon = "fa-home fa-lg",
    href = "index.html",
    align = "left"
  )

  vignettes <- pkg$vignettes
  pkg_intro <- vignettes$name == pkg$package
  if (any(pkg_intro)) {
    intro <- vignettes[pkg_intro, , drop = FALSE]
    vignettes <- vignettes[!pkg_intro, , drop = FALSE]

    items$intro <- list(
      text = "Get Started",
      href = paste0("articles/", intro$file_out),
      align = "left"
    )
  }

  items$reference <- list(
    text = "Reference",
    href = "reference/index.html",
    align = "left"
  )

  if (nrow(vignettes) > 0) {
    articles <- purrr::map2(
      vignettes$title, vignettes$file_out,
      ~ list(text = .x, href = paste0("articles/", .y)))

    items$articles <- list(
      text = "Articles",
      menu = articles,
      align = "left"
    )
  }

  if (has_news(pkg$path)) {
    items$news <- list(
      text = "News",
      href = "news/index.html",
      align = "left"
    )
  }

  items$gh <- purrr::flatten(list(
    github_link(pkg$path),
    align = "right"
  ))

  print_yaml(list(
    title = pkg$package,
    type = "default",
    items = unname(items)
  ))
}

github_link <- function(path = ".") {
  desc <- read_desc(path)

  if (!desc$has_fields("URL"))
    return()

  gh_links <- desc$get("URL")[[1]] %>%
    strsplit(",") %>%
    `[[`(1) %>%
    trimws()
  gh_links <- grep("^https?://github.com/", gh_links, value = TRUE)

  if (length(gh_links) == 0)
    return()

  list(
    icon = "fa-github fa-lg",
    href = gh_links[[1]]
  )
}
