capture_tibble_print <- function(x, exclude = c(-1, -3), underline = FALSE) {
  if (underline) exclude <- setdiff(exclude, -3)
  x <- capture.output(tibble:::print.tbl(x))
  if (underline) {
    x[3] <- gsub("\\*", "", x[3])
    x[3] <- gsub("[^ ]", "-", x[3])
  }
  strip_first <- regexec("\\d ", x[-1:-3]) %>%
    purrr::map_int(~.) %>%
    {
      sort(table(.), decreasing = TRUE)
    } %>%
    .[1] %>%
    names() %>%
    as.integer()
  x <- substring(x, strip_first + 2)
  x <- paste0("  ", x)
  x <- x[exclude]
  paste0("  ", x, collapse = "\n")
}

add_subclass <- function(x, subclass) {
  class(x) <- c(subclass, class(x))
  x
}

remove_subclass <- function(x, subclass) {
  class(x) <- setdiff(class(x), subclass)
  x
}

abort <- function(..., .envir = parent.frame(), .subclass = "NULL") {
  rlang::abort(glue::glue(..., .envir = .envir), .subclass = .subclass)
}

warn <- function(..., .envir = parent.frame(), .subclass = "NULL") {
  rlang::warn(glue::glue(..., .envir = .envir), .subclass = .subclass)
}

inform <- function(..., .envir = parent.frame()) {
  rlang::inform(glue::glue(..., .envir = .envir))
}
