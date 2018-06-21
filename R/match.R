#' Fuzzy string matching
#'
#' Goal: Take (column names as) two vectors of string, return tibble with
#' matching candidates.
#'
#' @export
fuzzy_match_str <- function(x, y, method = c("word", "bigram", "stringdist"), stringdist_args = list(method = "cosine")) {
  method <- match.arg(method)

  xy <- list(x = x, y = y) %>%
    purrr::map(wordify_strings)

  if (method %in% c("bigram", "word")) {
    if (method == "bigram") {
      xy <- purrr::map(xy, ~ mutate(., ngrams = purrr::map(tokens, ~ bigrams(.))))
    } else if (method == "word") {
      xy <- purrr::map(xy, ~ mutate(., ngrams = purrr::map(tokens, count_words)))
    }
    cutoff <- switch(method,
                     word = 3,
                     bigram = 5)

    xy$x$guess <- purrr::map(xy$x$ngrams, compare_ngrams, ngram_list = xy$y$ngrams, cutoff = cutoff)
    xy$x$y_guess <- purrr::map_chr(xy$x$guess, ~ ifelse(is.na(.), NA, xy$y$string[.]))
  } else if (method == "stringdist") {
    stringdist_args$x <- x
    stringdist_args$table <- y
    xy$x$guess <- do.call(stringdist::amatch, stringdist_args)
    xy$x$y_guess <- y[xy$x$guess]
  }

  dplyr::select(xy$x, x = string, y_guess) %>%
    dplyr::filter(!is.na(y_guess))
}

wordify_strings <- function(x) {
  tibble::data_frame(
    string = x,
    tokens = strsplit(tolower(x), "\\.|_| ")
  )
}

bigrams <- function(x) {
  tau::textcnt(x, n = 2) %>%
    {data_frame(ngram = names(.), count = as.integer(.))}
}

count_words <- function(x) {
  dplyr::data_frame(ngram = x) %>%
    dplyr::group_by(ngram) %>%
    dplyr::count() %>%
    dplyr::mutate(count = n)
}

compare_ngrams <- function(ngram_single, ngram_list, cutoff = NULL) {
  # Match bigram to bigram_list, return index
  x <- purrr::map_dbl(ngram_list,
                  ~ dplyr::full_join(., ngram_single, by = "ngram") %>%
                    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("count")), function(x) ifelse(is.na(x), 0, x)) %>%
                    dplyr::mutate(count = abs(count.x - count.y)) %>%
                    dplyr::pull(count) %>% sum(na.rm = TRUE))

  if (!is.null(cutoff)) {
    if (min(x) > cutoff) return(NA)
  }
  which.min(x)
}
