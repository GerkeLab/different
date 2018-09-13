#' Pull Out a Variable
#'
#' @inheritParams diff_report
#' @param var `<chr>` Vector of variable names
#' @param regexp `<chr>` Regular expression used as a filter to find variable
#'   names. If provided, this filter is applied first, and only columns matching
#'   the `regexp` and appearing in the `var` list are included.
#' @export
diff_pull <- function(x, var = NULL, regexp = NULL) UseMethod("diff_pull")

#' @export
diff_pull.diff_tbl <- function(x, var = NULL, regexp = NULL) {
  if (is.null(var) & is.null(regexp)) stop("Please provide one of `var` or `regexp`")
  vars <- x$variable
  if (!is.null(regexp)) vars <- grep(regexp, vars, perl = TRUE, value = TRUE)
  if (!is.null(var))    vars <- intersect(vars, var)
  if (!length(vars)) abort("No variables match the input criteria.")
  x_value <- sym(paste0("value.", metadata(x, "name")["x"]))
  y_value <- sym(paste0("value.", metadata(x, "name")["y"]))
  v <- x %>%
    filter(variable %in% vars) %>%
    split(.$variable) %>%
    purrr::map(~ tidyr::unnest(.)) %>%
    purrr::map(~ select(., -starts_with("type"), -n_diff, -starts_with("miss"))) %>%
    purrr::map(~ rename(., !!x_value := value.x, !!y_value := value.y))

  if (length(v) == 1) v[[1]] else v
}
