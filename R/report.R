#' Differences Report
#'
#' Creates an HTML report for exploring the differences between two data frames
#' or tibbles. If `outfile` is specified, the report is saved as a standalone
#' HTML file that can be shared with others.
#'
#' @param x `<diff_tbl|tbl>` `diff_tbl` output from [diff_compare()] or the
#'   reference data frame or tibble.
#' @param .x `<tbl>` Original reference data frame or tibble, provided only if
#'   `x` is a `diff_tbl`.
#' @param .y `<tbl>` Comparison data frame or tibble
#' @param df_names `<chr>` Vector of length two containg the names of the data
#'   frames that should be displayed in the report
#' @param outfile `<chr>` Filename (with optional path) to which report should
#'   be saved. By default writes to temp file and opens in RStudio Viewer pane.
#' @param keep_original `<lgl>` Should the original data frames be included in
#'   the report?
#' @param use_plotly `<lgl>` Should the differences visualization be interactive
#'   via [plotly]? By default this is `FALSE` when saving the report to a
#'   standalone file.
#' @param use_DT `<lgl>` Should the tables reporting differences be presented as
#'   an interactive datatable with the [DT] package? By default this is `FALSE`
#'   when saving the report to a standalone file.
#' @inheritParams rmarkdown::render
#' @inheritDotParams diff_compare exclude group_vars align tolerance
#' @return Invisibly returns path to rendered differences report
#' @export
diff_report <- function(
  x,
  df_names = NULL,
  outfile = NULL,
  keep_original = FALSE,
  use_plotly = is.null(outfile),
  use_DT = is.null(outfile),
  quiet = TRUE,
  ...,
  .y = NULL
) {
  UseMethod("diff_report")
}

#' @method diff_report data.frame
diff_report.data.frame <- function(.x, .y, df_names = NULL, ...) {
  if (is.null(.y)) {
    rlang::abort("A comparison data.frame must be provided as `.y`")
  }
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  args_compare <- list(x = .x, y = .y, df_names = df_names, ...)
  args_compare <- args_compare[intersect(names(args_compare), names(formals(diff_compare)))]
  df_diff <- do.call(diff_compare, args_compare)
  diff_report(df_diff, ..., .x = .x, .y = .y)
}

#' @method diff_report diff_tbl
diff_report.diff_tbl <- function(
  x,
  outfile = NULL,
  keep_original = FALSE,
  use_plotly = is.null(outfile),
  use_DT = is.null(outfile),
  quiet = TRUE,
  ...,
  .x = NULL,
  .y = NULL
) {
  specified_destination <- !is.null(outfile)
  outfile <- outfile %||% tempfile(fileext = ".html")
  outdir <- dirname(outfile)
  outfile <- basename(outfile)

  rpt <- rmarkdown::render(
    input = system.file("report.Rmd", package = "different"),
    output_file = outfile,
    output_dir = outdir,
    params = list(df_diff = z,
                  use_plotly = use_plotly,
                  use_DT = use_DT,
                  df_orig = if (keep_original) list(x = .x, y = .y)
    ),
    quiet = quiet
  )
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(rpt)
    if (specified_destination) {
      cli::cat_line("Output saved to ", file.path(outdir, outfile))
    }
  } else {
    cli::cat_line("Cannot open viewer via RStudio. Report was saved to ", file.path(outdir, outfile))
  }
  invisible(rpt)
}
