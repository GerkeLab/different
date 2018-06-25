#' Differences Report
#'
#' @export
report <- function(
  reference,
  comparison,
  df_names = NULL,
  outfile = NULL,
  use_plotly = TRUE,
  ...
) {
  df_names <- df_names %||% paste(sys.call())[2:3]

  specified_destination <- !is.null(outfile)
  outfile <- outfile %||% tempfile(fileext = ".html")
  outdir <- dirname(outfile)
  outfile <- basename(outfile)

  df_diff <- tidy_diff(reference, comparison, df_names = df_names, ...)

  rpt <- rmarkdown::render(
    input = system.file("report.Rmd", package = "different"),
    output_file = outfile,
    output_dir = outdir,
    params = list(df_diff = df_diff, use_plotly = use_plotly)
  )
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(rpt)
    if (specified_destination) {
      cli::cat_line("Output saved to ", file.path(output_dir, output_file))
    }
  } else {
    cli::cat_line("Cannot open viewer via RStudio. Report was saved to ", file.path(output_dir, output_file))
  }
}
