#' Compare Two Data Frames
#'
#' Finds row-wise differences in two data frames that are assumed to be
#' pre-arranged.
#'
#' @param x `<tbl|df>` Reference data frame
#' @param y `<tbl|df>` Comparison data frame
#' @param exclude `<chr:NULL>` ID column(s) that are excluded from the
#'   differencing
#' @param keys `<chr:ignore>` Column names of grouping variables for
#'   alignment, takes default from `ignore`.
#' @param align `<lgl:FALSE>` Should alignmment be performed based on the
#'   grouping variables or the `group_vars` parameter?
#' @param df_names `<chr:NULL>` Alternative names for the provided dataframes as
#'   a vector of length two, to be used during printing and reporting.
#' @param tolerance `<dbl>` The tolerance to be used for comparison of numerical
#'   values, defaults to `.Machine$double.eps`.
#' @param plain `<lgl:FALSE>` If `TRUE`, returns a nested tibble of differences
#'   without converting these differences to a `diff_tbl` object.
#' @return Object of class `diff_tbl` that can be printed via `print(diff_obj)`
#'   or plotted with [ggplot2] via `plot(diff_obj)`. Use `summary(diff_obj)` to
#'   view a summary of the differences found between `x` and `y`.
#' @export
diff_compare <- function(
  x,
  y,
  df_names = NULL,
  exclude = NULL,
  keys = exclude,
  align = FALSE,
  tolerance = .Machine$double.eps,
  plain = FALSE
) {
  # x <- arrange(x, .id)
  if (is.null(df_names)) {
    df_names <- c(x = "",  y = "")
    df_names["x"] = quo_name(enquo(x))
    df_names["y"] = quo_name(enquo(y))
  } else {
    if (is.null(names(df_names))) {
      names(df_names) <- c("x", "y")
    } else if (!identical(sort(names(df_names)), c("x", "y"))) {
      warn("`df_names` is not named with 'x' and 'y', assuming they appear in that order")
      names(df_names) <- c("x", "y")
    }
  }

  meta <- new_metadata(x, y, df_names, exclude = exclude)

  coltypes <- purrr::map(list(x = x, y = y), ~purrr::map(., ~class(.)[1])) %>%
    purrr::map_dfr(~., .id = "set") %>%
    tidyr::gather(variable, type, -set) %>%
    mutate(set = paste0("type.", set)) %>%
    tidyr::spread(set, type)

  # Check number of rows and address
  if (align || nrow(x) != nrow(y)) {
    xy <- align_data_frames(x, y, group_vars = keys, df_names)
    x <- xy$x
    y <- xy$y
  }

  # Drop ignored columns
  if (!is.null(exclude)) {
    x <- x[, intersect(colnames(x), exclude)]
    y <- y[, intersect(colnames(y), exclude)]
  }

  # De-factorize into characters
  x <- mutate_if(x, is.factor, as.character) %>% tibble::as_tibble()
  y <- mutate_if(y, is.factor, as.character) %>% tibble::as_tibble()

  xt <- purrr::map_dfr(x, ~ data_frame(value = list(.)), .id = "variable")
  yt <- purrr::map_dfr(y, ~ data_frame(value = list(.)), .id = "variable")
  z <- full_join(xt, yt, by = "variable") %>%
    mutate(
      misses     = purrr::map2(value.x, value.y, ~ not_equal(..1, ..2, tolerance)),
      misses     = purrr::pmap(list(misses, value.x, value.y), ~ union(..1, which(xor(is.na(..2), is.na(..3))))),
      misses     = purrr::map(misses, sort),
      n_diff     = purrr::map_int(misses, length),
      n_diff     = ifelse(purrr::map_lgl(value.x, is.null), NA, n_diff),
      n_diff     = ifelse(purrr::map_lgl(value.y, is.null), NA, n_diff),
      state      = ifelse(n_diff == 0, "same", "diff"),
      state      = ifelse(purrr::map_lgl(value.x, is.null), "unique_y", state),
      state      = ifelse(purrr::map_lgl(value.y, is.null), "unique_x", state),
      state      = ifelse(grepl("^_row\\.", variable), "same", state)             # Manually move row indices to "same" group
    ) %>%
    split(.$state)

  if (!is.null(z$diff)) {
    z_tidy_diff <- z$diff
    z_tidy_diff$value.x <- purrr::map2(z$diff$value.x, z$diff$misses, function(x, y) x[y])
    z_tidy_diff$value.y <- purrr::map2(z$diff$value.y, z$diff$misses, function(x, y) x[y])
    z_tidy_diff <- z_tidy_diff %>%
      select(-n_diff, -state) %>%
      split(.$variable) %>%
      purrr::map(~ {
        tidyr::unnest(.) %>%
          rename(miss_index = misses)
      })

    if (!is.null(z$same)) {
      # Add common columns to z_tidy_diff (i.e. ids, etc.)
      z_tidy_diff <- purrr::map(z_tidy_diff, function(z_td) {
        bind_cols(z_td, x[z_td$miss_index,  z$same$variable])
      })
    }

    if ("_row.x" %in% names(z_tidy_diff[[1]])) {
      z_tidy_diff <- purrr::map(
        z_tidy_diff,
        ~ rename(., miss_index.x = `_row.x`, miss_index.y = `_row.y`) %>%
          select(-`_row.z`) %>%
          select(variable, starts_with("value"), starts_with("miss"), everything())
      )
    }
  } else {
    z_tidy_diff <- NULL
  }

  z <- purrr::map_dfr(z, ~ {
    select(., variable, state, n_diff)
  })

  z <- if (is.null(z_tidy_diff)) {
    left_join(coltypes, z, by = "variable") %>%
      mutate(diff = list(NULL))
  } else {
    z_tidy_diff %>%
      purrr::map_df(~ tidyr::nest(., -variable, .key = "diff")) %>%
      left_join(z, ., by = "variable") %>%
      left_join(coltypes, ., by = "variable")
  }
  z <- z %>%
    mutate(state = factor(state, levels = c("diff", "same", "unique_x", "unique_y"))) %>%
    arrange(state, variable)

  if (!plain) new_diff_tbl(z, meta) else z
}


# ---- Helpers ----

not_equal <- function(x, y, tolerance = .Machine$double.eps) {
  # Check class of x and y and if not the same then hope for the best
  if (!identical(class(x), class(y))) class(y) <- class(x)
  this_class <- class(x)
  if (this_class %in% c("double", "numeric")) {
    which(abs(x - y) >= tolerance)
  } else {
    which(!purrr::map2_lgl(x, y, identical))
  }
}

#' Align tibbles or data frames using grouping vars
#'
#' Uses `group_vars` to find missing rows in `x` or `y` and align final rows.
align_data_frames <- function(x, y, group_vars = NULL, df_names) {
  if (!"grouped_df" %in% union(class(x), class(y))) {
    if (is.null(group_vars)) {
      xy_names <- paste0("`", df_names, "`", collapse = " and ")
      abort(paste(xy_names, "contain a different number of rows. Use `group_by()` to provide grouping variables to inform alignment."))
    }
  }
  if (!is.null(group_vars)) {
    # Use manually provided groups
    x <- group_by(x, !!!syms(group_vars))
    y <- group_by(y, !!!syms(group_vars))
  } else {
    group_vars <- determine_group_vars(x, y)
  }

  # Check duplicates in grouping vars
  list(x, y) %>%
    purrr::set_names(nm = df_names[c('x', 'y')]) %>%
    purrr::imap(., ~ {
      distinct_rows <- nrow(distinct(.x, !!!syms(group_vars)))
      if (nrow(.x) != distinct_rows) {
        abort(glue("`{.y}` only has {distinct_rows} distinct keys out of {nrow(.x)} key-pairs"))
      }
    })

  # Add original row numbers
  xy <- purrr::map(list(x = x, y  = y), ~ ungroup(.))
  for (v in c("x", "y")) {
    xy[[v]][[paste0("_row.", v)]] <- 1:nrow(xy[[v]])
  }

  # Join by IDs and generate new row numbers
  row_ids <- xy %>%
    purrr::map(~ select(., !!!syms(group_vars), starts_with("_row"))) %>%
    purrr::reduce(full_join, by = group_vars) %>%
    mutate(`_row.z` = 1:nrow(.))

  # Merge old data frames with new row numbers, arrange by new row
  xy %>%
    purrr::map(
      ~ full_join(., row_ids, by = intersect(names(.), names(row_ids))) %>%
        arrange(`_row.z`)
    )
}

determine_group_vars <- function(x, y) {
  # Use groups from grouped_df
  group_vars <- purrr::map(list(x = x, y = y), dplyr::group_vars)

  # Handle mismatched groups
  if (length(unique(purrr::map_int(group_vars, length))) != 1) {
    # If group vars are available in both, use union, else abort
    group_vars_all <- purrr::reduce(group_vars, union)
    if (length(setdiff(group_vars_all, union(names(x), names(y)))) == 0) {
      group_vars <- list(group_vars_all)
    } else {
      common <- purrr::reduce(group_vars, intersect)
      uniq   <- purrr::map(group_vars, setdiff, y = common) %>% purrr::compact()
      msg    <- glue(
        "`x` and `y` have group column(s) ", glue_collapse('"{common}"', sep = ", "),
        " in common but ", glue_collapse(
          purrr::imap_chr(uniq, ~ glue("`{.y}` has column(s) ", glue_collapse('"{.x}"', sep = ", "))),
          sep = " and "
        )
      )
      abort(msg)
    }
  }
  group_vars[[1]]
}
