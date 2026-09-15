# load_study_data ----
#' Data Formatting: Import and Format Study Files
#' @importFrom cli cli_abort cli_inform
#' @importFrom readr read_csv read_tsv
#' @importFrom janitor remove_empty clean_names
#' @importFrom dplyr rename_with
#' @param data_path string with path to study-data file
#' @param animal_string character string
#' for matching animal names to be replaced by `"animal"`.
#' Default is `NULL`, which resolves to `config$animal_string` (using
#' `study_config()`'s default of `"monkey|dog"` when `config` is not
#' supplied). An explicitly supplied `animal_string` argument always
#' overrides `config`.
#' @param config optional `PriorRhythmConfig` object created by [study_config()]
#'   or [read_study_config()]. When `animal_string` is `NULL`,
#'   `config$animal_string` supplies its value; when `config` is not
#'   supplied, [study_config()]'s default config is used instead. An
#'   explicitly supplied `animal_string` argument always wins, and
#'   `config$column_map` is applied to rename columns after
#'   [janitor::clean_names()] and animal-column normalization.
#' @param ... additional arguments passed to file readers.
#' @return A tibble containing the cleaned and formatted study data.
#' @importFrom rlang %||%
#' @export
#' @example R/examples/Example_load_study_data.R
load_study_data <- function(data_path     = NULL,
                            animal_string = NULL,
                            config        = NULL,
                            ...) {
  
  if (FALSE) {
    data_path     <- "study.csv"
    animal_string <- "monkey|dog"
    config        <- NULL
  }
  
  # Resolve animal_string: an explicitly supplied argument wins over config,
  # which falls back to study_config()'s default when config is not supplied.
  config        <- config %||% study_config()
  animal_string <- animal_string %||% config$animal_string
  
  if (is.null(data_path) || !nzchar(data_path)) {
    cli::cli_abort("{.arg data_path} must be a non-empty file path.")
  }
  
  if (!file.exists(data_path)) {
    cli::cli_abort("Study data file not found: {.path {data_path}}")
  }
  
  cli::cli_inform("processing as a file")
  
  file_ext <- tolower(tools::file_ext(data_path))
  
  input_dat <- if (identical(file_ext, "csv")) {
    readr::read_csv(data_path, show_col_types = FALSE, ...)
  } else if (identical(file_ext, "tsv")) {
    readr::read_tsv(data_path, show_col_types = FALSE, ...)
  } else {
    cli::cli_abort(
      "Unsupported study-data file extension {.val .{file_ext}} in {.path {data_path}}. \\
       CSV and TSV are supported; export the workbook to CSV or TSV before importing."
    )
  }
  
  output <- input_dat |>
    janitor::remove_empty("cols") |>
    janitor::clean_names() |>
    dplyr::rename_with(function(x_name) {
      gsub(animal_string, "animal", x = x_name, ignore.case = TRUE)
    })
  
  # Apply config-driven column mapping after clean-names and animal
  # normalization (no explicit column_map argument on this function).
  if (!is.null(config$column_map)) {
    output <- .resolve_column_map(output, column_map = config$column_map)
  }
  
  return(output)
}

# time_bins ----
#' Data Formatting: Break Up Time Bins
#'
#' Parses a time-interval column (e.g. `"0 to 3 hr"`) into two numeric
#' columns `time1` and `time2`.  Delegates to the shared internal parser
#' `.parse_time_intervals()`, which applies config-driven validation and
#' consistent parsing rules.
#'
#' Accepted interval formats (whitespace around the delimiter is optional):
#' \describe{
#'   \item{`"0 to 3 hr"`}{number + delimiter + number + unit suffix}
#'   \item{`"3.5to6.0 hr"`}{whitespace-free variant}
#'   \item{`"0.75 to 3.50"`}{unitless, when `allow_unitless = TRUE` (default)}
#' }
#'
#' Settings are resolved with the following precedence (highest to lowest):
#' explicit argument → `config$time_parser` → package defaults
#' (`delimiter = "to"`, `units = c("hr", "hours")`, `allow_unitless = TRUE`).
#'
#' @importFrom dplyr relocate all_of
#' @export
#' @param dat a data.frame.
#' @param time_col string with the column name containing time bins.
#' @param delimiter scalar character string separating the two numeric bounds.
#'   Default is `NULL`, which resolves to `"to"` via config or package default.
#'   Regex-special characters are safely escaped
#'   internally. Overrides `config$time_parser$delimiter`.
#' @param units character vector of accepted unit suffixes
#'   (e.g. `c("hr", "hours")`). Overrides `config$time_parser$units`.
#' @param allow_unitless logical. When `TRUE` (default), interval labels
#'   without a unit suffix are accepted. Overrides
#'   `config$time_parser$allow_unitless`.
#' @param config optional `PriorRhythmConfig` from [study_config()] or
#'   [read_study_config()]. Its `time_parser` sub-list supplies defaults for
#'   any of the three parser arguments not explicitly provided.
#' @return A data.frame with two new columns, `time1` and `time2`, parsed
#'   from the time bin column, placed immediately after `time_col`.
#' @example R/examples/Example_time_bins.R
time_bins <- function(dat            = NULL,
                      delimiter      = NULL,
                      time_col       = NULL,
                      units          = NULL,
                      allow_unitless = NULL,
                      config         = NULL) {
  
  output <- .parse_time_intervals(
    df             = dat,
    time_col       = time_col,
    delimiter      = delimiter,
    units          = units,
    allow_unitless = allow_unitless,
    config         = config
  )
  return(output)
}

# random_data_generator ----
#' @title Build Support: Randomly Generates Example Data Files
#' @importFrom stringr str_pad
#' @importFrom stats rnorm
#' @importFrom purrr set_names map
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate relocate rename_with
#' @param ST_length number indicating the number of
#' randomly generated studies to produce (default = 15).
#' @param syn_study_strings vector of strings to
#' be sampled for generating file names.
#' Default is `c("EX", "ZY", "QZ")`
#' @return A named list of data.frames, each representing a randomly
#' generated synthetic study dataset.
#' @export
#' @example R/examples/Example_random_data_generator.R
random_data_generator <- function(ST_length         = 15,
                                  syn_study_strings = c("EX", "ZY", "QZ")) {
  
  yr  <- sample(seq(15, 20), ST_length, replace = TRUE)
  str <- sample(syn_study_strings, ST_length, replace = TRUE)
  idx <- stringr::str_pad(sample(seq(5, 150), ST_length, replace = TRUE), 3, pad = "0")
  
  made_up_studies <- paste0(yr, str, idx)
  
  prefix_file_name <- sample(
    c("Study_", "Study_dat", "Final_analysis_", "Study_analysis_"),
    ST_length, replace = TRUE
  )
  suffix_file_name <- sample(
    c("_dat.csv", "_Complete.csv", "_complete.csv", "_.csv"),
    ST_length, replace = TRUE
  )
  
  made_up_file_names <- paste0(prefix_file_name, made_up_studies, suffix_file_name)
  
  made_up_file_names |>
    purrr::set_names(made_up_file_names) |>
    purrr::map(function(x) {
      length_out <- sample(seq(30, 300), 1, replace = TRUE)
      
      data.frame(
        animal         = seq(1, length_out),
        Period_code    = sample(c(1, 2, 3, 4, 5), length_out, replace = TRUE),
        Treatment_code = sample(c(1, 2, 3, 4), length_out, replace = TRUE),
        Time           = sample(
          c("1.00 to 3.5 hr", "9 to 18 hr", "3.5 to 6.0 hr", "3.5to6.0 hr"),
          length_out, replace = TRUE
        ),
        Assay1 = stats::rnorm(length_out, 40, 10),
        Assay2 = stats::rnorm(length_out, 40, 10),
        Assay3 = stats::rnorm(length_out, 40, 10),
        Assay4 = stats::rnorm(length_out, 40, 10)
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(Period = as.character(glue::glue("Day {.data$Period_code}"))) |>
        dplyr::relocate(c("Period"), .before = 3) |>
        dplyr::rename_with(
          function(x) sample(c("Period code", "Period_code"), size = 1, replace = TRUE),
          .cols = "Period_code"
        ) |>
        dplyr::rename_with(
          function(x) sample(c("Treatment code", "Treatment_code"), size = 1, replace = TRUE),
          .cols = "Treatment_code"
        ) |>
        dplyr::rename_with(
          function(x) sample(c("Animal", "Monkey", "monkey"), size = 1, replace = TRUE),
          .cols = "animal"
        )
    })
}

# .resolve_column_map ----
# Internal helper: apply a multi-alias column map to a single data frame.
# column_map is a named list where each value is a character vector of aliases.
# For each canonical name:
#   - if canonical is already present in df, skip silently.
#   - otherwise, find the first alias present in names(df) and rename it.
#   - if no alias is found, skip silently (matches any_of() behaviour).
# Named character vectors are coerced to a named list internally.
.resolve_column_map <- function(df = NULL, column_map = NULL) {
  
  if (FALSE) {
    df         <- data.frame(summary_period = 1:3, a_mean = 1:3)
    column_map <- list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
  }
  
  if (is.null(column_map) || length(column_map) == 0L) {
    return(df)
  }
  
  # Normalise named character vector to named list
  if (is.character(column_map)) {
    column_map <- as.list(column_map)
  }
  
  col_names <- names(df)
  
  for (canonical in names(column_map)) {
    # Canonical name already present — no rename needed
    if (canonical %in% col_names) {
      next
    }
    
    aliases   <- column_map[[canonical]]
    found_idx <- match(aliases, col_names)
    found_idx <- found_idx[!is.na(found_idx)]
    
    if (length(found_idx) > 0L) {
      col_names[found_idx[1L]] <- canonical
    }
    # No alias found — skip silently
  }
  
  names(df) <- col_names
  output    <- df
  return(output)
}


# .resolve_column_map_with_diagnostics ----
# Shared helper: applies a column map to a single data frame and returns
# both the renamed data frame and structured provenance diagnostics.
#
# Precedence (highest → lowest):
#   override_map  →  column_map  →  (no default fallback)
#
# Returns a named list:
#   $df          — the prepared data frame
#   $provenance  — named character vector; names are canonical field names,
#                  values are the source column that supplied them
#                  (NA_character_ when no alias matched and canonical was absent)
.resolve_column_map_with_diagnostics <- function(
    df           = NULL,
    column_map   = NULL,
    override_map = NULL
) {
  
  if (FALSE) {
    df           <- data.frame(summary_period = 1:3, a_mean = 1:3)
    column_map   <- list(time = c("summary_period", "Time"), activity = "a_mean")
    override_map <- NULL
  }
  
  # Merge: override_map entries win over column_map
  merged_map <- column_map
  if (!is.null(override_map) && length(override_map) > 0L) {
    if (is.character(override_map)) {
      override_map <- as.list(override_map)
    }
    for (nm in names(override_map)) {
      merged_map[[nm]] <- override_map[[nm]]
    }
  }
  
  if (is.null(merged_map) || length(merged_map) == 0L) {
    provenance <- setNames(
      rep(NA_character_, 0L),
      character(0L)
    )
    output     <- list(df = df, provenance = provenance)
    return(output)
  }
  
  if (is.character(merged_map)) {
    merged_map <- as.list(merged_map)
  }
  
  col_names  <- names(df)
  provenance <- setNames(
    rep(NA_character_, length(merged_map)),
    names(merged_map)
  )
  
  for (canonical in names(merged_map)) {
    if (canonical %in% col_names) {
      # Canonical already present — record it, no rename needed
      provenance[[canonical]] <- canonical
      next
    }
    
    aliases   <- merged_map[[canonical]]
    found_idx <- match(aliases, col_names)
    found_idx <- found_idx[!is.na(found_idx)]
    
    if (length(found_idx) > 0L) {
      src_col                  <- col_names[found_idx[1L]]
      provenance[[canonical]]  <- src_col
      col_names[found_idx[1L]] <- canonical
    }
    # No alias found → provenance remains NA
  }
  
  names(df) <- col_names
  output    <- list(df = df, provenance = provenance)
  return(output)
}


# .validate_study_input ----
# Shared validation API used by both import_historical_data() and
# mod_file_upload_server().
#
# Arguments:
#   df             — prepared data frame (after column mapping)
#   provenance     — named character vector from .resolve_column_map_with_diagnostics()
#   required_cols  — character vector of canonical column names that must be present
#   endpoint_cols  — character vector of endpoint columns to verify (optional)
#   abort          — if TRUE (default), call cli::cli_abort() when required cols are missing
#
# Returns a named list:
#   $ok       — logical; TRUE when all required columns are present
#   $missing  — character vector of missing required canonical column names
#   $warnings — character vector of non-fatal advisory messages
.validate_study_input <- function(
    df            = NULL,
    provenance    = NULL,
    required_cols = NULL,
    endpoint_cols = NULL,
    abort         = TRUE
) {
  
  if (FALSE) {
    df            <- data.frame(animal = 1, time = "0 to 3 hr")
    provenance    <- c(animal = "animal", time = "time")
    required_cols <- c("animal", "time")
    endpoint_cols <- NULL
    abort         <- TRUE
  }
  
  missing_cols  <- character(0L)
  warnings_out  <- character(0L)
  
  if (!is.null(required_cols) && length(required_cols) > 0L) {
    absent <- required_cols[!(required_cols %in% names(df))]
    if (length(absent) > 0L) {
      missing_cols <- absent
    }
  }
  
  if (!is.null(endpoint_cols) && length(endpoint_cols) > 0L) {
    absent_ep <- endpoint_cols[!(endpoint_cols %in% names(df))]
    if (length(absent_ep) > 0L) {
      warnings_out <- c(
        warnings_out,
        paste0("Configured endpoint column(s) not found: ",
               paste(absent_ep, collapse = ", "))
      )
    }
  }
  
  ok <- length(missing_cols) == 0L
  
  if (!ok && isTRUE(abort)) {
    cli::cli_abort(c(
      "Required column(s) missing from study data:",
      setNames(
        paste0("{.field ", missing_cols, "}"),
        rep("x", length(missing_cols))
      )
    ))
  }
  
  output <- list(
    ok       = ok,
    missing  = missing_cols,
    warnings = warnings_out
  )
  return(output)
}


# .regex_escape ----
# Internal helper: escape all regex-special characters in a string so it can
# be embedded literally inside a regular expression.  Uses only base R.
.regex_escape <- function(x = NULL) {
  output <- gsub("([.^$*+?{}|\\[\\](\\\\)])", "\\\\\\1", x, perl = TRUE)
  return(output)
}


# .parse_time_intervals ----
# Shared time-interval parser/validator.  Reads the already-resolved mapped
# time column, preserves it unchanged, and adds/refreshes derived numeric
# columns time1 and time2.
#
# Accepted formats (whitespace around delimiter is optional):
#   "<number> <delimiter> <number> <unit>"    e.g. "0 to 3 hr"
#   "<number><delimiter><number> <unit>"      e.g. "3.5to6.0 hr" (whitespace-free)
#   "<number> <delimiter> <number>"           e.g. "0.75 to 3.50" (unitless, when allowed)
#
# Argument precedence (highest → lowest):
#   explicit argument  >  config$time_parser  >  package defaults (.time_parser_defaults)
#
# Arguments:
#   df             — data frame containing the time column
#   time_col       — string: name of the time column in df
#   delimiter      — scalar character; overrides config and defaults
#   units          — character vector of accepted suffixes; overrides config and defaults
#   allow_unitless — logical; overrides config and defaults
#   config         — optional PriorRhythmConfig; supplies time_parser sub-list
#
# Returns df with time1 and time2 columns added/refreshed (immediately after
# time_col).
.parse_time_intervals <- function(
    df             = NULL,
    time_col       = NULL,
    delimiter      = NULL,
    units          = NULL,
    allow_unitless = NULL,
    config         = NULL
) {
  
  if (FALSE) {
    df             <- data.frame(time = c("0 to 3 hr", "3 to 6 hr", "3.5to6.0 hr"))
    time_col       <- "time"
    delimiter      <- NULL
    units          <- NULL
    allow_unitless <- NULL
    config         <- NULL
  }
  
  # Resolve settings with precedence: explicit arg > config$time_parser > defaults
  cfg_tp <- if (!is.null(config) && !is.null(config$time_parser)) {
    config$time_parser
  } else {
    list()
  }
  
  resolved_delimiter <- if (!is.null(delimiter)) {
    delimiter
  } else if (!is.null(cfg_tp$delimiter)) {
    cfg_tp$delimiter
  } else {
    .time_parser_defaults$delimiter
  }
  
  resolved_units <- if (!is.null(units)) {
    units
  } else if (!is.null(cfg_tp$units)) {
    cfg_tp$units
  } else {
    .time_parser_defaults$units
  }
  
  resolved_allow_unitless <- if (!is.null(allow_unitless)) {
    allow_unitless
  } else if (!is.null(cfg_tp$allow_unitless)) {
    cfg_tp$allow_unitless
  } else {
    .time_parser_defaults$allow_unitless
  }
  
  if (is.null(time_col) || !nzchar(time_col)) {
    cli::cli_abort("{.arg time_col} must be a non-empty string.")
  }
  
  if (!(time_col %in% names(df))) {
    cli::cli_abort(
      "Time column {.field {time_col}} not found in the data frame."
    )
  }
  
  raw_vals <- df[[time_col]]
  
  # Build regex pattern from resolved settings
  esc_delim <- .regex_escape(resolved_delimiter)
  
  units_part <- if (length(resolved_units) > 0L) {
    esc_units <- vapply(resolved_units, .regex_escape, character(1L))
    units_alt <- paste(esc_units, collapse = "|")
    if (resolved_allow_unitless) {
      paste0("(?:", units_alt, ")?")
    } else {
      paste0("(?:", units_alt, ")")
    }
  } else {
    # No configured units
    ""
  }
  
  pattern <- paste0(
    "^\\s*([0-9]+(?:\\.[0-9]*)?)\\s*",
    esc_delim,
    "\\s*([0-9]+(?:\\.[0-9]*)?)\\s*",
    units_part,
    "\\s*$"
  )
  
  non_missing <- !is.na(raw_vals)
  bad_mask    <- non_missing & !grepl(pattern, raw_vals, ignore.case = TRUE)
  
  if (any(bad_mask)) {
    bad_vals <- unique(raw_vals[bad_mask])
    example_unit <- if (length(resolved_units) > 0L) resolved_units[[1L]] else ""
    example_str  <- if (nzchar(example_unit)) {
      paste0("0 ", resolved_delimiter, " 3 ", example_unit)
    } else {
      paste0("0 ", resolved_delimiter, " 3")
    }
    cli::cli_abort(c(
      "Cannot parse time interval value(s) in column {.field {time_col}}:",
      setNames(
        paste0("{.val ", bad_vals, "}"),
        rep("x", length(bad_vals))
      ),
      "i" = "Accepted form: {.val {example_str}} (spaces around {.q {resolved_delimiter}} are optional)."
    ))
  }
  
  t1 <- rep(NA_real_, length(raw_vals))
  t2 <- rep(NA_real_, length(raw_vals))
  
  parsed <- regmatches(
    raw_vals[non_missing],
    regexec(pattern, raw_vals[non_missing], ignore.case = TRUE)
  )
  
  t1_vals <- as.numeric(vapply(parsed, `[[`, character(1L), 2L))
  t2_vals <- as.numeric(vapply(parsed, `[[`, character(1L), 3L))
  
  reversed <- !is.na(t1_vals) & !is.na(t2_vals) & (t2_vals <= t1_vals)
  if (any(reversed)) {
    bad_rev <- unique(raw_vals[non_missing][reversed])
    cli::cli_abort(c(
      "Time interval(s) have time2 \u2264 time1 in column {.field {time_col}}:",
      setNames(
        paste0("{.val ", bad_rev, "}"),
        rep("x", length(bad_rev))
      ),
      "i" = "The upper bound must be strictly greater than the lower bound."
    ))
  }
  
  t1[non_missing] <- t1_vals
  t2[non_missing] <- t2_vals
  
  df[["time1"]] <- t1
  df[["time2"]] <- t2
  
  df_out <- dplyr::relocate(
    df,
    dplyr::all_of(c("time1", "time2")),
    .after = dplyr::all_of(time_col)
  )
  
  output <- df_out
  return(output)
}


# .derive_period_code ----
# Internal helper: add a `period_code` integer column derived from a `period`
# column, preserving encounter order as the level ordering.
# If `period_code` already exists in df, the data frame is returned unchanged.
# If neither `period_code` nor `period` is present, the data frame is also
# returned unchanged.
.derive_period_code <- function(df = NULL, period_col = "period_code") {
  
  if (FALSE) {
    df         <- data.frame(period = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr"))
    period_col <- "period_code"
  }
  
  # Already present — never overwrite
  if (period_col %in% names(df)) {
    return(df)
  }
  
  # Source column absent — nothing to derive
  if (!("period" %in% names(df))) {
    return(df)
  }
  
  period_factor    <- factor(df[["period"]], levels = unique(df[["period"]]))
  df[[period_col]] <- as.integer(period_factor)
  output           <- df
  return(output)
}


# import_historical_data ----
#' Import Historical Study Data
#'
#' A simple, single-entry-point wrapper that loads a named list of study
#' data frames (or reads files from a directory), applies multi-alias column
#' renaming via a `.resolve_column_map()` helper, and returns a combined
#' data frame ready for [chain_priors_from_data()].
#'
#' @param data_file_list a named list of data frames (one per study), as
#'   returned by [base::list()] with one entry per study. If `NULL`,
#'   `data_path_dir` must be provided.
#' @param data_path_dir character path to a directory of study files.
#' @param file_pattern regex passed to [base::list.files()] for filtering
#'   study files by name. Defaults to matching `.csv` and `.tsv` files.
#' @param animal_string regex used to match/normalize animal column names when
#'   reading files from `data_path_dir`.
#' @param column_map column name mapping. Accepts four forms:
#'   \describe{
#'     \item{`NULL`}{No renaming applied (default).}
#'     \item{named character vector}{One-to-one mapping: `c(new = "old")`.
#'       Backward-compatible with the previous `name_key` argument.}
#'     \item{named list}{One-to-many mapping: each element is a character
#'       vector of aliases tried in order; first match wins.
#'       Example: `list(period_code = c("Period_Code", "Period Code", "period"))`.}
#'     \item{character scalar (file path)}{Path to a YAML file whose
#'       `column_map` key is parsed into one of the above forms.}
#'   }
#' @param animal_col string. Expected column name for animal identifiers in
#'   the output data frame after column mapping is applied. Default `"animal"`.
#' @param study_col string. Expected column name for study identifiers.
#'   Default `"study"`.
#' @param period_col string. Expected column name for period codes.
#'   Default `"period_code"`.
#' @param treatment_col string. Expected column name for treatment codes.
#'   Default `"treatment_code"`.
#' @param time_col string. Expected column name for time bins.
#'   Default `"time"`.
#' @param config optional `PriorRhythmConfig` object created by [study_config()]
#'   or [read_study_config()]. When supplied, `config$column_map` is used as
#'   the default for `column_map` (an explicit `column_map` argument always
#'   wins).
#' @return A data frame combining all study data with column mapping applied,
#'   ready for [chain_priors_from_data()].
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom dplyr bind_rows
#' @importFrom purrr map set_names
#' @importFrom yaml read_yaml
#' @export
#' @example R/examples/Example_import_historical_data.R
import_historical_data <- function(
    data_file_list = NULL,
    data_path_dir  = NULL,
    file_pattern   = "\\.(csv|tsv)$",
    animal_string  = NULL,
    column_map     = NULL,
    animal_col     = "animal",
    study_col      = "study",
    period_col     = "period_code",
    treatment_col  = "treatment_code",
    time_col       = "time",
    config         = NULL
) {
  
  if (FALSE) {
    
    data_file_list <- list(
      study_1 = data.frame(
        animal         = 1:3,
        summary_period = c("0 to 3 hr", "3 to 6 hr", "0 to 3 hr"),
        a_mean         = c(40.1, 42.3, 38.5)
      )
    )
    
    data_file_list <- NULL
    
    #data_path_dir
    #data_path_dir  <- NULL
    file_pattern   <- "\\.(csv|tsv)$"
    animal_string  <- "monkey|dog"
    column_map     <- list(
      time     = c("summary_period", "Time"),
      activity = c("a_mean", "Activity")
    )
    animal_col     <- "animal"
    study_col      <- "study"
    period_col     <- "period_code"
    treatment_col  <- "treatment_code"
    time_col       <- "time"
    config         <- NULL
  }
  
  # 1. Resolve column_map: explicit arg wins over config
  if (is.null(column_map) && !is.null(config)) {
    column_map <- config$column_map
  }
  
  if (is.null(data_file_list) == is.null(data_path_dir)) {
    cli::cli_abort(
      "Provide exactly one of {.arg data_file_list} or {.arg data_path_dir}."
    )
  }
  
  if (is.null(animal_string)) {
    if (!is.null(config) && !is.null(config$animal_string)) {
      animal_string <- config$animal_string
    } else {
      animal_string <- "monkey|dog"
    }
  }
  
  # Handle YAML file path (unnamed length-1 character scalar)
  if (is.character(column_map) &&
      length(column_map) == 1L &&
      (is.null(names(column_map)) || !nzchar(names(column_map)))) {
    if (!file.exists(column_map)) {
      cli::cli_abort("YAML file not found: {.path {column_map}}")
    }
    yaml_parsed <- yaml::read_yaml(column_map)
    raw_cm      <- yaml_parsed[["column_map"]]
    if (!is.null(raw_cm)) {
      column_map <- lapply(raw_cm, function(v) as.character(unlist(v)))
    } else {
      column_map <- NULL
    }
  }
  
  # Normalise named character vector to named list
  if (is.character(column_map) && !is.null(names(column_map))) {
    column_map <- as.list(column_map)
  }
  
  # 2. Resolve data_file_list
  if (is.null(data_file_list)) {
    if (!dir.exists(data_path_dir)) {
      cli::cli_abort("Historical data directory not found: {.path {data_path_dir}}")
    }
    
    data_file_paths <- list.files(
      path        = data_path_dir,
      pattern     = file_pattern,
      full.names  = TRUE,
      ignore.case = TRUE
    )
    
    if (length(data_file_paths) == 0L) {
      cli::cli_abort(
        "No historical-data files found in {.path {data_path_dir}} 
        matching pattern {.val {file_pattern}}."
      )
    }
    
    data_file_list <- data_file_paths |>
      purrr::map(function(x_path) {
        load_study_data(
          data_path     = x_path,
          animal_string = animal_string
        )
      }) |>
      purrr::set_names(tools::file_path_sans_ext(basename(data_file_paths)))
  }
  
  if (is.null(names(data_file_list)) || any(names(data_file_list) == "")) {
    cli::cli_abort("{.arg data_file_list} must be a named list.")
  }
  
  # 3. Ensure study column exists before mapping/binding
  data_file_list <- purrr::map2(
    .x = data_file_list,
    .y = names(data_file_list),
    .f = function(x_df, y_study) {
      if (!(study_col %in% names(x_df))) {
        x_df[[study_col]] <- y_study
      }
      x_df
    }
  )
  
  # 4. Apply column mapping to each data frame (with provenance diagnostics)
  data_file_list <- purrr::map(data_file_list, function(df) {
    resolved <- .resolve_column_map_with_diagnostics(df, column_map = column_map)
    resolved$df
  })
  
  # 5. Derive period_code from period where possible
  data_file_list <- purrr::map(data_file_list, function(x_df) {
    .derive_period_code(x_df, period_col = period_col)
  })
  
  # Normalize raw period labels so files with numeric labels (e.g. 1, 2)
  # and character labels (e.g. "Day 1") can be row-bound. The canonical
  # period_code field is derived when absent and preserved when supplied.
  data_file_list <- purrr::map(data_file_list, function(x_df) {
    if ("period" %in% names(x_df)) {
      x_df[["period"]] <- as.character(x_df[["period"]])
    }
    x_df
  })
  
  # 6. Combine into a single data frame
  output <- purrr::list_rbind(data_file_list)
  
  return(output)
}
