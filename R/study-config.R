# .time_parser_defaults ----
# Package-level defaults for time-interval parsing.
.time_parser_defaults <- list(
  delimiter      = "to",
  units          = c("hr", "hours"),
  allow_unitless = TRUE
)


# study_config ----
#' Create a PriorRhythm Study Configuration Object
#' @importFrom cli cli_abort cli_inform cli_rule cli_bullets
#' @param file_pattern regex passed to [base::list.files()] for filtering
#'   study files by name. Default
#'   (`"\\.(csv|tsv)$"`) matches CSV and TSV study-data files.
#' @param animal_string regex used to identify animal species strings in the
#'   data. Default is `"monkey|dog"`.
#' @param column_map column name mapping. Accepts a named character vector
#'   (one-to-one: `c(new_name = "old_name")`) or a named list (one-to-many:
#'   each element is a character vector of aliases tried in order; first match
#'   wins). Set to `NULL` (default) if no renaming is needed. Example (multi-alias):
#'   `list(period_code = c("Period_Code", "Period Code", "period"))`.
#' @param endpoints named list of endpoint-specific settings. Each element
#'   must be a named list with any subset of the following fields:
#'   `alpha0_pop_mean` (numeric), `value_min` (numeric), `value_max`
#'   (numeric), `threshold` (numeric). When both `value_min` and `value_max`
#'   are provided, `value_min` must be strictly less than `value_max`.
#' @param active_endpoint character scalar naming the active endpoint. Must
#'   be `NULL` (default) or a name present in `endpoints`.
#' @param time_parser named list of time-interval parsing settings, merged
#'   onto package defaults. Accepted keys:
#'   \describe{
#'     \item{`delimiter`}{Scalar non-empty character string separating the
#'       two numeric bounds in a time-interval label. Default `"to"`.}
#'     \item{`units`}{Character vector of accepted unit suffixes
#'       (e.g. `c("hr", "hours")`). Use `character(0)` to accept no suffix
#'       (only meaningful when `allow_unitless = TRUE`).}
#'     \item{`allow_unitless`}{Non-NA scalar logical. When `TRUE` (default),
#'       interval labels with no unit suffix are accepted. When `FALSE`, at
#'       least one suffix from `units` must be present.}
#'   }
#'   Unrecognised keys cause an error. Missing keys retain their defaults.
#' @return A validated S3 object of class `"PriorRhythmConfig"`.
#' @seealso [read_study_config()], [write_study_config_template()]
#' @export
#' @example R/examples/Example_study_config.R
study_config <- function(
    file_pattern    = "\\.(csv|tsv)$",
    animal_string   = "monkey|dog",
    column_map      = NULL,
    endpoints       = list(),
    active_endpoint = NULL,
    time_parser     = list()
) {

  if (FALSE) {
    file_pattern    <- "\\.(csv|tsv)$"
    animal_string   <- "monkey|dog"
    column_map      <- list(
      time         = c("summary_period", "Time"),
      activity     = c("a_mean", "Activity"),
      temperature  = "t_mean"
    )
    endpoints       <- list(
      qtc = list(alpha0_pop_mean = 250, value_min = 150, value_max = 400, threshold = 10)
    )
    active_endpoint <- "qtc"
    time_parser     <- list(delimiter = "to", units = c("hr", "hours"), allow_unitless = TRUE)
  }

  if (!is.null(column_map)) {
    # Accept named character vector (old format) OR named list (new multi-alias format)
    if (!is.character(column_map) && !is.list(column_map)) {
      cli::cli_abort(
        "{.arg column_map} must be a named character vector, a named list, or NULL."
      )
    }
    if (is.null(names(column_map)) || any(nchar(names(column_map)) == 0L)) {
      cli::cli_abort(
        "{.arg column_map} must be fully named (every element needs a name)."
      )
    }
    if (is.list(column_map)) {
      # Each element must be a non-empty character vector
      bad <- vapply(column_map, function(v) !is.character(v) || length(v) == 0L, logical(1))
      if (any(bad)) {
        cli::cli_abort(
          "Each value in {.arg column_map} must be a non-empty character vector. \\
           Problem entries: {.val {names(column_map)[bad]}}."
        )
      }
    }
  }

  if (!is.list(endpoints)) {
    cli::cli_abort("{.arg endpoints} must be a named list.")
  }

  if (length(endpoints) > 0L) {
    if (is.null(names(endpoints)) || any(nchar(names(endpoints)) == 0L)) {
      cli::cli_abort(
        "{.arg endpoints} must be a fully named list (every element needs a name)."
      )
    }

    allowed_keys <- c("alpha0_pop_mean", "value_min", "value_max", "threshold")

    for (ep_name in names(endpoints)) {
      ep <- endpoints[[ep_name]]

      if (!is.list(ep)) {
        cli::cli_abort(
          "Each entry in {.arg endpoints} must be a list. Entry {.val {ep_name}} is not a list."
        )
      }

      unknown_keys <- setdiff(names(ep), allowed_keys)
      if (length(unknown_keys) > 0L) {
        cli::cli_abort(
          c(
            "Unknown key(s) in endpoint {.val {ep_name}}: {.val {unknown_keys}}.",
            "i" = "Allowed keys: {.val {allowed_keys}}."
          )
        )
      }

      for (num_key in c("alpha0_pop_mean", "value_min", "value_max", "threshold")) {
        if (!is.null(ep[[num_key]]) && !is.numeric(ep[[num_key]])) {
          cli::cli_abort(
            "{.field {num_key}} in endpoint {.val {ep_name}} must be numeric."
          )
        }
      }

      if (!is.null(ep$value_min) && !is.null(ep$value_max)) {
        if (ep$value_min >= ep$value_max) {
          cli::cli_abort(
            "{.field value_min} must be strictly less than {.field value_max} in \\
             endpoint {.val {ep_name}}. \\
             Got value_min = {ep$value_min}, value_max = {ep$value_max}."
          )
        }
      }
    }
  }

  if (!is.null(active_endpoint)) {
    if (!is.character(active_endpoint) || length(active_endpoint) != 1L) {
      cli::cli_abort("{.arg active_endpoint} must be NULL or a scalar character string.")
    }
    if (length(endpoints) == 0L || !(active_endpoint %in% names(endpoints))) {
      cli::cli_abort(
        "{.arg active_endpoint} ({.val {active_endpoint}}) must be a name present in {.arg endpoints}."
      )
    }
  }

  # Validate and resolve time_parser
  if (!is.list(time_parser)) {
    cli::cli_abort("{.arg time_parser} must be a named list.")
  }

  allowed_tp_keys <- c("delimiter", "units", "allow_unitless")
  unknown_tp_keys <- setdiff(names(time_parser), allowed_tp_keys)
  if (length(unknown_tp_keys) > 0L) {
    cli::cli_abort(
      c(
        "Unknown key(s) in {.arg time_parser}: {.val {unknown_tp_keys}}.",
        "i" = "Allowed keys: {.val {allowed_tp_keys}}."
      )
    )
  }

  # Merge user values onto package defaults
  resolved_tp <- .time_parser_defaults
  for (k in names(time_parser)) {
    resolved_tp[[k]] <- time_parser[[k]]
  }

  if (!is.character(resolved_tp$delimiter) ||
      length(resolved_tp$delimiter) != 1L ||
      !nzchar(resolved_tp$delimiter)) {
    cli::cli_abort(
      "{.field delimiter} in {.arg time_parser} must be a scalar non-empty character string."
    )
  }

  if (!is.character(resolved_tp$units)) {
    cli::cli_abort(
      "{.field units} in {.arg time_parser} must be a character vector."
    )
  }

  if (length(resolved_tp$units) > 0L && any(!nzchar(resolved_tp$units))) {
    cli::cli_abort(
      "{.field units} in {.arg time_parser} must not contain empty strings."
    )
  }

  if (!is.logical(resolved_tp$allow_unitless) ||
      length(resolved_tp$allow_unitless) != 1L ||
      is.na(resolved_tp$allow_unitless)) {
    cli::cli_abort(
      "{.field allow_unitless} in {.arg time_parser} must be a non-NA scalar logical."
    )
  }

  if (!resolved_tp$allow_unitless && length(resolved_tp$units) == 0L) {
    cli::cli_abort(
      c(
        "{.field units} in {.arg time_parser} must be non-empty when {.field allow_unitless} is {.code FALSE}.",
        "i" = "Provide at least one unit suffix, or set {.field allow_unitless = TRUE}."
      )
    )
  }

  config <- structure(
    list(
      file_pattern    = file_pattern,
      animal_string   = animal_string,
      column_map      = column_map,
      endpoints       = endpoints,
      active_endpoint = active_endpoint,
      time_parser     = resolved_tp
    ),
    class = "PriorRhythmConfig"
  )

  return(config)
}


# print.PriorRhythmConfig ----
#' Print a PriorRhythmConfig Object
#' @param x a `PriorRhythmConfig` object.
#' @param ... unused; present for S3 method compatibility.
#' @return Invisibly returns `x`.
#' @export
print.PriorRhythmConfig <- function(x = NULL, ...) {

  if (FALSE) {
    x <- study_config()
  }

  cli::cli_rule(left = "PriorRhythmConfig")

  cli::cli_bullets(c(
    "*" = "file_pattern:  {x$file_pattern}",
    "*" = "animal_string: {x$animal_string}"
  ))

  if (!is.null(x$column_map)) {
    map_str <- paste(
      names(x$column_map),
      "\u2192",
      vapply(
        x$column_map,
        function(v) if (length(v) == 1L) v else paste0("[", paste(v, collapse = ", "), "]"),
        character(1)
      ),
      collapse = ", "
    )
    cli::cli_bullets(c("*" = "column_map:    {map_str}"))
  } else {
    cli::cli_bullets(c("*" = "column_map:    (none)"))
  }

  if (!is.null(x$active_endpoint) && x$active_endpoint %in% names(x$endpoints)) {
    ep <- x$endpoints[[x$active_endpoint]]
    cli::cli_bullets(c("*" = "active_endpoint: {x$active_endpoint}"))
    if (!is.null(ep$alpha0_pop_mean)) {
      cli::cli_bullets(c(" " = "\u2514\u2500 alpha0_pop_mean: {ep$alpha0_pop_mean}"))
    }
    if (!is.null(ep$value_min) || !is.null(ep$value_max)) {
      cli::cli_bullets(c(
        " " = "\u2514\u2500 value_min: {ep$value_min}  value_max: {ep$value_max}"
      ))
    }
    if (!is.null(ep$threshold)) {
      cli::cli_bullets(c(" " = "\u2514\u2500 threshold: {ep$threshold}"))
    }
  } else if (length(x$endpoints) > 0L) {
    ep_names <- paste(names(x$endpoints), collapse = ", ")
    cli::cli_bullets(c("*" = "endpoints:     {ep_names} (no active endpoint)"))
  } else {
    cli::cli_bullets(c("*" = "active_endpoint: (none)"))
  }

  if (!is.null(x$time_parser)) {
    tp          <- x$time_parser
    units_str   <- if (length(tp$units) == 0L) "(none)" else paste(tp$units, collapse = ", ")
    cli::cli_bullets(c("*" = "time_parser:   delimiter={.val {tp$delimiter}}  units=[{units_str}]  allow_unitless={tp$allow_unitless}"))
  }

  invisible(x)
}


# read_study_config ----
#' Read a Study Configuration from a YAML File
#' @importFrom yaml read_yaml
#' @importFrom stats setNames
#' @param path character, path to the YAML file to read.
#' @return A `PriorRhythmConfig` object constructed from the YAML contents.
#' @seealso [study_config()], [write_study_config_template()]
#' @export
#' @example R/examples/Example_study_config.R
read_study_config <- function(path = NULL) {

  if (FALSE) {
    path <- "study_config.yaml"
  }

  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a non-empty character string.")
  }

  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  parsed <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      cli::cli_abort("Failed to parse YAML file {.path {path}}: {conditionMessage(e)}")
    }
  )

  if (!is.list(parsed)) {
    cli::cli_abort("YAML file {.path {path}} did not parse to a list.")
  }

  file_pattern_arg <- if (!is.null(parsed[["file_pattern"]])) {
    parsed[["file_pattern"]]
  } else {
    "\\.(csv|tsv)$"
  }

  animal_string_arg <- if (!is.null(parsed[["animal_string"]])) {
    parsed[["animal_string"]]
  } else {
    "monkey|dog"
  }

  column_map_arg <- if (!is.null(parsed[["column_map"]])) {
    raw_cm <- parsed[["column_map"]]
    # Each entry may be a scalar string (old) or a character vector (new multi-alias).
    # yaml::read_yaml returns scalars as length-1 vectors and sequences as lists,
    # so we normalise: if every value is length-1, return as named character vector
    # for backward compat; otherwise return as named list.
    vals <- lapply(raw_cm, function(v) as.character(unlist(v)))
    if (all(vapply(vals, length, integer(1)) == 1L)) {
      # All single-alias: return flat named character vector (backward compat)
      setNames(vapply(vals, `[[`, character(1), 1L), names(vals))
    } else {
      # At least one multi-alias: return named list
      vals
    }
  } else {
    NULL
  }

  endpoints_arg <- if (!is.null(parsed[["endpoints"]])) {
    parsed[["endpoints"]]
  } else {
    list()
  }

  active_endpoint_arg <- parsed[["active_endpoint"]]

  time_parser_arg <- if (!is.null(parsed[["time_parser"]])) {
    raw_tp <- parsed[["time_parser"]]
    # Normalise units: YAML may return a list; flatten to character vector
    if (!is.null(raw_tp[["units"]])) {
      raw_tp[["units"]] <- as.character(unlist(raw_tp[["units"]]))
    }
    raw_tp
  } else {
    list()
  }

  config <- study_config(
    file_pattern    = file_pattern_arg,
    animal_string   = animal_string_arg,
    column_map      = column_map_arg,
    endpoints       = endpoints_arg,
    active_endpoint = active_endpoint_arg,
    time_parser     = time_parser_arg
  )

  return(config)
}


# write_study_config_template ----
#' Write a Study Configuration Template YAML File
#' @param path character, file path where the template should be written.
#'   Default is `"study_config.yaml"`.
#' @param overwrite logical. If `TRUE`, overwrite an existing file. If
#'   `FALSE` (default), abort with an error if `path` already exists.
#' @return Invisibly returns `path`.
#' @seealso [study_config()], [read_study_config()]
#' @export
#' @example R/examples/Example_study_config.R
write_study_config_template <- function(path = "study_config.yaml", overwrite = FALSE) {

  if (FALSE) {
    path      <- file.path(tempdir(), "study_config.yaml")
    overwrite <- FALSE
  }

  if (file.exists(path) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "File already exists: {.path {path}}. \\
       Use {.code overwrite = TRUE} to overwrite."
    )
  }

  template <- paste0(
    "# PriorRhythm study configuration\n",
    "# Generated by write_study_config_template()\n",
    "# See ?study_config for full documentation.\n",
    "\n",
    "# File loading\n",
    "file_pattern:  \"\\\\.(csv|tsv)$\"\n",
    "animal_string: \"monkey|dog\"\n",
    "\n",
    "# Column renaming: map canonical column names to one or more source names.\n",
    "# Use a single string for one-to-one mapping, or a YAML list for multiple\n",
    "# possible source names (first match found in each file wins).\n",
    "column_map:\n",
    "  time:        \"summary_period\"               # single alias\n",
    "  activity:    \"a_mean\"\n",
    "  temperature: \"t_mean\"\n",
    "  # period_code: [Period_Code, \"Period Code\", period]   # multi-alias example\n",
    "\n",
    "# Endpoint-specific settings\n",
    "# Add one entry per endpoint label (must match the 'parameter' column after pivot).\n",
    "# Typical values: QTc ~250 ms, HR ~70 bpm, MBP ~80 mmHg\n",
    "endpoints:\n",
    "  qtc:\n",
    "    alpha0_pop_mean: 250   # baseline hyperprior mean (ms)\n",
    "    value_min:       150   # outlier filter lower bound\n",
    "    value_max:       400   # outlier filter upper bound\n",
    "    threshold:       10    # regulatory decision threshold\n",
    "  hr:\n",
    "    alpha0_pop_mean: 70\n",
    "    value_min:       30\n",
    "    value_max:       300\n",
    "    threshold:       10\n",
    "  mbp:\n",
    "    alpha0_pop_mean: 80\n",
    "    value_min:       40\n",
    "    value_max:       200\n",
    "    threshold:       10\n",
    "\n",
    "# Active endpoint (must be a key in `endpoints` above, or omit/null)\n",
    "active_endpoint: \"qtc\"\n",
    "\n",
    "# Time-interval parsing settings\n",
    "# Controls how time-bin labels (e.g. '0 to 3 hr') are split into numeric bounds.\n",
    "# delimiter:      string that separates the two numeric bounds (default: 'to')\n",
    "# units:          accepted unit suffixes; omit or use [] for none (default: [hr, hours])\n",
    "# allow_unitless: true = accept labels with no unit; false = require a suffix (default: true)\n",
    "time_parser:\n",
    "  delimiter:      \"to\"\n",
    "  units:          [hr, hours]\n",
    "  allow_unitless: true\n"
  )

  writeLines(template, con = path)

  cli::cli_inform("Study config template written to {.path {path}}")

  invisible(path)
}


# config_get_endpoint ----
# Internal helper: returns the endpoint sub-list for `endpoint`, falling
# back to config$active_endpoint. Returns list() if neither is set.
config_get_endpoint <- function(config = NULL, endpoint = NULL) {

  if (FALSE) {
    config   <- study_config(
      endpoints       = list(qtc = list(alpha0_pop_mean = 250)),
      active_endpoint = "qtc"
    )
    endpoint <- NULL
  }

  if (is.null(config)) {
    return(list())
  }

  ep_name <- if (!is.null(endpoint)) {
    endpoint
  } else {
    config$active_endpoint
  }

  if (is.null(ep_name) || !(ep_name %in% names(config$endpoints))) {
    return(list())
  }

  output <- config$endpoints[[ep_name]]

  return(output)
}
