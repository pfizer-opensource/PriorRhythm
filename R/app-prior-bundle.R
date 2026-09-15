# validate_prior_bundle ----

#' Resolve and Validate a Prior Bundle
#'
#' Resolves the path to a prior bundle RDS file and validates its structure.
#' When `prior_file` is `NULL`, the bundled artifact shipped with the package
#' is used. When `prior_file` is a non-NULL character string it is treated as
#' a trusted local path and must exist and pass validation before use.
#'
#' An explicit `prior_file` that is missing, unreadable, or structurally
#' incompatible aborts with an actionable error; the function never silently
#' falls back to the bundled priors when a custom path has been supplied.
#'
#' **Bundle schema** — two shapes are accepted and normalised to the same
#' internal form:
#'
#' *Versioned envelope (preferred):*
#' ```
#' list(
#'   schema_version = 1L,          # required for versioned shape
#'   metadata       = list(...),   # optional non-confidential metadata
#'   priors         = list(
#'     <species> = list(
#'       <endpoint> = list(
#'         pa          = <data.frame with columns alpha0, A0, beta0, phi0>,
#'         diagnostics = <list, see below>
#'       )
#'     )
#'   )
#' )
#' ```
#'
#' *Legacy flat shape (backward compatible):*
#' ```
#' list(
#'   <species> = list(
#'     <endpoint> = list(
#'       pa          = <data.frame with columns alpha0, A0, beta0, phi0>,
#'       diagnostics = <list, see below>
#'     )
#'   ),
#'   schema_version = ...,   # recognized metadata — not treated as a species
#'   metadata       = ...    # recognized metadata — not treated as a species
#' )
#' ```
#'
#' In both cases the returned object has the form:
#' ```
#' list(
#'   schema_version = <integer or NULL>,
#'   metadata       = <list or NULL>,
#'   priors         = list(<species> = list(<endpoint> = list(pa = ..., diagnostics = ...)))
#' )
#' ```
#'
#' Every endpoint entry must contain a `diagnostics` component recording the
#' non-confidential, chain-aware convergence evidence produced during the
#' controlled Stage 1 build/refresh workflow (see
#' `summarize_convergence_diagnostics()`). It must be a named list with:
#' `parameters` (character vector), `rhat` and `ess` (named numeric vectors
#' aligned to `parameters`), `status` (one of `"pass"`, `"acceptable"`,
#' `"failed"`, `"unassessed"`), `policy` (a named list with `rhat_target`,
#' `rhat_acceptable`, `ess_target`), `assessed_at` (scalar character), and
#' `package_version` (scalar character). This `diagnostics` object is a
#' recorded summary from the controlled build — it is not, and cannot be,
#' recomputed at runtime from the pooled `pa` draws alone (chain identity is
#' not retained in `pa`).
#'
#' Additional list components beyond `pa`/`diagnostics` at the endpoint level
#' (e.g. model objects) are permitted and preserved.
#'
#' @param prior_file `NULL` or a character string giving the path to a local
#'   RDS file containing a compatible prior bundle. When `NULL` the bundled
#'   artifact at `inst/extdata/priors/list_historical_prior.RDS` is used.
#' @return A normalised and validated named list with keys `schema_version`,
#'   `metadata`, and `priors`.
#' @importFrom cli cli_abort cli_inform
#' @keywords internal
#' @noRd
resolve_prior_bundle <- function(prior_file = NULL) {
  
  if (FALSE) {
    prior_file <- NULL
    prior_file <- "/path/to/my_priors.RDS"
  }
  
  if (is.null(prior_file)) {
    bundled_path <- system.file(
      "extdata", "priors", "list_historical_prior.RDS",
      package = "PriorRhythm",
      mustWork = FALSE
    )
    if (!nzchar(bundled_path) || !file.exists(bundled_path)) {
      cli::cli_abort(c(
        "The bundled prior artifact is missing from the installed package.",
        "i" = "Expected location: {.path inst/extdata/priors/list_historical_prior.RDS}",
        "i" = "Re-install the package or contact the package maintainer."
      ))
    }
    bundle <- .read_prior_rds(bundled_path)
    bundle <- validate_prior_bundle(bundle, path = bundled_path)
    return(bundle)
  }
  
  if (!is.character(prior_file) || length(prior_file) != 1L || !nzchar(prior_file)) {
    cli::cli_abort(c(
      "{.arg prior_file} must be a single non-empty character string or {.val NULL}.",
      "x" = "Got: {.obj_type_friendly {prior_file}}"
    ))
  }
  
  if (!file.exists(prior_file)) {
    cli::cli_abort(c(
      "Custom prior file not found.",
      "x" = "Path: {.path {prior_file}}",
      "i" = "Check that the file exists and the path is correct."
    ))
  }
  
  bundle <- .read_prior_rds(prior_file)
  bundle <- validate_prior_bundle(bundle, path = prior_file)
  
  cli::cli_inform(c(
    "v" = "Custom prior bundle loaded from {.path {prior_file}}."
  ))
  
  return(bundle)
}


#' Validate and Normalise a Prior Bundle Object
#'
#' Accepts either the versioned-envelope shape
#' (`list(schema_version, metadata, priors = list(...))`) or the legacy flat
#' shape (top-level species keys, with optional recognized metadata keys).
#' Validates the prior tree structure and returns a normalised object with
#' keys `schema_version`, `metadata`, and `priors`.
#'
#' Recognized top-level metadata keys (not treated as species):
#' `schema_version`, `metadata`, `bundle_id`, `created_with`, `created_at`,
#' `package_version`.
#'
#' @param bundle An R object to validate.
#' @param path Character string. The file path from which `bundle` was read,
#'   used only in error messages.
#' @return A normalised list with keys `schema_version`, `metadata`, and
#'   `priors` if valid.
#' @importFrom cli cli_abort
#' @importFrom utils modifyList
#' @keywords internal
#' @noRd
validate_prior_bundle <- function(bundle = NULL, path = "<unknown>") {
  
  if (FALSE) {
    bundle <- list(dog = list(q_tc = list(pa = data.frame(
      alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1))))
    path <- "/tmp/test.RDS"
  }
  
  if (!is.list(bundle) || is.null(names(bundle)) || length(bundle) == 0L) {
    cli::cli_abort(c(
      "Prior bundle must be a non-empty named list.",
      "x" = "File: {.path {path}}"
    ))
  }
  
  # Recognized top-level metadata keys — never treated as species
  metadata_keys <- c(
    "schema_version", "metadata", "bundle_id",
    "created_with", "created_at", "package_version"
  )
  
  # Currently supported schema versions for the versioned-envelope format
  supported_schema_versions <- 1L
  
  # Detect versioned-envelope shape: has a "priors" key
  if (!is.null(bundle[["priors"]])) {
    sv <- bundle[["schema_version"]]
    
    if (is.null(sv)) {
      cli::cli_abort(c(
        "Versioned-envelope prior bundle is missing {.field schema_version}.",
        "x" = "File: {.path {path}}",
        "i" = "Add {.code schema_version = {supported_schema_versions}} to the bundle."
      ))
    }
    
    if (!is.integer(sv) || length(sv) != 1L || is.na(sv)) {
      cli::cli_abort(c(
        "{.field schema_version} must be a scalar integer.",
        "x" = "Got {.cls {class(sv)}} of length {length(sv)}: {.val {sv}}",
        "i" = "File: {.path {path}}"
      ))
    }
    
    if (!sv %in% supported_schema_versions) {
      cli::cli_abort(c(
        "Unsupported {.field schema_version} {.val {sv}}.",
        "x" = "File: {.path {path}}",
        "i" = "Supported version(s): {supported_schema_versions}. \\
               Update {.pkg PriorRhythm} to read this bundle."
      ))
    }
    
    priors_tree <- bundle[["priors"]]
    schema_version <- sv
    metadata_obj <- bundle[["metadata"]]
  } else {
    # Legacy flat shape: separate metadata keys from species keys
    species_keys <- setdiff(names(bundle), metadata_keys)
    if (length(species_keys) == 0L) {
      cli::cli_abort(c(
        "Prior bundle contains no species entries.",
        "x" = "File: {.path {path}}",
        "i" = "Expected either a {.field priors} envelope or top-level species keys."
      ))
    }
    priors_tree <- bundle[species_keys]
    schema_version <- bundle[["schema_version"]]
    
    # Collect any recognized top-level scalar metadata (all keys except
    # schema_version, metadata itself, and species keys) into a flat list,
    # then merge with the nested metadata — nested metadata takes precedence.
    scalar_meta_keys <- setdiff(
      metadata_keys,
      c("schema_version", "metadata")
    )
    top_level_meta <- bundle[intersect(scalar_meta_keys, names(bundle))]
    nested_meta    <- if (is.list(bundle[["metadata"]])) bundle[["metadata"]] else list()
    metadata_obj   <- utils::modifyList(top_level_meta, nested_meta)
    if (length(metadata_obj) == 0L) metadata_obj <- NULL
  }
  
  # Validate the priors tree
  .validate_priors_tree(priors_tree, path = path)
  
  # Return normalised form
  normalised <- list(
    schema_version = schema_version,
    metadata       = metadata_obj,
    priors         = priors_tree
  )
  
  return(normalised)
}


#' Validate the Priors Tree
#'
#' Internal helper that validates the species → endpoint → pa structure.
#'
#' @param priors_tree Named list of species.
#' @param path Character string used in error messages.
#' @return `invisible(NULL)` if valid; aborts otherwise.
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
.validate_priors_tree <- function(priors_tree = NULL, path = "<unknown>") {
  
  if (FALSE) {
    priors_tree <- list(dog = list(q_tc = list(pa = data.frame(
      alpha0 = 1, A0 = 1, beta0 = 1, phi0 = 1))))
    path <- "/tmp/test.RDS"
  }
  
  if (!is.list(priors_tree) || is.null(names(priors_tree)) || length(priors_tree) == 0L) {
    cli::cli_abort(c(
      "Prior bundle must contain at least one species with endpoint data.",
      "x" = "File: {.path {path}}"
    ))
  }
  
  required_pa_cols <- c("alpha0", "A0", "beta0", "phi0")
  
  for (species in names(priors_tree)) {
    sp_obj <- priors_tree[[species]]
    
    if (!is.list(sp_obj) || is.null(names(sp_obj)) || length(sp_obj) == 0L) {
      cli::cli_abort(c(
        "Each species entry must be a non-empty named list of endpoints.",
        "x" = "Species {.val {species}} in {.path {path}} is not a named list."
      ))
    }
    
    for (endpoint in names(sp_obj)) {
      ep_obj <- sp_obj[[endpoint]]
      
      if (!is.list(ep_obj)) {
        cli::cli_abort(c(
          "Each endpoint entry must be a list.",
          "x" = "Species {.val {species}}, endpoint {.val {endpoint}} in",
          " " = "{.path {path}} is not a list."
        ))
      }
      
      if (is.null(ep_obj[["pa"]])) {
        cli::cli_abort(c(
          "Each endpoint entry must contain a {.field pa} component.",
          "x" = "Missing {.field pa} for species {.val {species}},",
          " " = "endpoint {.val {endpoint}} in {.path {path}}."
        ))
      }
      
      pa <- ep_obj[["pa"]]
      
      if (!is.data.frame(pa)) {
        cli::cli_abort(c(
          "{.field pa} must be a data frame.",
          "x" = "Species {.val {species}}, endpoint {.val {endpoint}} in",
          " " = "{.path {path}}: {.field pa} is {.obj_type_friendly {pa}}."
        ))
      }
      
      missing_cols <- setdiff(required_pa_cols, names(pa))
      if (length(missing_cols) > 0L) {
        cli::cli_abort(c(
          "{.field pa} is missing required posterior parameter columns.",
          "x" = "Missing: {.val {missing_cols}}",
          "i" = "Species {.val {species}}, endpoint {.val {endpoint}} in",
          " " = "{.path {path}}."
        ))
      }
      
      # Non-confidential convergence evidence recorded during the
      # controlled build/refresh workflow. Required for every endpoint —
      # this package has no diagnostics-free bundle format to stay
      # compatible with.
      .validate_diagnostics(
        ep_obj[["diagnostics"]],
        species  = species,
        endpoint = endpoint,
        path     = path
      )
      
      confidential_fields <- c(
        "animal", "subject", "study", "source_file", "raw_data"
      )
      present_confidential <- intersect(confidential_fields, names(pa))
      if (length(present_confidential) > 0L) {
        cli::cli_abort(c(
          "Prior bundle contains raw historical data fields.",
          "x" = "Confidential columns detected in {.field pa}:",
          " " = "{.val {present_confidential}}",
          "i" = "Species {.val {species}}, endpoint {.val {endpoint}} in",
          " " = "{.path {path}}.",
          "i" = "Remove historical observation columns before packaging."
        ))
      }
      
      # Also inspect every other component of the endpoint entry for
      # confidential fields, to prevent raw historical data from being
      # smuggled in outside of `pa`.
      other_keys <- setdiff(names(ep_obj), "pa")
      for (other_key in other_keys) {
        found <- .find_confidential_in_obj(ep_obj[other_key], confidential_fields)
        if (length(found) > 0L) {
          cli::cli_abort(c(
            "Prior bundle contains raw historical data outside {.field pa}.",
            "x" = "Confidential field(s) {.val {found}} detected in",
            " " = "{.field {other_key}} for species {.val {species}},",
            " " = "endpoint {.val {endpoint}} in {.path {path}}.",
            "i" = "Remove all historical observation data before packaging."
          ))
        }
      }
    }
  }
  
  return(invisible(NULL))
}


#' Validate an Endpoint's Diagnostics Object
#'
#' Internal helper that validates the required endpoint-level `diagnostics`
#' component: the recorded, non-confidential, chain-aware convergence
#' evidence computed once during the controlled build/refresh workflow (see
#' `summarize_convergence_diagnostics()`). Never recomputed at runtime.
#'
#' @param diagnostics The candidate `diagnostics` object (or `NULL`).
#' @param species,endpoint Scalar character used only in error messages.
#' @param path Character string used in error messages.
#' @return `invisible(NULL)` if valid; aborts otherwise.
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
.validate_diagnostics <- function(diagnostics = NULL,
                                  species     = NULL,
                                  endpoint    = NULL,
                                  path        = "<unknown>") {
  
  if (FALSE) {
    diagnostics <- list(
      parameters      = c("alpha0", "A0", "beta0", "phi0"),
      rhat            = stats::setNames(rep(1.001, 4), c("alpha0", "A0", "beta0", "phi0")),
      ess             = stats::setNames(rep(900, 4), c("alpha0", "A0", "beta0", "phi0")),
      status          = "pass",
      policy          = list(rhat_target = 1.01, rhat_acceptable = 1.1, ess_target = 400),
      assessed_at     = "2026-01-01 00:00:00 UTC",
      package_version = "0.0.0.63"
    )
    species  <- "dog"
    endpoint <- "q_tc"
    path     <- "/tmp/test.RDS"
  }
  
  loc <- c(
    "i" = "Species {.val {species}}, endpoint {.val {endpoint}} in {.path {path}}."
  )
  
  if (is.null(diagnostics)) {
    cli::cli_abort(c(
      "Each endpoint entry must contain a {.field diagnostics} component.",
      "x" = "Missing {.field diagnostics} for species {.val {species}},",
      " " = "endpoint {.val {endpoint}} in {.path {path}}.",
      "i" = "Record convergence evidence with {.fun summarize_convergence_diagnostics} \\
             before packaging."
    ))
  }
  
  if (!is.list(diagnostics) || is.null(names(diagnostics))) {
    cli::cli_abort(c(
      "{.field diagnostics} must be a named list.",
      "x" = "{.field diagnostics} is {.obj_type_friendly {diagnostics}}.",
      loc
    ))
  }
  
  required_fields <- c(
    "parameters", "rhat", "ess", "status", "policy", "assessed_at",
    "package_version"
  )
  missing_fields <- setdiff(required_fields, names(diagnostics))
  if (length(missing_fields) > 0L) {
    cli::cli_abort(c(
      "{.field diagnostics} is missing required fields.",
      "x" = "Missing: {.val {missing_fields}}",
      loc
    ))
  }
  
  parameters <- diagnostics[["parameters"]]
  if (!is.character(parameters) || length(parameters) == 0L || anyNA(parameters)) {
    cli::cli_abort(c(
      "{.field diagnostics$parameters} must be a non-empty character vector.",
      loc
    ))
  }
  
  for (field in c("rhat", "ess")) {
    val <- diagnostics[[field]]
    if (!is.numeric(val) || is.null(names(val))) {
      cli::cli_abort(c(
        "{.field diagnostics${field}} must be a named numeric vector.",
        "x" = "Got {.obj_type_friendly {val}}.",
        loc
      ))
    }
    if (!setequal(names(val), parameters)) {
      cli::cli_abort(c(
        "{.field diagnostics${field}} names must match {.field diagnostics$parameters}.",
        "x" = "{.field {field}} names: {.val {names(val)}}",
        " " = "{.field parameters}: {.val {parameters}}",
        loc
      ))
    }
  }
  
  status <- diagnostics[["status"]]
  valid_statuses <- c("pass", "acceptable", "failed", "unassessed")
  if (!is.character(status) || length(status) != 1L || is.na(status) ||
      !status %in% valid_statuses) {
    cli::cli_abort(c(
      "{.field diagnostics$status} must be one of {.val {valid_statuses}}.",
      "x" = "Got: {.val {status}}",
      loc
    ))
  }
  
  policy <- diagnostics[["policy"]]
  required_policy_fields <- c("rhat_target", "rhat_acceptable", "ess_target")
  if (!is.list(policy) || is.null(names(policy)) ||
      length(setdiff(required_policy_fields, names(policy))) > 0L) {
    cli::cli_abort(c(
      "{.field diagnostics$policy} must be a named list with fields \\
       {.val {required_policy_fields}}.",
      loc
    ))
  }
  for (field in required_policy_fields) {
    if (!is.numeric(policy[[field]]) || length(policy[[field]]) != 1L ||
        is.na(policy[[field]]) || !is.finite(policy[[field]])) {
      cli::cli_abort(c(
        "{.field diagnostics$policy${field}} must be a single finite numeric value.",
        "x" = "Got: {.val {policy[[field]]}}",
        loc
      ))
    }
  }
  
  if (!(policy[["ess_target"]] > 0)) {
    cli::cli_abort(c(
      "{.field diagnostics$policy$ess_target} must be greater than 0.",
      "x" = "Got: {.val {policy[['ess_target']]}}",
      loc
    ))
  }
  
  if (!(policy[["rhat_target"]] > 0)) {
    cli::cli_abort(c(
      "{.field diagnostics$policy$rhat_target} must be greater than 0.",
      "x" = "Got: {.val {policy[['rhat_target']]}}",
      loc
    ))
  }
  
  if (!(policy[["rhat_target"]] < policy[["rhat_acceptable"]])) {
    cli::cli_abort(c(
      "{.field diagnostics$policy$rhat_target} must be less than \\
       {.field diagnostics$policy$rhat_acceptable}.",
      "x" = "Got rhat_target = {.val {policy[['rhat_target']]}}, \\
             rhat_acceptable = {.val {policy[['rhat_acceptable']]}}",
      loc
    ))
  }
  
  for (field in c("assessed_at", "package_version")) {
    val <- diagnostics[[field]]
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      cli::cli_abort(c(
        "{.field diagnostics${field}} must be a single non-empty character string.",
        "x" = "Got: {.obj_type_friendly {val}}",
        loc
      ))
    }
  }
  
  rhat <- diagnostics[["rhat"]]
  ess  <- diagnostics[["ess"]]
  
  if (!all(is.finite(ess)) || !all(ess > 0)) {
    cli::cli_abort(c(
      "{.field diagnostics$ess} must contain only finite, positive values.",
      "x" = "Got: {.val {ess}}",
      loc
    ))
  }
  
  non_missing_rhat <- rhat[!is.na(rhat)]
  if (length(non_missing_rhat) > 0L && !all(is.finite(non_missing_rhat))) {
    cli::cli_abort(c(
      "{.field diagnostics$rhat} must contain only finite values or {.val NA} \\
       (missing R-hat is only permitted when {.field diagnostics$status} is \\
       {.val unassessed}).",
      "x" = "Got: {.val {rhat}}",
      loc
    ))
  }
  
  expected_status <- .convergence_status(rhat = rhat, ess = ess, policy = policy)
  if (!identical(expected_status, status)) {
    cli::cli_abort(c(
      "{.field diagnostics$status} is inconsistent with the recorded {.field rhat}/{.field ess}/{.field policy}.",
      "x" = "Recorded status: {.val {status}}; expected from evidence: {.val {expected_status}}.",
      "i" = "Missing (NA) {.field rhat} is only consistent with status {.val unassessed}.",
      loc
    ))
  }
  
  return(invisible(NULL))
}


#' Recursively Search an Object for Confidential Field Names
#'
#' Walks any R object depth-first. Rejects confidential names at two levels:
#' \enumerate{
#'   \item Named list element names (e.g. `raw_data = numeric(5)` triggers a
#'     match regardless of the element's type).
#'   \item Data frame column names encountered anywhere in the object tree.
#' }
#' Stops early once at least one confidential name is found to avoid
#' unnecessary traversal.
#'
#' @param obj An R object.
#' @param confidential_fields Character vector of field names to reject.
#' @return Character vector of confidential field names found (may be empty).
#' @keywords internal
#' @noRd
.find_confidential_in_obj <- function(obj = NULL, confidential_fields = NULL) {
  
  if (FALSE) {
    obj <- list(raw_data = numeric(5))
    confidential_fields <- c("animal", "subject", "study", "source_file", "raw_data")
  }
  
  if (is.data.frame(obj)) {
    found <- intersect(confidential_fields, names(obj))
    return(found)
  }
  
  if (is.list(obj)) {
    # Check the names of this list's elements — a confidential name is a
    # violation regardless of what value it holds.
    if (!is.null(names(obj))) {
      found_names <- intersect(confidential_fields, names(obj))
      if (length(found_names) > 0L) {
        return(found_names)
      }
    }
    # Recurse into each element.
    for (item in obj) {
      found <- .find_confidential_in_obj(item, confidential_fields)
      if (length(found) > 0L) {
        return(found)
      }
    }
  }
  
  return(character(0))
}


#' Read a Prior RDS File Safely
#'
#' @param path Character string. Path to an RDS file.
#' @return The deserialized R object.
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
.read_prior_rds <- function(path = NULL) {
  
  if (FALSE) {
    path <- "/tmp/test.RDS"
  }
  
  bundle <- tryCatch(
    readRDS(path),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to read prior bundle.",
        "x" = "Path: {.path {path}}",
        "i" = "Original error: {conditionMessage(e)}"
      ))
    }
  )
  
  return(bundle)
}

#' List Species/Endpoint Inventory from a Prior Bundle
#'
#' Returns one row per available species/endpoint prior in a compatible bundle,
#' exposing only non-confidential inventory fields suitable for review
#' workflows.
#'
#' When `prior_file` is `NULL`, the bundled prior artifact shipped with
#' `PriorRhythm` is loaded. When `prior_file` is a non-NULL path, it is
#' validated through `resolve_prior_bundle()`.
#'
#' @param prior_file `NULL` or a single non-empty character path to a
#'   compatible prior bundle RDS file.
#' @return A data frame with columns `species`, `endpoint`, and
#'   `convergence_status`, containing one row per species/endpoint combination.
#' @example R/examples/Example_list_prior_bundle_endpoints.R
#' @export
list_prior_bundle_endpoints <- function(prior_file = NULL) {
  
  if (FALSE) {
    prior_file <- NULL
    prior_file <- "/path/to/custom_bundle.RDS"
  }
  
  bundle <- resolve_prior_bundle(prior_file = prior_file)
  priors <- bundle[["priors"]]
  
  species_out <- character(0)
  endpoint_out <- character(0)
  status_out <- character(0)
  
  for (species in names(priors)) {
    for (endpoint in names(priors[[species]])) {
      diagnostics <- priors[[species]][[endpoint]][["diagnostics"]]
      convergence_status <- diagnostics[["status"]]
      
      species_out <- c(species_out, species)
      endpoint_out <- c(endpoint_out, endpoint)
      status_out <- c(status_out, convergence_status)
    }
  }
  
  output <- data.frame(
    species = species_out,
    endpoint = endpoint_out,
    convergence_status = status_out,
    stringsAsFactors = FALSE
  )
  
  output <- output[order(output$species, output$endpoint), , drop = FALSE]
  rownames(output) <- NULL
  
  return(output)
}

.build_blank_endpoint_descriptions <- function(bundle_inventory = NULL) {
  
  if (FALSE) {
    bundle_inventory <- data.frame(
      species = c("dog", "nhp"),
      endpoint = c("q_tc", "hr"),
      convergence_status = c("pass", "acceptable"),
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.data.frame(bundle_inventory)) {
    cli::cli_abort("{.arg bundle_inventory} must be a data frame.")
  }
  
  required_cols <- c("species", "endpoint")
  missing_cols <- setdiff(required_cols, names(bundle_inventory))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg bundle_inventory} is missing required column(s): {.val {missing_cols}}."
    )
  }
  
  unique_keys <- unique(bundle_inventory[, required_cols, drop = FALSE])
  unique_keys <- unique_keys[order(unique_keys$species, unique_keys$endpoint), , drop = FALSE]
  
  output <- list()
  
  for (species in unique(unique_keys$species)) {
    species_rows <- unique_keys[unique_keys$species == species, , drop = FALSE]
    endpoints <- as.character(species_rows$endpoint)
    
    endpoint_entries <- list()
    for (endpoint in endpoints) {
      endpoint_entries[[endpoint]] <- list(
        label = "",
        unit = "",
        description = ""
      )
    }
    
    output[[species]] <- list(
      label = "",
      endpoints = endpoint_entries
    )
  }
  
  return(output)
}

.normalise_annotation_text <- function(value = NULL) {
  
  if (FALSE) {
    value <- "Label"
  }
  
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    output <- ""
    return(output)
  }
  
  if (!nzchar(trimws(value))) {
    output <- ""
    return(output)
  }
  
  output <- value
  return(output)
}

.key_lookup <- function(keys = NULL) {
  
  if (FALSE) {
    keys <- data.frame(species = "dog", endpoint = "q_tc", stringsAsFactors = FALSE)
  }
  
  if (!is.data.frame(keys) || !all(c("species", "endpoint") %in% names(keys))) {
    cli::cli_abort("{.arg keys} must be a data frame with {.field species} and {.field endpoint}.")
  }
  
  if (nrow(keys) == 0L) {
    output <- character(0)
    return(output)
  }
  
  output <- paste(keys$species, keys$endpoint, sep = "::")
  return(output)
}

.build_endpoint_descriptions_candidate <- function(
    bundle_inventory = NULL,
    reviewed_descriptions = NULL
) {
  
  if (FALSE) {
    bundle_inventory <- data.frame(
      species = c("dog", "dog", "nhp"),
      endpoint = c("q_tc", "hr", "q_tc"),
      convergence_status = c("pass", "pass", "acceptable"),
      stringsAsFactors = FALSE
    )
    reviewed_descriptions <- list(
      dog = list(
        label = "Dog",
        endpoints = list(
          q_tc = list(label = "QTc", unit = "ms", description = "QT corrected")
        )
      )
    )
  }
  
  blank_candidate <- .build_blank_endpoint_descriptions(bundle_inventory = bundle_inventory)
  
  if (is.null(reviewed_descriptions)) {
    reviewed_descriptions <- list()
  }
  
  if (!is.list(reviewed_descriptions)) {
    cli::cli_abort("{.arg reviewed_descriptions} must be a list or {.val NULL}.")
  }
  
  candidate <- blank_candidate
  
  bundle_keys <- unique(bundle_inventory[, c("species", "endpoint"), drop = FALSE])
  bundle_keys <- bundle_keys[order(bundle_keys$species, bundle_keys$endpoint), , drop = FALSE]
  
  reviewed_keys_species <- character(0)
  reviewed_keys_endpoint <- character(0)
  
  for (species in names(reviewed_descriptions)) {
    species_entry <- reviewed_descriptions[[species]]
    
    if (!is.list(species_entry)) {
      next
    }
    
    reviewed_keys_species <- c(reviewed_keys_species, species)
    reviewed_keys_endpoint <- c(reviewed_keys_endpoint, NA_character_)
    
    endpoints_entry <- species_entry[["endpoints"]]
    if (is.list(endpoints_entry)) {
      endpoint_names <- names(endpoints_entry)
      reviewed_keys_species <- c(reviewed_keys_species, rep(species, length(endpoint_names)))
      reviewed_keys_endpoint <- c(reviewed_keys_endpoint, endpoint_names)
    }
  }
  
  reviewed_keys <- data.frame(
    species = reviewed_keys_species,
    endpoint = reviewed_keys_endpoint,
    stringsAsFactors = FALSE
  )
  
  for (species in names(candidate)) {
    reviewed_species <- reviewed_descriptions[[species]]
    
    if (is.list(reviewed_species)) {
      candidate[[species]][["label"]] <- .normalise_annotation_text(reviewed_species[["label"]])
      
      reviewed_endpoints <- reviewed_species[["endpoints"]]
      if (is.list(reviewed_endpoints)) {
        for (endpoint in names(candidate[[species]][["endpoints"]])) {
          reviewed_endpoint <- reviewed_endpoints[[endpoint]]
          if (is.list(reviewed_endpoint)) {
            candidate[[species]][["endpoints"]][[endpoint]][["label"]] <-
              .normalise_annotation_text(reviewed_endpoint[["label"]])
            candidate[[species]][["endpoints"]][[endpoint]][["unit"]] <-
              .normalise_annotation_text(reviewed_endpoint[["unit"]])
            candidate[[species]][["endpoints"]][[endpoint]][["description"]] <-
              .normalise_annotation_text(reviewed_endpoint[["description"]])
          }
        }
      }
    }
  }
  
  reviewed_key_lookup <- .key_lookup(reviewed_keys)
  bundle_key_lookup <- .key_lookup(bundle_keys)
  
  new_rows <- !bundle_key_lookup %in% reviewed_key_lookup
  new_keys <- bundle_keys[new_rows, , drop = FALSE]
  rownames(new_keys) <- NULL
  
  reviewed_endpoint_keys <- reviewed_keys[!is.na(reviewed_keys$endpoint), , drop = FALSE]
  reviewed_endpoint_lookup <- .key_lookup(reviewed_endpoint_keys)
  
  stale_rows <- !reviewed_endpoint_lookup %in% bundle_key_lookup
  stale_keys <- reviewed_endpoint_keys[stale_rows, , drop = FALSE]
  
  bundle_species <- unique(bundle_keys$species)
  reviewed_species <- unique(reviewed_keys$species)
  stale_species <- setdiff(reviewed_species, bundle_species)
  if (length(stale_species) > 0L) {
    stale_species_keys <- data.frame(
      species = stale_species,
      endpoint = NA_character_,
      stringsAsFactors = FALSE
    )
    stale_keys <- rbind(stale_keys, stale_species_keys)
  }
  
  stale_keys <- stale_keys[order(stale_keys$species, stale_keys$endpoint), , drop = FALSE]
  rownames(stale_keys) <- NULL
  
  incomplete_species <- character(0)
  incomplete_endpoint <- character(0)
  missing_fields <- character(0)
  
  for (species in names(candidate)) {
    species_label <- .normalise_annotation_text(candidate[[species]][["label"]])
    
    for (endpoint in names(candidate[[species]][["endpoints"]])) {
      endpoint_entry <- candidate[[species]][["endpoints"]][[endpoint]]
      endpoint_label <- .normalise_annotation_text(endpoint_entry[["label"]])
      endpoint_unit <- .normalise_annotation_text(endpoint_entry[["unit"]])
      endpoint_description <- .normalise_annotation_text(endpoint_entry[["description"]])
      
      missing <- character(0)
      if (!nzchar(species_label)) {
        missing <- c(missing, "species_label")
      }
      if (!nzchar(endpoint_label)) {
        missing <- c(missing, "label")
      }
      if (!nzchar(endpoint_unit)) {
        missing <- c(missing, "unit")
      }
      if (!nzchar(endpoint_description)) {
        missing <- c(missing, "description")
      }
      
      if (length(missing) > 0L) {
        incomplete_species <- c(incomplete_species, species)
        incomplete_endpoint <- c(incomplete_endpoint, endpoint)
        missing_fields <- c(missing_fields, paste(missing, collapse = ","))
      }
    }
  }
  
  incomplete_keys <- data.frame(
    species = incomplete_species,
    endpoint = incomplete_endpoint,
    missing_fields = missing_fields,
    stringsAsFactors = FALSE
  )
  
  if (nrow(incomplete_keys) > 0L) {
    incomplete_keys <- incomplete_keys[
      order(incomplete_keys$species, incomplete_keys$endpoint),
      ,
      drop = FALSE
    ]
    rownames(incomplete_keys) <- NULL
  }
  
  output <- list(
    candidate = candidate,
    new_keys = new_keys,
    stale_keys = stale_keys,
    incomplete_keys = incomplete_keys
  )
  
  return(output)
}
