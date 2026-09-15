# PriorRhythm 0.0.0.73

## Improvements

* The Shiny app now loads reviewed species/endpoint display metadata and
  file-upload guidance from packaged YAML once per session, shows friendly
  labels and units while preserving raw prior-bundle keys for bundle lookup,
  and safely falls back to raw codes for valid custom prior bundles without
  packaged metadata.
* Documentation now describes the CSV/TSV upload contract and the user's
  responsibility to confirm species, endpoint-definition, and
  measurement-unit compatibility.

## Tests

* Added coverage for packaged YAML parsing, display-metadata fallback behavior,
  Shiny selector raw-value preservation, bundled-prior metadata rendering, and
  file-upload guidance rendering.

# PriorRhythm 0.0.0.72

## New Functions

* Added `list_prior_bundle_endpoints(prior_file = NULL)`, a read-only API that
  reports the available species/endpoint combinations and convergence status in
  a validated prior bundle.

## Improvements

* Added packaged endpoint-description metadata for reviewed display labels and
  units.

# PriorRhythm 0.0.0.71

## Improvements

* Target-study loading and the File Upload UI support CSV and TSV input only;
  Excel workbook input is not supported.

# PriorRhythm 0.0.0.70

## Improvements

* Added Results-sidebar controls for figure typography and aspect ratio so
  credible-interval and probability plots can be formatted for publication.

# PriorRhythm 0.0.0.69

## Documentation

* Consolidated the maintained workflow documentation into
  `vignettes/bayesian-priors-workflow.Rmd`.
* Documented the Shiny application as a runtime that uses a reviewed bundled
  prior artifact by default and does not build priors from raw historical data
  during an app session.
* Clarified synthetic demonstration-data use, prior compatibility
  responsibilities, convergence-evidence limitations, and supported
  CSV/TSV target-study input.

# PriorRhythm 0.0.0.68

## Improvements

* `load_study_data()` accepts an optional `PriorRhythmConfig` object for
  configuration-backed column mapping and animal-string resolution while
  preserving explicit-argument precedence.

# PriorRhythm 0.0.0.67

## Refactoring

* Removed the unreleased experimental Stan backend. Stage 1 prior construction
  uses JAGS.

# PriorRhythm 0.0.0.64–0.0.0.66

## Improvements

* Added convergence-policy validation and compact endpoint-level convergence
  evidence to runtime prior bundles.
* Improved time-interval parsing, study-data validation, configuration support,
  and user-facing model terminology.

# PriorRhythm 0.0.0.59

## Historical note

Earlier internal development history is retained in the private development
repository and is intentionally omitted from this public-source candidate.
