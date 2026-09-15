# Refresh the editable endpoint-description candidate from the active
# packaged prior bundle.
#
# Run interactively from the package repository root.
#
# Workflow:
#   1. Run this script.
#   2. Complete and save the YAML file that opens.
#   3. Review its Git diff and scientific wording.
#   4. When approved, manually promote the candidate to:
#        inst/config/endpoint_descriptions.yaml
#   5. Run this script again to confirm the reviewed YAML is retained.

# Prepare session ---------------------------------------------------------

rm(list = ls())
gc()

devtools::load_all()


# Discover current raw bundle keys ----------------------------------------

bundle_inventory <- PriorRhythm::list_prior_bundle_endpoints()


# Read reviewed content, when available -----------------------------------

reviewed_yaml_path <- "inst/config/endpoint_descriptions.yaml"
candidate_yaml_path <- "inst/config/endpoint_descriptions.candidate.yaml"

reviewed_descriptions <- NULL

if (file.exists(reviewed_yaml_path)) {
  reviewed_descriptions <- yaml::read_yaml(reviewed_yaml_path)
}


# Build and write editable candidate --------------------------------------

candidate_out <- PriorRhythm:::.build_endpoint_descriptions_candidate(
  bundle_inventory = bundle_inventory,
  reviewed_descriptions = reviewed_descriptions
)

yaml::write_yaml(
  candidate_out$candidate,
  file = candidate_yaml_path
)


# Explain next steps and open the candidate -------------------------------

cli::cli_inform(c(
  "v" = "Endpoint-description candidate written.",
  "i" = "Edit and save: {.path {candidate_yaml_path}}",
  "i" = paste0(
    "The candidate contains ",
    nrow(bundle_inventory),
    " raw species/endpoint entries from the active prior bundle."
  ),
  "i" = paste0(
    "Existing reviewed annotations were retained where their raw keys ",
    "are still available."
  ),
  "i" = paste0(
    "After scientific/content review, manually promote the candidate to: ",
    "{.path {reviewed_yaml_path}}"
  )
))

file.edit(candidate_yaml_path)
