library(PriorRhythm)

bundle_inventory <- list_prior_bundle_endpoints()
print(bundle_inventory)

if (FALSE) {
  custom_bundle_path <- "/path/to/custom_bundle.RDS"
  custom_inventory <- list_prior_bundle_endpoints(custom_bundle_path)
  print(custom_inventory)
}
