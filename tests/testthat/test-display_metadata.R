library(testthat)
library(PriorRhythm)

test_that("packaged endpoint descriptions parse from the installed package", {
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  expect_type(metadata, "list")
  expect_true("dog" %in% names(metadata))
  expect_equal(metadata$dog$label, "Dog")
  expect_equal(
    metadata$dog$endpoints$q_tc$label,
    "Corrected QT Interval (QTc)"
  )
  expect_equal(metadata$dog$endpoints$q_tc$unit, "ms")
})

test_that("packaged upload guidance parses from the installed package", {
  guidance <- PriorRhythm:::.read_upload_guidance()

  expect_type(guidance, "list")
  expect_equal(guidance$title, "Upload target-study data")
  expect_match(guidance$accepted_files, "CSV or TSV")
  expect_true(length(unlist(guidance$workflow, use.names = FALSE)) >= 3L)
})

test_that("display metadata returns friendly reviewed labels and units", {
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  display <- PriorRhythm:::.display_metadata(
    endpoint_descriptions = metadata,
    species = "nhp",
    endpoint = "rr_i"
  )

  expect_identical(display$species, "nhp")
  expect_identical(display$endpoint, "rr_i")
  expect_identical(display$species_label, "Non-human Primate")
  expect_identical(display$endpoint_label, "RR Interval")
  expect_identical(display$endpoint_unit, "ms")
  expect_identical(display$endpoint_display, "RR Interval [ms]")
})

test_that("display metadata falls back to raw codes for missing or malformed metadata", {
  missing_display <- PriorRhythm:::.display_metadata(
    endpoint_descriptions = list(),
    species = "custom_species",
    endpoint = "custom_endpoint"
  )

  malformed_display <- PriorRhythm:::.display_metadata(
    endpoint_descriptions = list(
      custom_species = list(
        label = 10,
        endpoints = list(
          custom_endpoint = "bad entry"
        )
      )
    ),
    species = "custom_species",
    endpoint = "custom_endpoint"
  )

  expect_identical(missing_display$species_label, "custom_species")
  expect_identical(missing_display$endpoint_label, "custom_endpoint")
  expect_null(missing_display$endpoint_unit)
  expect_identical(missing_display$endpoint_display, "custom_endpoint")

  expect_identical(malformed_display$species_label, "custom_species")
  expect_identical(malformed_display$endpoint_label, "custom_endpoint")
  expect_null(malformed_display$endpoint_unit)
  expect_identical(malformed_display$endpoint_display, "custom_endpoint")
})

test_that("display choice helpers keep raw values while showing friendly labels", {
  metadata <- PriorRhythm:::.read_endpoint_descriptions()

  species_choices <- PriorRhythm:::.display_species_choices(
    species_values = c("dog", "custom_species"),
    endpoint_descriptions = metadata
  )

  endpoint_choices <- PriorRhythm:::.display_endpoint_choices(
    species = "dog",
    endpoint_values = c("q_tc", "custom_endpoint"),
    endpoint_descriptions = metadata
  )

  expect_identical(unname(species_choices), c("dog", "custom_species"))
  expect_identical(names(species_choices), c("Dog", "custom_species"))

  expect_identical(unname(endpoint_choices), c("q_tc", "custom_endpoint"))
  expect_identical(
    names(endpoint_choices),
    c("Corrected QT Interval (QTc) [ms]", "custom_endpoint")
  )
})

test_that("upload guidance UI renders the YAML-driven CSV or TSV guidance", {
  guidance <- PriorRhythm:::.read_upload_guidance()

  html <- paste(as.character(PriorRhythm:::.upload_guidance_ui(guidance)), collapse = "\n")

  expect_match(html, "Upload target-study data", fixed = TRUE)
  expect_match(html, "Upload CSV or TSV files only.", fixed = TRUE)
  expect_match(html, "Expected analysis roles", fixed = TRUE)
  expect_match(html, "Typical target-study workflow", fixed = TRUE)
  expect_match(html, "The current app expects the selected time column", fixed = TRUE)
  expect_match(html, "The app does not validate this automatically.", fixed = TRUE)
  expect_no_match(html, ".xls", fixed = TRUE)
  expect_no_match(html, ".xlsx", fixed = TRUE)
})
