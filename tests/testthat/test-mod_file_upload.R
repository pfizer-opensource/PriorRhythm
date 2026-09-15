library(testthat)
library(PriorRhythm)

test_that("file upload sidebar accepts CSV and TSV only", {
  html <- as.character(PriorRhythm:::mod_file_upload_sidebar_ui("upload"))

  expect_match(html, "Upload study data", fixed = TRUE)
  expect_match(html, "Format data", fixed = TRUE)
  expect_match(html, "text/csv", fixed = TRUE)
  expect_match(html, "text/tab-separated-values", fixed = TRUE)
  expect_match(html, "text/comma-separated-values", fixed = TRUE)
  expect_match(html, ".csv", fixed = TRUE)
  expect_match(html, ".tsv", fixed = TRUE)
  expect_match(html, "Accepted files:", fixed = TRUE)
  expect_match(html, "CSV and TSV only.", fixed = TRUE)
  expect_no_match(html, "text/plain", fixed = TRUE)
  expect_no_match(html, "excel", ignore.case = TRUE)
  expect_no_match(html, ".xls", fixed = TRUE)
  expect_no_match(html, 'id="upload-upload_guidance"', fixed = TRUE)
})

test_that("file upload panel contains collapsed guidance above the data preview", {
  html <- as.character(PriorRhythm:::mod_file_upload_panel_ui("upload"))

  expect_match(html, "File requirements and workflow.", fixed = TRUE)
  expect_match(html, 'id="upload-upload_guidance"', fixed = TRUE)
  expect_match(html, 'id="upload-contents"', fixed = TRUE)
})

test_that("file upload server renders YAML-driven guidance text", {
  guidance <- PriorRhythm:::.read_upload_guidance()

  shiny::testServer(
    mod_file_upload_server,
    args = list(upload_guidance = guidance),
    {
      html <- paste(as.character(output$upload_guidance), collapse = "\n")
      expect_match(html, "Upload CSV or TSV files only.", fixed = TRUE)
      expect_match(html, "Typical target-study workflow", fixed = TRUE)
      expect_no_match(html, ".xlsx", fixed = TRUE)
    }
  )
})
