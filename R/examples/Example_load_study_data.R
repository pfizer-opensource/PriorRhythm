library(PriorRhythm)
library(readr)

tmp_dir <- file.path(tempdir(), "example_load")
dir.create(tmp_dir, showWarnings = FALSE)

set.seed(42)
study_list <- random_data_generator(ST_length = 1)
study_dat  <- study_list[[1]]

csv_path <- file.path(tmp_dir, "study_a.csv")
tsv_path <- file.path(tmp_dir, "study_a.tsv")
readr::write_csv(study_dat, file = csv_path)
readr::write_tsv(study_dat, file = tsv_path)

result_csv <- load_study_data(csv_path)
print(result_csv)

result_tsv <- load_study_data(tsv_path, animal_string = "monkey|dog|rat")
print(result_tsv)

# import with a study configuration ------------------------------------------
# config$animal_string is only used when animal_string is not supplied, and
# config$column_map is applied after janitor::clean_names() and animal-column
# normalization.
cfg <- study_config(
  animal_string = "monkey|dog|rat",
  column_map    = list(time = c("summary_period", "Time"))
)
result_config <- load_study_data(csv_path, config = cfg)
print(result_config)

# error handling
tryCatch(
  load_study_data("non_existent_file.csv"),
  error = function(e) message(conditionMessage(e))
)

tmp_txt <- file.path(tmp_dir, "not_supported.txt")
writeLines("hello", tmp_txt)
tryCatch(
  load_study_data(tmp_txt),
  error = function(e) message(conditionMessage(e))
)

unlink(tmp_dir, recursive = TRUE)
