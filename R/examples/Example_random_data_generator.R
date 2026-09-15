library(PriorRhythm)

set.seed(42)
studies <- random_data_generator(ST_length = 5)

cat("Number of studies generated:", length(studies), "\n")
cat("Study names:", names(studies), "\n")
cat("Columns in first study:", names(studies[[1]]), "\n")
print(head(studies[[1]]))

set.seed(42)
studies2 <- random_data_generator(ST_length = 3, syn_study_strings = c("AA", "BB"))
cat("Custom prefix study names:", names(studies2), "\n")

set.seed(42)
single <- random_data_generator(ST_length = 1)
cat("Single study rows:", nrow(single[[1]]), "\n")
