library(PriorRhythm)

dat <- data.frame(
  animal = 1:5,
  time   = c("1.00 to 3.5 hr", "3.5 to 6.0 hr", "9 to 18 hr",
             "3.5to6.0 hr", "0 to 1 hr"),
  value  = c(40, 42, 38, 45, 41),
  stringsAsFactors = FALSE
)

result <- time_bins(dat = dat, time_col = "time", delimiter = "to")
print(result)

stopifnot(all(c("time1", "time2") %in% names(result)))

dat2 <- data.frame(
  animal   = 1:3,
  time_bin = c("1-3", "3-6", "6-12"),
  stringsAsFactors = FALSE
)
result2 <- time_bins(dat = dat2, time_col = "time_bin", delimiter = "-")
print(result2)
