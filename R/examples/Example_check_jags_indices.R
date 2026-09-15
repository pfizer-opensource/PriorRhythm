library(PriorRhythm)

# Build a valid input list with gap-free 1:N indices
valid_input <- list(
  a  = c(1L, 1L, 2L, 2L),
  b  = c(1L, 2L, 1L, 2L),
  s  = c(1L, 1L, 2L, 2L),
  na = 2L,
  nb = 2L,
  ns = 2L,
  N  = 4L,
  sa = c(1L, 2L)
)

# Should return TRUE invisibly with no error
check_jags_indices(valid_input)

# Build an invalid input list — period index 'b' has a gap (2 is missing)
invalid_input <- valid_input
invalid_input$b <- c(1L, 3L, 1L, 3L)

# Demonstrates the informative error thrown when indices are not gap-free
tryCatch(
  check_jags_indices(invalid_input),
  error = function(e) message(conditionMessage(e))
)
