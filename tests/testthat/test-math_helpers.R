# test-math_helpers.R

library(testthat)
library(PriorRhythm)

test_that("sinusoidal_interval_mean returns correct scalar value", {
  # With phi = 0, the formula reduces to:
  # A * (12/pi/(t2-t1)) * (sin(pi/12 * t2) - sin(pi/12 * t1))
  result <- sinusoidal_interval_mean(A = 1, t1 = 0, t2 = 3, phi = 0)
  expected <- 1 * (12 / pi / 3) * (sin(pi / 12 * 3) - sin(pi / 12 * 0))
  expect_equal(result, expected)
})

test_that("sinusoidal_interval_mean is vectorised over A and phi", {
  A   <- c(1, 2, 3)
  phi <- c(0, pi / 4, pi / 2)
  result <- sinusoidal_interval_mean(A, t1 = 0, t2 = 6, phi = phi)
  expect_length(result, 3)
})

test_that("sinusoidal_interval_mean phi is NOT scaled by pi/12", {
  # phi should be a raw radian offset, NOT scaled by pi/12
  # These two must give different results
  r1 <- sinusoidal_interval_mean(A = 1, t1 = 0, t2 = 3, phi = pi / 4)
  # Wrong old formula (phi inside): A*(12/pi/(t2-t1))*(sin(pi/12*(t2+phi)) - sin(pi/12*(t1+phi)))
  r_wrong <- 1 * (12 / pi / 3) * (sin(pi / 12 * (3 + pi / 4)) - sin(pi / 12 * (0 + pi / 4)))
  expect_false(isTRUE(all.equal(r1, r_wrong)))
})

test_that("sinusoidal_interval_mean matches manual calculation with non-zero phi", {
  A <- 5; t1 <- 3; t2 <- 6; phi <- 1.2
  result   <- sinusoidal_interval_mean(A, t1, t2, phi)
  expected <- A * (12 / pi / (t2 - t1)) * (sin(pi / 12 * t2 + phi) - sin(pi / 12 * t1 + phi))
  expect_equal(result, expected)
})
