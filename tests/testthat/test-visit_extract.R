test_that("visit_extract", {
  data(visit)
  # Check height extract
  expect_equal(visit_extract(visit, 50)[, .N], 140, tolerance = 1e-6)
  expect_equal(visit_extract(visit, 50)[, mean(value)], 168.0429, tolerance = 1e-6)
  # Check weight extract
  expect_equal(visit_extract(visit, 21002)[, .N], 135)
  expect_equal(visit_extract(visit, 21002)[, mean(value)], 75.02444, tolerance = 1e-6)
  # Check field naming
  expect_length(visit_extract(visit, c("h" = 50, "w" = 21002))[, unique(field)], 2)
  expect_equal(visit_extract(visit, c("h" = 50, "w" = 21002))[, unique(field)], c("h", "w"))
})
